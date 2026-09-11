# Readers for compact public analysis artifacts and report resources.
#
# Reports are read-only consumers.  Missing artifacts are represented by an
# explicit status and are never converted to zero or to a successful result.

# Report chunks check each absolute artifact path before including a figure.
# Keep knitr's relative paths for portable HTML and Typst output, but skip its
# second existence check: that check is evaluated from the report working
# directory rather than the chapter output directory on Windows.
options(
  knitr.graphics.rel_path = TRUE,
  knitr.graphics.error = FALSE
)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
}

report_project_root <- function(root = NULL) {
  if (!is.null(root)) return(normalizePath(root, winslash = "/", mustWork = FALSE))
  if (requireNamespace("here", quietly = TRUE)) {
    return(normalizePath(here::here(), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

report_path <- function(..., root = NULL) {
  pieces <- list(...)
  if (any(vapply(pieces, function(x) length(x) != 1L || is.na(x), logical(1)))) {
    stop("Report path components must be non-missing scalar strings.")
  }
  file.path(report_project_root(root), do.call(file.path, pieces))
}

read_result_table <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  extension <- tolower(tools::file_ext(path))
  out <- switch(
    extension,
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required to read Parquet artifacts.")
      }
      arrow::read_parquet(path)
    },
    rds = readRDS(path),
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    tsv = utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
    stop("Unsupported result-table format: ", extension)
  )
  if (!is.data.frame(out)) stop("Result artifact does not contain a data frame: ", path)
  out
}

read_report_resource <- function(path, encoding = "UTF-8") {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("qmd", "md", "html", "tex", "txt")) {
    return(paste(readLines(path, warn = FALSE, encoding = encoding), collapse = "\n"))
  }
  read_result_table(path)
}

report_status_value <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (value %in% c("available", "complete", "success", "succeeded", "ok")) {
    return("available")
  }
  if (value %in% c("failed", "error", "invalid")) return("failed")
  if (value %in% c("blocked", "pending")) return("blocked")
  if (value %in% c("unavailable", "missing")) return("unavailable")
  if (value %in% c("not_run", "not-run", "not run", "notstarted", "not_started")) {
    return("not_run")
  }
  "unavailable"
}

report_manifest_candidates <- function(section, root = NULL) {
  root <- report_project_root(root)
  c(
    file.path(root, "results", section, "status.yml"),
    file.path(root, "results", section, "status.yaml"),
    file.path(root, "results", section, "manifest.yml"),
    file.path(root, "results", section, "manifest.yaml"),
    file.path(root, "artifacts", section, "status.yml"),
    file.path(root, "artifacts", section, "status.yaml"),
    file.path(root, "results", "status", paste0(section, ".yml")),
    file.path(root, "results", "status", paste0(section, ".yaml"))
  )
}

read_report_manifest <- function(path, root = NULL) {
  if (!file.exists(path)) {
    return(list(
      status = "not_run",
      reason = "No status manifest was produced for this analysis.",
      artifacts = character(), manifest_path = NA_character_
    ))
  }
  extension <- tolower(tools::file_ext(path))
  manifest <- switch(
    extension,
    yml = {
      if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.")
      yaml::read_yaml(path)
    },
    yaml = {
      if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.")
      yaml::read_yaml(path)
    },
    json = {
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
      jsonlite::read_json(path, simplifyVector = TRUE)
    },
    rds = readRDS(path),
    stop("Unsupported report manifest format: ", extension)
  )
  if (!is.list(manifest)) stop("Report status manifest must be a mapping: ", path)
  status <- report_status_value(manifest$status %||% manifest$state %||% "unavailable")
  reason <- manifest$reason %||% manifest$message %||% manifest$readiness_reason
  if (is.null(reason) || length(reason) == 0L) {
    reason <- switch(
      status,
      available = "Result artifact is available.",
      failed = "Analysis failed; inspect the stage log.",
      blocked = "Analysis is blocked by an input or readiness condition.",
      not_run = "Analysis has not been run.",
      "Result artifact is unavailable."
    )
  }
  artifacts <- manifest$artifacts %||% manifest$artifact %||% manifest$files %||% character()
  if (is.list(artifacts)) artifacts <- unlist(artifacts, use.names = FALSE)
  artifacts <- as.character(artifacts)
  root <- report_project_root(root)
  artifacts <- vapply(artifacts, function(item) {
    if (grepl("^[A-Za-z]:|^[/\\\\]", item)) {
      normalizePath(item, winslash = "/", mustWork = FALSE)
    } else {
      normalizePath(file.path(root, item), winslash = "/", mustWork = FALSE)
    }
  }, character(1))
  list(
    status = status,
    reason = as.character(reason[[1L]]),
    artifacts = artifacts,
    manifest_path = normalizePath(path, winslash = "/", mustWork = TRUE),
    raw = manifest
  )
}

report_section_state <- function(section, root = NULL, artifact_paths = character()) {
  root <- report_project_root(root)
  candidates <- report_manifest_candidates(section, root)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) {
    state <- tryCatch(
      read_report_manifest(existing[[1L]], root = root),
      error = function(error) list(
        status = "failed", reason = conditionMessage(error),
        artifacts = character(), manifest_path = existing[[1L]]
      )
    )
  } else {
    state <- list(
      status = "not_run",
      reason = "No status manifest was produced for this analysis.",
      artifacts = character(), manifest_path = NA_character_
    )
  }
  declared <- c(state$artifacts %||% character(), artifact_paths)
  declared <- as.character(declared)
  declared <- vapply(declared, function(item) {
    if (grepl("^[A-Za-z]:|^[/\\\\]", item)) item else file.path(root, item)
  }, character(1))
  declared <- normalizePath(declared, winslash = "/", mustWork = FALSE)
  declared <- unique(declared)
  if (length(existing) == 0L && any(file.exists(declared))) {
    state$status <- "available"
    state$reason <- "Compact result artifact found without a status manifest."
  }
  if (identical(state$status, "available") &&
      length(declared) > 0L && !any(file.exists(declared))) {
    state$status <- "unavailable"
    state$reason <- "Status says available but the declared result artifact is missing."
  }
  state$section <- section
  state$root <- root
  state$artifacts <- declared
  state
}

report_artifact_candidates <- function(section) {
  mapping <- list(
    population = c(
      "results/population/summary.parquet", "results/population/summary.csv",
      "results/population/table.parquet", "results/population/table.csv"
    ),
    os = c(
      "results/main/os/summary.parquet", "results/main/os/summary.csv",
      "results/os/summary.parquet", "results/os/summary.csv",
      "results/os/rmst_summary.parquet"
    ),
    qol = c(
      "results/primary/quality-of-life/summary.parquet",
      "results/primary/quality-of-life/estimand-draws.parquet",
      "results/main/qol/summary.parquet", "results/main/qol/summary.csv",
      "results/qol/summary.parquet", "results/qol/summary.csv"
    ),
    taooh = c(
      "results/primary/taooh/home-time-summary.parquet",
      "results/primary/taooh/state-occupancy-summary.parquet",
      "results/main/taooh/summary.parquet", "results/main/taooh/summary.csv",
      "results/taooh/summary.parquet", "results/taooh/summary.csv"
    ),
    supporting = c("results/supporting/summary.parquet", "results/supporting/summary.csv"),
    implementation = c("results/implementation/summary.parquet", "results/implementation/summary.csv"),
    secondary = c("results/secondary/summary.parquet", "results/secondary/summary.csv"),
    pfs = c("results/secondary/pfs/summary.parquet", "results/secondary/pfs/summary.csv"),
    survival_rates = c("results/secondary/survival-rates/summary.parquet", "results/secondary/survival-rates/summary.csv"),
    bsc = c("results/secondary/bsc/summary.parquet", "results/secondary/bsc/summary.csv"),
    blood_products = c("results/secondary/blood-products/summary.parquet", "results/secondary/blood-products/summary.csv"),
    tissue_biopsy = c("results/secondary/tissue-biopsy/summary.parquet", "results/secondary/tissue-biopsy/summary.csv"),
    imaging = c("results/secondary/imaging/summary.parquet", "results/secondary/imaging/summary.csv")
  )
  mapping[[section]] %||% character()
}

report_table_artifacts <- function(state) {
  artifacts <- state$artifacts %||% character()
  artifacts[file.exists(artifacts) &
              tolower(tools::file_ext(artifacts)) %in% c("parquet", "rds", "csv", "tsv")]
}

report_figure_artifacts <- function(state) {
  artifacts <- state$artifacts %||% character()
  artifacts[file.exists(artifacts) &
              tolower(tools::file_ext(artifacts)) %in% c("png", "jpg", "jpeg", "svg", "pdf")]
}

report_render_status <- function(state) {
  if (!is.list(state)) stop("Report state must be a list.")
  label <- switch(
    state$status,
    available = "available",
    failed = "failed",
    blocked = "blocked",
    not_run = "not run",
    "unavailable"
  )
  cat("**Status:** ", label, ". ", state$reason, "\n\n", sep = "")
}

report_render_tables <- function(state, max_tables = 3L) {
  paths <- head(report_table_artifacts(state), max_tables)
  if (length(paths) == 0L) return(invisible(FALSE))
  for (path in paths) {
    table <- tryCatch(read_result_table(path), error = function(error) NULL)
    if (is.null(table)) next
    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(table, format = "pipe"))
    } else {
      print(table)
    }
    cat("\n")
  }
  invisible(TRUE)
}

report_render_figures <- function(state) {
  paths <- report_figure_artifacts(state)
  if (length(paths) == 0L) return(invisible(FALSE))
  if (!requireNamespace("knitr", quietly = TRUE)) return(invisible(FALSE))
  for (path in paths) {
    print(knitr::include_graphics(path))
  }
  invisible(TRUE)
}

report_render_section <- function(section, root = NULL, artifact_paths = NULL,
                                  max_tables = 3L) {
  if (is.null(artifact_paths)) artifact_paths <- report_artifact_candidates(section)
  state <- report_section_state(section, root = root, artifact_paths = artifact_paths)
  report_render_status(state)
  report_render_tables(state, max_tables = max_tables)
  report_render_figures(state)
  invisible(state)
}

read_qol_analysis_decision <- function(root = NULL) {
  root <- report_project_root(root)
  candidates <- c(
    file.path(root, "results", "primary", "overall-survival", "qol-decision.yml"),
    file.path(root, "results", "primary", "overall-survival", "qol_decision.yml"),
    file.path(root, "results", "os", "qol-decision.yml"),
    file.path(root, "results", "os", "qol_decision.yml"),
    file.path(root, "results", "main", "os", "qol-decision.yml"),
    file.path(root, "results", "main", "os", "qol_decision.yml"),
    file.path(root, "results", "qol-decision.yml"),
    file.path(root, "results", "qol_decision.yml")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    return(list(
      status = "unavailable",
      reason = "The OS-to-QoL decision artifact is not available; no QoL primary is selected.",
      primary = NA_character_, supporting = NA_character_, artifact = NA_character_
    ))
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(list(status = "failed", reason = "Package 'yaml' is required to read the QoL decision.",
                primary = NA_character_, supporting = NA_character_, artifact = existing[[1L]]))
  }
  decision <- tryCatch(yaml::read_yaml(existing[[1L]]), error = function(error) NULL)
  if (!is.list(decision)) {
    return(list(status = "failed", reason = "QoL decision artifact is invalid.",
                primary = NA_character_, supporting = NA_character_, artifact = existing[[1L]]))
  }
  primary <- tolower(as.character(decision$primary_analysis %||% decision$primary %||% ""))
  if (!primary %in% c("principal_stratum", "death_inclusive_ordinal")) {
    return(list(status = "failed", reason = "QoL decision artifact has no approved primary analysis.",
                primary = NA_character_, supporting = NA_character_, artifact = existing[[1L]]))
  }
  supporting <- decision$supporting_analysis %||% if (primary == "principal_stratum") {
    "death-inclusive longitudinal ordinal QoL"
  } else {
    "death-inclusive longitudinal ordinal QoL"
  }
  list(
    status = "available",
    reason = as.character(decision$reason %||% "Read from the OS decision artifact."),
    primary = primary,
    supporting = as.character(supporting[[1L]]),
    artifact = normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE),
    raw = decision
  )
}

qol_primary_label <- function(decision) {
  if (!is.list(decision) || !identical(decision$status, "available")) {
    return("QoL primary analysis not selected (OS decision artifact unavailable).")
  }
  if (identical(decision$primary, "principal_stratum")) {
    return("Principal-stratum QoL is the primary analysis; death-inclusive longitudinal QoL is supporting.")
  }
  "Six-month death-inclusive ordinal QoL is the primary analysis; principal-stratum and death-inclusive longitudinal QoL analyses are supporting."
}

list_public_report_outputs <- function(root = report_project_root("reports")) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) return(character())
  files <- list.files(root, recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/(private|raw|derived|logs|cache)/",
                        gsub("\\\\", "/", files))]
  normalizePath(files, winslash = "/", mustWork = TRUE)
}
