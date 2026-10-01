# Readers for compact public analysis artifacts and report resources.
here::i_am("R/report-reader.R")
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

report_table_labels <- function(labels) {
  replacements <- c(
    tx = "Randomized group",
    n = "Patients, n",
    n_participants = "Participants, n",
    n_censoring_events = "Censoring events, n",
    mutation_records = "Mutation records, n",
    percent_of_valid_mutations = "Valid ctDNA mutations, %",
    unique_variants = "Unique variants, n",
    assumption = "ctDNA-only assumption",
    chip_mutations = "CHIP mutation records, n (%)",
    assumption_eligible_mutation_records = "Eligible mutation records, n",
    patients_with_chip_mutation = "Patients with a CHIP mutation, n (%)",
    valid_ctdna_patients = "Patients with valid ctDNA, n",
    q025 = "2.5% quantile",
    q50 = "Median",
    q975 = "97.5% quantile",
    `q 2.5` = "2.5% quantile",
    q97.5 = "97.5% quantile",
    probability_benefit = "P(benefit)",
    probability_superiority = "P(superiority)",
    probability_hr_below_one = "P(HR < 1)",
    p_value = "p-value",
    posterior_draws_per_imputation = "Posterior draws per imputation",
    draws_per_imputation = "Draws per imputation",
    computationally_reduced = "Reduced posterior draws",
    horizon_days = "Horizon (days)",
    horizon_weeks = "Horizon (weeks)",
    deaths_by_day_182 = "Deaths by day 182",
    baseline_qol_observed = "Baseline QoL observed",
    baseline_qol_missing = "Baseline QoL missing",
    landmark_qol_observed = "Landmark QoL observed",
    landmark_qol_missing = "Landmark QoL missing",
    composite_observed = "Composite observed",
    composite_missing = "Composite missing",
    progression_first = "Progression before death, n",
    death_before_progression = "Death before progression, n",
    pfs_events = "PFS events, n",
    pfs_exclusion_reason = "PFS exclusion reason",
    censored = "Censored, n",
    chisq = "Chi-square statistic",
    df = "Degrees of freedom",
    ci_66_low = "66% CrI lower",
    ci_66_high = "66% CrI upper",
    ci_95_low = "95% CrI lower",
    ci_95_high = "95% CrI upper",
    lower_95 = "95% CI lower",
    upper_95 = "95% CI upper",
    conf_low = "95% CI lower",
    conf_high = "95% CI upper",
    odds_ratio_low = "Odds ratio lower bound",
    odds_ratio_high = "Odds ratio upper bound",
    odds_ratio = "Odds ratio",
    log_odds_ratio = "Log odds ratio",
    hazard_ratio = "Hazard ratio",
    posterior_mean = "Posterior mean",
    posterior_median = "Posterior median",
    probability_benefit = "P(benefit)",
    probability_superiority = "P(superiority)",
    n_eff = "Effective sample size",
    Rhat = "R-hat",
    maxit = "MICE iterations",
    p = "p-value"
  )
  labels <- as.character(labels)
  vapply(labels, function(label) {
    if (label %in% names(replacements)) return(unname(replacements[[label]]))
    if (!grepl("_", label, fixed = TRUE) && !grepl("^[a-z]", label)) {
      return(label)
    }
    label <- gsub("_+", " ", label)
    label <- tolower(label)
    label <- paste0(toupper(substr(label, 1L, 1L)), substr(label, 2L, nchar(label)))
    for (term in c("ctdna", "qol", "taooh", "pfs", "mtb", "os", "id", "elpd")) {
      label <- gsub(paste0("\\b", term, "\\b"),
                    switch(term, ctdna = "ctDNA", qol = "QoL", taooh = "TAOOH",
                           pfs = "PFS", mtb = "MTB", os = "OS", id = "ID",
                           elpd = "ELPD"),
                    label, ignore.case = TRUE)
    }
    label
  }, character(1), USE.NAMES = FALSE)
}

report_kable <- function(x, ..., col.names = NULL) {
  if (is.null(col.names) && !is.null(colnames(x))) {
    col.names <- report_table_labels(colnames(x))
  }
  knitr::kable(x, ..., col.names = col.names)
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
              !grepl("draws", basename(artifacts), ignore.case = TRUE) &
              tolower(tools::file_ext(artifacts)) %in% c("parquet", "rds", "csv", "tsv")]
}

# Keep book images inside the book root for both HTML and Typst.
report_include_graphics <- function(path, ...) {
  stopifnot(length(path) > 0L, all(file.exists(path)))
  directory <- here::here("reports", "_figures")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  relative <- substring(normalizePath(path, winslash = "/"),
                        nchar(normalizePath(here::here(), winslash = "/")) + 2L)
  destination <- file.path(directory, gsub("[/\\\\:]", "-", relative))
  same <- normalizePath(path, winslash = "/", mustWork = TRUE) ==
    normalizePath(destination, winslash = "/", mustWork = FALSE)
  if (any(!same)) stopifnot(all(file.copy(path[!same], destination[!same], overwrite = TRUE)))
  knitr::include_graphics(destination, ...)
}

report_run_details <- function(analysis, imputation_source) {
  models <- read.csv(here::here("reports", "_data", "model-runs.csv"))
  imputations <- read.csv(here::here("reports", "_data", "imputation-runs.csv"))
  models <- models[models$analysis == analysis, ]
  imputation <- imputations[imputations$source == imputation_source, ]
  stopifnot(nrow(imputation) == 1L)
  values <- function(x) paste(sort(unique(x)), collapse = ", ")
  settings <- data.frame(
    `Imputations created` = imputation$completed,
    `Imputations fitted` = nrow(models),
    `MICE maxit` = imputation$maxit,
    `Chains per fit` = values(models$chains),
    `Iterations per chain` = values(models$iterations),
    `Retained per chain` = values(models$retained),
    check.names = FALSE
  )
  data.frame(Setting = names(settings), Value = unlist(settings, use.names = FALSE))
}

report_analysis_label <- function(x) {
  x <- sub("^(primary|supporting|secondary)/", "", x)
  x <- gsub("overall-survival", "OS", x, fixed = TRUE)
  x <- gsub("quality-of-life", "QoL", x, fixed = TRUE)
  x <- gsub("os_adjusted", "fixed adjustment", x, fixed = TRUE)
  x <- gsub("[\\/_-]", " ", x)
  x <- gsub("\\bqol\\b", "QoL", x)
  x <- gsub("\\bos\\b", "OS", x)
  x <- gsub("\\bpfs\\b", "PFS", x)
  x <- gsub("\\bbsc\\b", "BSC", x)
  gsub("\\btaooh\\b", "TAOOH", x)
}

report_model_run_table <- function(scope = "") {
  runs <- read.csv(here::here("reports", "_data", "model-runs.csv"))
  runs <- runs[Reduce(`|`, lapply(scope, function(prefix) startsWith(runs$analysis, prefix))), ]
  summary <- runs |>
    dplyr::group_by(analysis) |>
    dplyr::summarise(Imputations = dplyr::n(),
      Chains = paste(sort(unique(chains)), collapse = ", "),
      Iterations = paste(sort(unique(iterations)), collapse = ", "),
      Retained = paste(sort(unique(retained)), collapse = ", "), .groups = "drop")
  imputation_sources <- c(
    "primary/overall-survival" = "primary/overall-survival/imputations.rds",
    "primary/quality-of-life" = "primary/quality-of-life/imputation-fits.rds",
    "primary/taooh" = "primary/taooh/imputations/imputation-fit.rds",
    "supporting/os-unrestricted-ph" = "supporting/os-unrestricted/imputations.rds",
    "supporting/os-unrestricted-hierarchical" = "supporting/os-unrestricted-hierarchical/imputation.rds",
    "supporting/qol-unadjusted-landmark" = "primary/quality-of-life/imputation-fits.rds",
    "supporting/qol-death-inclusive-longitudinal" = "supporting/qol-death-inclusive-longitudinal/baseline-imputations.rds",
    "supporting/taooh-first-order" = "primary/taooh/imputations/imputation-fit.rds",
    "secondary/bsc/adjusted-cause_specific" = "secondary/bsc/imputations.rds",
    "secondary/bsc/adjusted-composite" = "secondary/bsc/imputations.rds",
    "secondary/bsc-diagnosis/cause_specific" = "secondary/bsc-diagnosis/imputation.rds",
    "secondary/bsc-diagnosis/composite" = "secondary/bsc-diagnosis/imputation.rds",
    "secondary/pfs/os_adjusted" = "secondary/pfs/imputations-os_adjusted.rds",
    "secondary/pfs/diagnosis" = "secondary/pfs/imputations-diagnosis.rds"
  )
  imputations <- read.csv(here::here("reports", "_data", "imputation-runs.csv"))
  summary$maxit <- imputations$maxit[match(imputation_sources[summary$analysis], imputations$source)]
  complete_data <- grepl("^secondary/(bsc|pfs)/unadjusted", summary$analysis) |
    summary$analysis == "supporting/os-unrestricted-censoring" |
    startsWith(summary$analysis, "supporting/os-censoring-scenarios")
  summary$Imputations[complete_data] <- 0L
  summary |>
    dplyr::mutate(Analysis = report_analysis_label(analysis), .before = 1) |>
    dplyr::select(Analysis, Imputations, maxit, Chains, Iterations, Retained)
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
      print(report_kable(table, format = "pipe"))
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
    print(report_include_graphics(path))
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

read_diagnostics <- function(endpoint) {
  paths <- list.files(file.path(report_root, "artifacts", "primary", endpoint, "diagnostics"),
                      pattern = "^diagnostics-[0-9]+\\.parquet$", full.names = TRUE)
  stopifnot(length(paths) > 0)
  bind_rows(lapply(paths, function(path) {
    x <- read_result_table(path)
    if (!"imputation" %in% names(x)) x$imputation <- as.integer(sub("diagnostics-([0-9]+).*", "\\1", basename(path)))
    x
  }))
}
show_model_output <- function(endpoint) {
  diagnostics <- read_diagnostics(endpoint)
  parameters <- diagnostics |> filter(!grepl("^log_lik\\[", variable))
  write.csv(parameters, file.path(report_root, "reports", "_data", paste0(endpoint, "-model-output.csv")), row.names = FALSE)
  convergence <- diagnostics |>
    summarise(`Fits` = n_distinct(imputation),
      `Maximum R-hat` = max(rhat, na.rm = TRUE),
      `Minimum bulk ESS` = min(ess_bulk, na.rm = TRUE),
      `Minimum tail ESS` = min(ess_tail, na.rm = TRUE),
      `Fits with R-hat > 1.01` = n_distinct(imputation[is.finite(rhat) & rhat > 1.01]))
  print(report_kable(convergence, digits = 3, format = "pipe"))
  cat("\n\nParameter summaries below are from the first completed dataset, to show the fitted model's coefficients and uncertainty. The 5th and 95th percentiles form a 90% interval. Main-chapter treatment estimates combine imputations.\n\n")
  first <- parameters |>
    filter(imputation == min(imputation)) |>
    select(Parameter = variable, Median = median, `5th percentile` = q5, `95th percentile` = q95)
  print(report_kable(first, digits = 3, format = "pipe"))
  cat("\n\n[Parameter summaries and convergence statistics for every fitted imputation](../_data/", endpoint, "-model-output.csv).\n\n", sep = "")
}
