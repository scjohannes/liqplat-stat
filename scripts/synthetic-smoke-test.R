#!/usr/bin/env Rscript

# End-to-end no-PHI smoke test.  It validates a synthetic schema/helper/
# checkpoint/report path and renders the artifact-only report to both HTML and
# Typst.  Expensive Stan models are intentionally not called.

script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- script_args[grepl("^--file=", script_args)]
script_file <- if (length(script_file_arg) == 0L) "" else
  sub("^--file=", "", script_file_arg[[1L]])
project_root <- if (nzchar(script_file)) {
  normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(project_root, "R", "analysis-config.R"), local = FALSE)
source(file.path(project_root, "R", "paths-artifacts.R"), local = FALSE)
source(file.path(project_root, "R", "checkpoints.R"), local = FALSE)
source(file.path(project_root, "R", "data-helpers.R"), local = FALSE)
source(file.path(project_root, "R", "data-validation.R"), local = FALSE)
source(file.path(project_root, "R", "survival-helpers.R"), local = FALSE)
source(file.path(project_root, "R", "actionability.R"), local = FALSE)
source(file.path(project_root, "R", "report-reader.R"), local = FALSE)
source(file.path(project_root, "scripts", "generate-synthetic-fixture.R"), local = FALSE)

run_synthetic_smoke <- function(project_root) {
  fixture <- synthetic_fixture(project_root = project_root)
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  if (!isTRUE(fixture$checkpoint_current)) stop("Synthetic checkpoint is not current.")
  if (!identical(fixture$report_state$status, "available")) {
    stop("Synthetic report artifact was not read as available.")
  }
  if (!identical(fixture$report_table$label,
                 "synthetic fixture (not a clinical result)")) {
    stop("Synthetic report artifact label did not round-trip.")
  }
  if (any(grepl("patient_name|medical_record|free_text|mrn",
                names(fixture$cohort), ignore.case = TRUE))) {
    stop("Synthetic fixture unexpectedly contains a PHI-like column.")
  }

  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto is required for the synthetic report smoke test.")
  html_dir <- file.path(fixture$root, "report-html")
  typst_dir <- file.path(fixture$root, "report-typst")
  dir.create(html_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(typst_dir, recursive = TRUE, showWarnings = FALSE)
  # Quarto 1.9 stores a SQLite/Deno Sass cache under LOCALAPPDATA.  Isolate it
  # to this disposable fixture so concurrent local renders or a stale user
  # cache cannot make the smoke test fail with a locked database.
  old_local_appdata <- Sys.getenv("LOCALAPPDATA", unset = "")
  local_appdata <- file.path(fixture$root, "quarto-cache")
  dir.create(local_appdata, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(LOCALAPPDATA = local_appdata)
  on.exit({
    if (nzchar(old_local_appdata)) Sys.setenv(LOCALAPPDATA = old_local_appdata)
    else Sys.unsetenv("LOCALAPPDATA")
  }, add = TRUE)
  run_render <- function(format, output_dir) {
    status <- system2(
      quarto,
      args = c("render", fixture$report_qmd, "--to", format,
               "--output-dir", output_dir),
      stdout = "", stderr = ""
    )
    if (!identical(status, 0L)) {
      stop("Quarto ", format, " report render failed with status ", status, ".")
    }
  }
  run_render("html", html_dir)
  run_render("typst", typst_dir)

  html <- list.files(html_dir, pattern = "\\.html$", recursive = TRUE,
                     full.names = TRUE)
  typst <- list.files(typst_dir, pattern = "\\.(pdf|typ)$", recursive = TRUE,
                      full.names = TRUE)
  if (length(html) == 0L) stop("Synthetic HTML report was not created.")
  if (length(typst) == 0L) stop("Synthetic Typst report was not created.")
  html_text <- paste(readLines(html[[1L]], warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
  if (!grepl("not run|not_run|synthetic", html_text, ignore.case = TRUE)) {
    stop("Synthetic HTML report did not show a labeled synthetic/not-run status.")
  }
  invisible(list(fixture = fixture, html = html, typst = typst))
}

# Rscript supplies --file=...; keeping the guard makes this file sourceable by
# a test or an interactive debugging session without rendering unexpectedly.
if (length(grep("synthetic-smoke-test\\.R$", script_file)) > 0L) {
  result <- run_synthetic_smoke(project_root)
  cat("Synthetic smoke passed. HTML:", result$html[[1L]], "\n")
  cat("Synthetic smoke passed. Typst:", result$typst[[1L]], "\n")
}
