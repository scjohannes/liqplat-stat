#!/usr/bin/env Rscript

# Generate a tiny, entirely synthetic fixture for CI and local smoke tests.
# Nothing in this file reads data/private/ or any clinical source.  The
# fixture deliberately uses CSV for its compact report artifact so the smoke
# path does not need to fit a model or install a Stan toolchain.

synthetic_fixture <- function(root = tempfile("liqplat-synthetic-"),
                              project_root = getwd(), cleanup = FALSE) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  if (dir.exists(root)) stop("Synthetic fixture root already exists: ", root)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(cleanup)) {
    on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  }

  dirs <- file.path(root, c(
    "data/schema", "results/population", "results/os",
    "report-project/reports/sections", "report-project/R"
  ))
  for (directory in dirs) dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  schema_source <- file.path(project_root, "data", "schema")
  schema_paths <- list.files(schema_source, pattern = "\\.yml$", full.names = TRUE)
  if (length(schema_paths) == 0L) stop("No public schemas found in: ", schema_source)
  file.copy(schema_paths, file.path(root, "data", "schema"), overwrite = TRUE)

  cohort <- data.frame(
    id = c("synthetic-001", "synthetic-002"),
    randomization_date = as.Date(c("2026-01-01", "2026-01-08")),
    tx = c(1L, 0L),
    diagnosis = c("synthetic-a", "synthetic-b"),
    gender = c("not-recorded", "not-recorded"),
    age = c(54, 67),
    ecog_fstcnt = c(0L, 2L),
    albumin = c(40, 32),
    c_reactive_protein = c(4, 16),
    death_date = as.Date(c(NA, "2026-02-01")),
    follow_up_end_date = as.Date(c("2026-07-01", "2026-02-01")),
    status_death = c(0L, 1L),
    survival_time_days_unrestricted = c(181, 24),
    status_death_unrestricted = c(0L, 1L),
    potential_follow_up_days = c(232, 225),
    stringsAsFactors = FALSE
  )
  cohort_schema <- read_data_schema(file.path(root, "data", "schema", "cohort_follow_up.yml"))
  validate_data_schema(cohort, cohort_schema)
  assert_pseudonymized_columns(cohort, cohort_schema)

  cohort_csv <- file.path(root, "data", "cohort_follow_up.csv")
  utils::write.csv(cohort, cohort_csv, row.names = FALSE, na = "")

  survival <- derive_survival_endpoint(
    cohort, origin_col = "randomization_date", death_date_col = "death_date",
    follow_up_end_col = "follow_up_end_date", lock_date = as.Date("2026-09-05"),
    horizon_days = 182L
  )
  survival$mgps <- derive_mgps(survival$albumin, survival$c_reactive_protein)
  survival$q30_state <- invert_q30(c(1L, 7L))
  survival$exposure <- construct_exposure(survival, assignment_col = "tx")$exposure

  # A compact report artifact is labeled synthetic and contains no clinical
  # result.  It exists solely to exercise read_result_table().
  report_table <- data.frame(
    label = "synthetic fixture (not a clinical result)",
    status = "complete",
    value = 2L,
    stringsAsFactors = FALSE
  )
  report_table_path <- file.path(root, "results", "population", "summary.csv")
  utils::write.csv(report_table, report_table_path, row.names = FALSE)
  status_path <- file.path(root, "results", "population", "status.yml")
  writeLines(c(
    "status: complete",
    "reason: Synthetic fixture only; no clinical result is represented.",
    "artifacts:",
    "  - results/population/summary.csv"
  ), status_path, useBytes = TRUE)
  report_table_read <- read_result_table(report_table_path)
  report_state <- report_section_state("population", root = root)

  # Make a self-contained report-book project.
  report_project <- file.path(root, "report-project")
  report_source <- file.path(project_root, "reports")
  file.copy(
    file.path(report_source, c("_quarto.yml", "index.qmd")),
    file.path(report_project, "reports"),
    overwrite = TRUE
  )
  section_source <- file.path(report_source, "sections")
  section_target <- file.path(report_project, "reports", "sections")
  file.copy(list.files(section_source, full.names = TRUE), section_target,
            recursive = FALSE, overwrite = TRUE)
  file.copy(file.path(project_root, "R", "report-reader.R"),
            file.path(report_project, "R"), overwrite = TRUE)
  list(
    root = root,
    project_root = project_root,
    cohort = cohort,
    cohort_schema = cohort_schema,
    cohort_csv = cohort_csv,
    survival = survival,
    report_table = report_table_read,
    report_state = report_state,
    report_project = report_project,
    report_qmd = file.path(report_project, "reports")
  )
}
