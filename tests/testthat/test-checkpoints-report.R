test_that("checkpoint manifests are content-addressed and resumable", {
  root <- tempfile("liqplat-checkpoint-")
  dir.create(root, recursive = TRUE)
  input_path <- file.path(root, "input.txt")
  output_path <- file.path(root, "output.txt")
  code_path <- file.path(root, "code.R")
  manifest_path <- file.path(root, "checkpoint.yml")
  writeLines("input", input_path)
  writeLines("output", output_path)
  writeLines("code", code_path)
  config <- list(data_lock = "2026-08-21", synthetic = TRUE)
  finalize_checkpoint(
    manifest_path, "synthetic", input_path, output_path, code_path, config,
    metadata = list(status = "complete")
  )
  expect_true(file.exists(manifest_path))
  expect_true(checkpoint_is_current(manifest_path, "synthetic", input_path,
                                    output_path, code_path, config))
  expect_equal(checkpoint_decision(manifest_path, "synthetic", input_path,
                                   output_path, code_path, config), "skip")
  expect_equal(checkpoint_decision(manifest_path, "synthetic", input_path,
                                   output_path, code_path, config, force = TRUE), "force")
  writeLines("changed", input_path)
  expect_false(checkpoint_is_current(manifest_path, "synthetic", input_path,
                                     output_path, code_path, config))
  expect_equal(checkpoint_decision(manifest_path, "synthetic", input_path,
                                   output_path, code_path, config), "resume")
})

test_that("report readers preserve status and QoL decision semantics", {
  root <- tempfile("liqplat-report-")
  dir.create(file.path(root, "results", "population"), recursive = TRUE)
  dir.create(file.path(root, "results", "os"), recursive = TRUE)
  population_csv <- file.path(root, "results", "population", "summary.csv")
  status_path <- file.path(root, "results", "population", "status.yml")
  write.csv(data.frame(label = "synthetic", value = 1), population_csv, row.names = FALSE)
  writeLines(c("status: complete", "reason: synthetic fixture", "artifacts:",
               "  - results/population/summary.csv"), status_path)
  state <- report_section_state("population", root = root)
  expect_identical(state$status, "available")
  expect_equal(read_result_table(population_csv)$label, "synthetic")
  expect_equal(report_status_value("not-run"), "not_run")
  expect_identical(report_section_state("qol", root = root)$status, "not_run")
  writeLines(c("status: complete", "primary_analysis: principal_stratum",
               "reason: synthetic decision"), file.path(root, "results", "os", "qol-decision.yml"))
  decision <- read_qol_analysis_decision(root)
  expect_identical(decision$status, "available")
  expect_identical(decision$primary, "principal_stratum")
  expect_match(qol_primary_label(decision), "primary")
  unlink(root, recursive = TRUE, force = TRUE)
})
