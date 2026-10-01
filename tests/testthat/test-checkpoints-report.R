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

test_that("report table headers are reader-facing", {
  expect_identical(
    report_table_labels(c(
      "classification", "mutation_records", "percent_of_valid_mutations",
      "ctdna_samples", "n_eff", "Rhat", "q025"
    )),
    c(
      "Classification", "Mutation records, n", "Valid ctDNA mutations, %",
      "ctDNA samples", "Effective sample size", "R-hat", "2.5% quantile"
    )
  )
})
