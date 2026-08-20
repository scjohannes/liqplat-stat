test_that("the no-PHI synthetic fixture exercises schemas, helpers, checkpoints, and reports", {
  generator <- liqplat_path("scripts", "generate-synthetic-fixture.R")
  expect_true(file.exists(generator))
  sys.source(generator, envir = .GlobalEnv)
  fixture <- synthetic_fixture(project_root = .liqplat_project_root, cleanup = TRUE)
  expect_true(is.data.frame(fixture$cohort))
  expect_true(isTRUE(fixture$checkpoint_current))
  expect_identical(fixture$report_state$status, "available")
  expect_identical(fixture$report_table$status, "complete")
  expect_false(any(grepl("patient_name|medical_record|free_text|mrn",
                          names(fixture$cohort), ignore.case = TRUE)))
})
