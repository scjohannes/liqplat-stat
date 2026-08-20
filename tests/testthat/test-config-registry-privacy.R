test_that("analysis config distinguishes 50 primary and five supporting imputations", {
  config <- read_analysis_config(liqplat_path("config", "analysis.yml"))
  expect_identical(module_exact_indices(config), 1:50)
  expect_identical(module_exact_indices(config, supporting = TRUE), 1:5)
  expect_equal(config$imputation$main$m, 50)
  expect_equal(config$imputation$supporting$indices, 1:5)
  expect_error(module_exact_indices(modifyList(config, list(imputation = list(main = list(m = 5))))), "50")
})

test_that("all public schemas load and enforce the privacy boundary", {
  schema_paths <- list.files(liqplat_path("data", "schema"), "\\.yml$", full.names = TRUE)
  expect_gte(length(schema_paths), 14L)
  schemas <- lapply(schema_paths, read_data_schema)
  expect_true(all(vapply(schemas, function(x) is.list(x) && length(schema_columns(x)) > 0L, logical(1))))

  cohort_schema <- schemas[[match("cohort_follow_up.yml", basename(schema_paths))]]
  cohort <- data.frame(
    id = "synthetic-01",
    randomization_date = as.Date("2026-01-01"),
    tx = 1L,
    diagnosis = "synthetic",
    stringsAsFactors = FALSE
  )
  expect_silent(validate_data_schema(cohort, cohort_schema))
  expect_error(assert_no_phi_columns(transform(cohort, patient_name = "not allowed")), "PHI")
  expect_error(assert_pseudonymized_columns(transform(cohort, patient_name = "not allowed"), cohort_schema), "PHI")
})

test_that("the complete public YAML contract loads", {
  yaml_paths <- c(
    list.files(liqplat_path("config"), "\\.yml$", full.names = TRUE),
    list.files(liqplat_path("data", "schema"), "\\.yml$", full.names = TRUE)
  )
  expect_gte(length(yaml_paths), 16L)
  loaded <- lapply(yaml_paths, yaml::read_yaml)
  expect_true(all(vapply(loaded, is.list, logical(1))))
})

test_that("the stage registry is ordered, unique, and points to existing QMDs", {
  registry <- yaml::read_yaml(liqplat_path("config", "stages.yml"))
  stages <- registry$stages
  expect_true(is.list(stages) && length(stages) >= 16L)
  ids <- vapply(stages, function(stage) as.character(stage$id), character(1))
  numbers <- vapply(stages, function(stage) as.integer(stage$number), integer(1))
  paths <- vapply(stages, function(stage) as.character(stage$path), character(1))
  expect_identical(anyDuplicated(ids), 0L)
  expect_identical(anyDuplicated(numbers), 0L)
  expect_identical(anyDuplicated(paths), 0L)
  expect_identical(numbers, sort(numbers))
  expect_true(all(grepl("\\.qmd$", paths, ignore.case = TRUE)))
  expect_true(all(file.exists(liqplat_path(paths))))
  expect_true(any(ids %in% c("sap", "SAP")))
})
