testthat::test_that("CONSORT schema accepts anonymous candidates and rejects invalid columns", {
  x <- data.frame(
    screened = c(1L, 1L),
    not_screened_reason = NA_character_,
    exclusion_reason = NA_character_,
    qualified_for_random_selection = 1L,
    group_assignment = "SOC",
    invitation_status = NA_character_,
    accepted_invitation = NA_integer_
  )
  schema <- read_data_schema(liqplat_path("data", "schema", "consort_ipd.yml"))
  testthat::expect_silent(validate_data_schema(rbind(x, x), schema))
  testthat::expect_silent(assert_pseudonymized_columns(x, schema))
  testthat::expect_error(validate_data_schema(x[-1], schema), "missing")
  testthat::expect_error(validate_data_schema(transform(x, extra = 1), schema), "exactly")
  testthat::expect_error(validate_data_schema(transform(x, screened = NA_real_), schema), "missing")
})
