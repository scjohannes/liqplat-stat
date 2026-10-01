testthat::test_that("V4 CONSORT branches reconcile and distinguish invitation outcomes", {
  x <- data.frame(screened = c(0, 1, 1, 1, 1, 1, 1, 1, 1),
    not_screened_reason = c("Competing trial", rep(NA_character_, 8)),
    exclusion_reason = c(NA, "No solid tumor", rep(NA_character_, 7)),
    qualified_for_random_selection = c(0, 0, rep(1, 7)),
    group_assignment = c(NA, NA, NA, "SOC", rep("ctDNA + SOC", 5)),
    invitation_status = c(rep(NA_character_, 4),
      "Approached and accepted intervention", "Approached and declined intervention",
      "Not approached", "Approached with decision missing", NA),
    accepted_invitation = c(rep(NA_real_, 4), 1, 0, NA, NA, NA))
  a <- consort_accounting(x)
  schema <- read_data_schema(liqplat_path("data", "schema", "consort_ipd.yml"))
  testthat::expect_silent(validate_data_schema(rbind(x, x), schema))
  testthat::expect_silent(assert_pseudonymized_columns(x, schema))
  testthat::expect_error(validate_data_schema(x[-1], schema), "missing")
  testthat::expect_error(validate_data_schema(transform(x, extra = 1), schema), "exactly")
  testthat::expect_error(validate_data_schema(transform(x, screened = NA_real_), schema), "missing")
  n <- setNames(a$n, a$step)
  testthat::expect_equal(n[["candidates"]], n[["not_screened"]] + n[["assessed"]])
  testthat::expect_equal(n[["assessed"]], n[["excluded"]] + n[["eligible"]])
  testthat::expect_equal(n[["eligible"]], n[["unallocated"]] + n[["randomized"]])
  testthat::expect_equal(n[["randomized"]], n[["selected"]] + n[["usual_care"]])
  testthat::expect_equal(n[["selected"]], n[["approached"]] + n[["not_approached"]] + n[["status_missing"]])
  testthat::expect_equal(n[["approached"]], n[["accepted"]] + n[["declined"]] + n[["decision_missing"]])
  testthat::expect_equal(n[["decision_missing"]], 1)
  testthat::expect_silent(ggplot2::ggplot_build(plot_consort(a)))
  conflict <- x; conflict$exclusion_reason[3] <- "Not eligible - reason not recorded"
  testthat::expect_equal(consort_accounting(conflict), a)
  bad <- x; bad$not_screened_reason[1] <- NA
  testthat::expect_error(consort_accounting(bad), "reasons disagree")
  bad <- x; bad$exclusion_reason[1] <- "No solid tumor"
  testthat::expect_error(consort_accounting(bad), "Clinical exclusions")
  bad <- x; bad$accepted_invitation[6] <- 1
  testthat::expect_error(consort_accounting(bad), "acceptance disagree")
  bad <- x; bad$invitation_status[4] <- "Not approached"
  testthat::expect_error(consort_accounting(bad), "SAT arm")
})
