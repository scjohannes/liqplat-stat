testthat::test_that("baseline QL2 reverses export coding and follows EORTC missing-item scoring", {
  testthat::expect_equal(baseline_ql2(c(1, 7, 4, 2, NA, NA),
                                    c(1, 7, 4, 6, 1, NA)),
                         c(100, 0, 50, 50, 100, NA))
  testthat::expect_error(baseline_ql2(8, 1), "integers")
  testthat::expect_error(baseline_ql2(1.5, 1), "integers")
  testthat::expect_error(baseline_ql2(1:2, 1), "lengths")
  testthat::expect_equal(snomed_to_diagnosis("SNM_781382000"), "Colorectal cancer")
})

testthat::test_that("baseline fallback preserves dedicated items and uses the primary 14-day window", {
  cohort <- data.frame(id = c("a", "b", "c"), visit_date = as.Date("2026-01-20"),
    baseline_q29 = c(2, NA, NA), baseline_q30 = c(NA, NA, NA))
  qol <- data.frame(id = c("a", "a", "a", "b", "c"),
    assessment_id = c("z", "b", "a", "d", "e"),
    questionnaire_date = as.Date("2026-01-20") + c(2, -2, -2, 14, 15),
    q29 = c(7, 6, 5, 3, 2), q30 = c(7, 6, 5, 3, 2))
  result <- complete_table_baseline_qol(cohort, qol)
  testthat::expect_equal(result$baseline_q29, c(2, 3, NA))
  testthat::expect_equal(result$baseline_q30, c(5, 3, NA))
  qol$q30[3] <- NA
  testthat::expect_equal(complete_table_baseline_qol(cohort, qol)$baseline_q30, c(6, 3, NA))
})
