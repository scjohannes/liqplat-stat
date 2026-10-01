test_that("BSC and composite endpoints honor competing death and the export lock", {
  origin <- as.Date("2026-01-01")
  lock_days <- as.numeric(as.Date("2026-09-05") - origin)
  cohort <- data.frame(
    id = c("bsc_first", "death_first", "censored", "at_lock", "after_lock",
           "late_death", "tie", "later_bsc", "missing_start", "prior_bsc", "zero"),
    tx = rep(0:1, length.out = 11),
    randomization_date = origin,
    treatment_start_date = origin,
    bsc_date = origin + c(20, 60, NA, lock_days, lock_days + 1, NA, 40, 50, NA, -1, 0),
    survival_time_days_unrestricted = c(40, 40, 100, lock_days, 300, 300, 40, 30, 100, 100, 100),
    status_death_unrestricted = c(1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0),
    potential_follow_up_days = c(40, 40, rep(lock_days, 4), 40, rep(lock_days, 4))
  )
  # Treatment starts are deliberately missing and do not affect eligibility.
  cohort$treatment_start_date <- as.Date(NA)
  result <- derive_bsc_endpoints(cohort)
  expect_equal(result$event_bsc, c(1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1))
  expect_equal(result$event_bsc_or_death, c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1))
  expect_equal(result$time_to_bsc_days,
               c(20, 40, 100, lock_days, lock_days, lock_days, 40, 50, 100, -1, 0))
  expect_equal(result$bsc_censor_reason[2], "death_before_bsc")
  expect_equal(result$bsc_exclusion_reason[9:11],
               c("included", "bsc_before_randomization", "no_positive_follow_up"))
  expect_equal(derive_bsc_endpoints(cohort[setdiff(names(cohort), "treatment_start_date")]),
               result[setdiff(names(result), "treatment_start_date")])
  expect_equal(which(result$bsc_extends_recorded_follow_up), 8L)
  expect_true(all(result$bsc_event_or_censor_date <= result$bsc_follow_up_limit))
  expect_equal(result$bsc_date, cohort$bsc_date)

  # Privacy shifting must not change event classification or follow-up duration.
  shifted <- cohort
  for (column in c("randomization_date", "treatment_start_date", "bsc_date")) {
    shifted[[column]] <- shifted[[column]] + 1000
  }
  shifted_result <- derive_bsc_endpoints(shifted)
  invariant <- c("time_to_bsc_days", "event_bsc", "event_bsc_or_death", "bsc_in_analysis")
  expect_equal(shifted_result[invariant], result[invariant])
  expect_error(derive_bsc_endpoints(cohort, "2026-08-21"), "2026-09-05")
  expect_error(derive_bsc_endpoints(transform(cohort, tx = 2)), "coded 0/1")
  expect_error(derive_bsc_endpoints(transform(cohort, potential_follow_up_days = NA_real_)),
               "finite non-negative")
  expect_error(derive_bsc_endpoints(transform(cohort, bsc = 1)), "no BSC date")
})
