test_that("PFS is first progression or death and respects shifted follow-up limits", {
  origin <- as.Date("2026-01-01")
  cohort <- data.frame(
    id = c("progression", "death", "censor", "tie", "late", "extend", "prior", "zero"),
    tx = rep(0:1, 4), randomization_date = origin,
    progression_date = origin + c(20, 60, NA, 40, 101, 50, -1, 0),
    survival_time_days_unrestricted = c(40, 40, 100, 40, 120, 30, 100, 100),
    status_death_unrestricted = c(1, 1, 0, 1, 0, 0, 0, 0),
    potential_follow_up_days = c(40, 40, 100, 40, 100, 100, 100, 100)
  )
  result <- derive_pfs_endpoint(cohort)
  expect_equal(result$event_pfs, c(1, 1, 0, 1, 0, 1, 1, 1))
  expect_equal(result$pfs_time_days, c(20, 40, 100, 40, 100, 50, -1, 0))
  expect_equal(result$pfs_exclusion_reason[7:8],
               c("progression_before_randomization", "no_positive_follow_up"))
  expect_equal(which(result$progression_extends_recorded_follow_up), 6L)
  shifted <- cohort
  shifted$randomization_date <- shifted$randomization_date + 1000
  shifted$progression_date <- shifted$progression_date + 1000
  invariant <- c("event_pfs", "pfs_time_days", "pfs_in_analysis")
  expect_equal(derive_pfs_endpoint(shifted)[invariant], result[invariant])
  expect_error(derive_pfs_endpoint(transform(cohort, progression = 1)), "no progression date")
})
