test_that("fixed-lock censoring uses the lock and horizon as administrative bounds", {
  data <- data.frame(
    id = c("p01", "p02", "p03", "p04"),
    origin = as.Date("2026-01-01") + c(0, 0, 0, 0),
    event = as.Date(c("2026-05-01", "2026-07-10", NA, "2026-03-01")),
    follow_up_end = as.Date(c(NA, NA, NA, "2026-02-15")),
    stringsAsFactors = FALSE
  )
  out <- censor_at_lock(
    data, origin_col = "origin", event_date_col = "event",
    follow_up_end_col = "follow_up_end", lock_date = as.Date("2026-08-21"),
    horizon_days = 182, time_col = "time", event_col = "status"
  )

  expect_equal(out$status, c(1L, 0L, 0L, 0L))
  expect_equal(out$time, c(120, 182, 182, 45))
  expect_equal(out$censor_date, as.Date(c("2026-07-02", "2026-07-02", "2026-07-02", "2026-02-15")))
  expect_error(
    censor_at_lock(
      data.frame(origin = as.Date("2026-01-02"), event = as.Date("2026-01-01")),
      "origin", "event"
    ),
    "precedes"
  )
})

test_that("Q30 inversion preserves missingness and rejects out-of-range values", {
  expect_equal(invert_q30(1:7), 7:1)
  expect_equal(q30_to_analysis_state(c(1L, 7L, NA_integer_)), c(7L, 1L, NA_integer_))
  expect_error(invert_q30(c(1, 8)), "range")
  expect_equal(invert_q30(c(1, 8), strict = FALSE), c(7, NA_real_))
})

test_that("missing values are not silently converted to zero", {
  expect_equal(missing_zero_status(c(NA, 0, 2)), c("missing", "zero", "nonzero"))
  expect_error(assert_missing_not_zero(c(1, NA)), "missing")
  expect_error(coalesce_missing_zero(c(1, NA), field_name = "events"), "events")
  expect_equal(coalesce_missing_zero(c(1, NA), missing_is_zero = TRUE), c(1, 0))
})

test_that("count models require an observed positive exposure", {
  data <- data.frame(events = c(0L, 2L), exposure = c(1, 3))
  expect_equal(
    validate_count_inputs(data, events ~ offset(log(exposure))),
    list(response = "events", exposure_col = "exposure")
  )
  expect_error(
    validate_count_inputs(data.frame(events = c(NA, 1), exposure = c(1, 2)),
                          events ~ offset(log(exposure))),
    "missing"
  )
  expect_error(
    validate_count_inputs(data.frame(events = c(0, 1), exposure = c(0, 2)),
                          events ~ offset(log(exposure))),
    "positive"
  )
  expect_error(
    secondary_count_ready(data.frame(events = c(1, 2), days = c(1, NA)), "events", "days"),
    "positive"
  )
})

test_that("equal-weight pooling retains the same number of draws per imputation", {
  draws <- list(
    data.frame(estimate = c(1, 2, 3)),
    data.frame(estimate = c(10, 20, 30, 40))
  )
  pooled <- pool_equal_weight_draws(draws, n_draws = 2, seed = 17002)
  expect_equal(nrow(pooled), 4L)
  expect_equal(unname(as.integer(table(pooled$.imputation))), c(2L, 2L))
  expect_equal(pooled, pool_equal_weight_draws(draws, n_draws = 2, seed = 17002))
  expect_equal(one_imputation_seed(17002, 1), 17002L)
  expect_equal(one_imputation_seed(17002, 2), 18002L)
})
