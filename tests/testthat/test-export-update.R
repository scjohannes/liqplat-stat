test_that("unrestricted TAOOH retains partial terminal weeks and death at week 50", {
  cohort <- data.frame(
    id = c("death", "censored"),
    survival_time_days_unrestricted = c(345, 349),
    status_death_unrestricted = c(1L, 0L)
  )
  taooh <- tidyr::expand_grid(id = cohort$id, week = -1L:50L) |>
    dplyr::mutate(y_taooh = ifelse(id == "death" & week == 50L, 5L, 1L))
  expect_silent(validate_taooh_follow_up(taooh, cohort))
  expect_error(validate_taooh_follow_up(
    dplyr::bind_rows(taooh, data.frame(id = "censored", week = 51L, y_taooh = 1L)),
    cohort
  ), "after")
  expect_error(validate_taooh_follow_up(
    dplyr::filter(taooh, !(id == "death" & week == 50L)), cohort
  ), "death week")
  expect_error(validate_taooh_follow_up(
    dplyr::filter(taooh, !(id == "censored" & week == 50L)), cohort
  ), "final")
  empirical <- taooh_empirical_history(taooh, 364)
  expect_equal(empirical$week[empirical$id == "death" & empirical$y_taooh == 5L], 50:52)
  expect_equal(max(empirical$week[empirical$id == "censored"]), 50L)
  expect_equal(max(taooh_empirical_history(taooh, 182)$week), 26L)
  expect_equal(max(taooh$week), 50L)
  expect_equal(taooh_horizon_weeks(728), 104L)
  expect_error(taooh_horizon_weeks(183), "whole number of weeks")
  expect_false(identical(taooh_path("artifacts", 182), taooh_path("artifacts", 364)))
})

test_that("death on day zero and exact week boundaries use the terminal week", {
  cohort <- data.frame(id = c("zero", "seven", "eight"),
                       survival_time_days_unrestricted = c(0, 7, 8),
                       status_death_unrestricted = c(1L, 1L, 1L))
  taooh <- data.frame(id = cohort$id, week = c(1L, 1L, 2L), y_taooh = 5L)
  expect_silent(validate_taooh_follow_up(taooh, cohort))
  expect_equal(sum(taooh_empirical_history(taooh, 364)$week == 52L), 3L)
})

test_that("recruitment allows identical rows and missing consent but exactly three columns", {
  schema <- read_data_schema(liqplat_path("data", "schema", "recruitment.yml"))
  recruitment <- data.frame(
    informed_consent_date = as.Date(c(NA, NA)),
    randomization_date = as.Date(c("2026-01-01", "2026-01-01")),
    group_assignment = c(0L, 0L)
  )
  expect_silent(validate_data_schema(recruitment, schema))
  expect_error(validate_data_schema(transform(recruitment, id = c("a", "b")), schema), "exactly")
  expect_error(validate_data_schema(recruitment[-1], schema), "missing")
})

test_that("blood-product counts preserve missing values and validate components", {
  counts <- data.frame(n_blood_products = c(3, NA),
                       n_erythrocyte_concentrates = c(2, NA),
                       n_thrombocyte_concentrates = c(1, 0))
  expect_silent(validate_blood_product_counts(counts))
  expect_true(is.na(counts$n_blood_products[2]))
  expect_error(validate_blood_product_counts(counts[-1]), "missing")
  expect_error(validate_blood_product_counts(transform(counts, n_blood_products = c(4, NA))), "sum")
  expect_error(validate_blood_product_counts(transform(counts, n_erythrocyte_concentrates = -1)), "nonnegative")
})
