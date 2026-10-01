source(here::here("R", "survival-helpers.R"))

testthat::test_that("survival risks retain imputation identity and require horizon coverage", {
  curves <- expand.grid(imputation = 1:2, draw = 1:2, tx = 0:1, time = c(0, 182, 365))
  curves$survival <- 1 - curves$time / 1000 * (curves$imputation + curves$draw / 10 - curves$tx / 2)
  for (horizon in c(182, 200, 365)) {
    risks <- survival_risk_draws(curves, horizon)
    testthat::expect_equal(nrow(risks), 8L)
    testthat::expect_equal(risks$survival,
      1 - horizon / 1000 * (risks$imputation + risks$draw / 10 - risks$tx / 2))
    paired <- tidyr::pivot_wider(risks, names_from = tx, values_from = survival,
      names_prefix = "survival_")
    testthat::expect_equal(paired$survival_1 - paired$survival_0, rep(horizon / 2000, 4))
  }
  testthat::expect_error(survival_risk_draws(subset(curves, time <= 182), 365), "cover")
  testthat::expect_error(survival_risk_draws(rbind(curves, curves[1, ])), "Duplicate")
  single <- survival_risk_draws(subset(curves, imputation == 1, select = -imputation))
  testthat::expect_equal(nrow(single), 4L)
})
