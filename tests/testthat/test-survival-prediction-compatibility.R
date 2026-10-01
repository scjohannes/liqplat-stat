source(here::here("R", "survival-helpers.R"))

testthat::test_that("hierarchical survival prediction fixes frame metadata without changing predictions", {
  model_path <- here::here("artifacts", "secondary", "pfs", "models", "diagnosis-01.rds")
  testthat::skip_if_not(file.exists(model_path), "Requires the fitted PFS analysis artifact")
  model <- readRDS(model_path)
  data <- model$data
  original_body <- body(getFromNamespace(".pp_data_surv", "rstanarm"))
  testthat::expect_true(is.factor(data$diagnosis) && is.factor(data$mgps))
  for (standardise in c(FALSE, TRUE)) {
    args <- list(object = model, newdata = data, times = 182, extrapolate = FALSE,
      standardise = standardise, draws = 20L, seed = 123L, return_matrix = TRUE)
    observed_warnings <- character()
    actual <- withCallingHandlers(do.call(posterior_survfit_compatible, args), warning = function(w) {
      observed_warnings <<- c(observed_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    testthat::expect_false(any(grepl("not a factor|mkReTrms", observed_warnings)))
    expected <- suppressWarnings(do.call(rstanarm::posterior_survfit, args))
    testthat::expect_identical(actual, expected)
  }
  testthat::expect_identical(body(getFromNamespace(".pp_data_surv", "rstanarm")), original_body)
  testthat::expect_identical(environment(getFromNamespace(".pp_data_surv", "rstanarm")), asNamespace("rstanarm"))
  data$mgps <- factor(rep("unknown", nrow(data)))
  testthat::expect_error(posterior_survfit_compatible(model, newdata = data,
    times = 182, extrapolate = FALSE, draws = 5L), "new level")
})
