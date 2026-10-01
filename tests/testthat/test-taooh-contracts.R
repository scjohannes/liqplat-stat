test_that("TAOOH preparation uses the prespecified patient-level categories", {
  preparation <- liqplat_qmd_text(
    "analysis/02-primary/03-taooh/01-preparation.qmd"
  )

  expect_match(preparation, "ecog_fstcnt == 4L")
  expect_match(preparation, "fct_lump_n")
  expect_match(preparation, "n = 4")
  expect_match(preparation, 'other_level = "Other"')
  expect_match(preparation, 'ties.method = "first"')
})

test_that("TAOOH imputation is patient-level and leaves weekly states observed", {
  imputation <- liqplat_qmd_text(
    "analysis/02-primary/03-taooh/02-imputation.qmd"
  )

  expect_match(imputation, "prop_planned_admission")
  expect_match(imputation, "prop_er")
  expect_match(imputation, "prop_unplanned_admission")
  expect_match(imputation, "event_death")
  expect_match(imputation, "na_est")
  expect_match(imputation, 'methods\\["ecog_fstcnt"\\] <- "polr"')
  expect_false(grepl('methods\\["y_taooh"\\]', imputation))
  expect_false(grepl("2l\\.pmm|2lonly\\.pmm", imputation))
})

test_that("TAOOH history retains entry into death but removes later rows", {
  observed <- data.frame(
    id = rep("p01", 6L),
    week = -1:4,
    y = c(1L, 1L, 2L, 5L, 5L, 5L)
  )

  observed$yprev <- dplyr::lag(observed$y, 1L)
  observed$ypprev <- dplyr::lag(observed$y, 2L)
  analysis <- observed |>
    dplyr::filter(
      week >= 1L,
      !is.na(yprev),
      !is.na(ypprev),
      yprev != 5L,
      ypprev != 5L
    )

  expect_equal(analysis$y, c(2L, 5L))
  expect_equal(analysis$yprev, c(1L, 2L))
  expect_equal(analysis$ypprev, c(1L, 1L))
})

test_that("TAOOH first- and second-order fits are comparable full-PO models", {
  second_order <- liqplat_qmd_text(
    "analysis/02-primary/03-taooh/03-fitting.qmd"
  )
  first_order <- liqplat_qmd_text(
    "analysis/03-supporting/05-taooh-first-order/01-fitting.qmd"
  )

  expect_match(second_order, "seq_len\\(config\\$imputation\\$main\\$m\\)")
  expect_match(first_order, "seq_len\\(config\\$imputation\\$main\\$m\\)")
  expect_false(grepl("(?<!p)ppo\\s*=|cppo\\s*=", second_order, perl = TRUE))
  expect_false(grepl("(?<!p)ppo\\s*=|cppo\\s*=", first_order, perl = TRUE))
  expect_match(second_order, "fit\\$pppo == 0L")
  expect_match(first_order, "fit\\$pppo == 0L")
  expect_match(second_order, "week %ia% yprev")
  expect_match(first_order, "week %ia% yprev")
  expect_match(second_order, "ypprev")
  expect_false(grepl("ypprev", sub(
    ".*model_formula <-", "", first_order
  )))
})

test_that("TAOOH Markov diagnostics use rmsb Stan diagnostics", {
  diagnostic_files <- c(
    "analysis/02-primary/03-taooh/05-diagnostics.qmd",
    "analysis/03-supporting/05-taooh-first-order/03-diagnostics.qmd"
  )

  for (path in diagnostic_files) {
    diagnostics <- liqplat_qmd_text(path)
    expect_match(diagnostics, "rmsb::stanDx\\(model\\)")
    expect_match(diagnostics, "rmsb::stanDxplot\\(model\\)")
    expect_false(grepl("bayesplot::mcmc_trace", diagnostics))
  }
})

test_that("TAOOH Markov-order comparison uses conditional PSIS-LOO", {
  comparison <- liqplat_qmd_text(
    "analysis/03-supporting/05-taooh-first-order/04-model-comparison.qmd"
  )

  expect_match(comparison, "rmsb::compareBmods")
  expect_match(comparison, "loo::loo_compare")
  expect_match(comparison, "second_order\\$loo <- second_order_loo")
  expect_match(comparison, "first_order\\$loo <- first_order_loo")
  expect_match(comparison, "ordinary observation-level PSIS-LOO")
  expect_match(comparison, "integrating those random effects out")
  expect_false(grepl("group_by\\(id\\).*log_lik", comparison))
})
