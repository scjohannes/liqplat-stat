test_that("mGPS and derived ECOG fields agree with their source variables", {
  data <- data.frame(
    albumin = c(40, 40, 30, 30, NA),
    c_reactive_protein = c(10, 11, 11, 10, 2),
    ecog_fstcnt = c(0, 1, 2, 4, NA)
  )
  data$mgps <- derive_mgps(data$albumin, data$c_reactive_protein)
  data$ecog_binary <- derive_ecog_binary(data$ecog_fstcnt)
  expect_equal(data$mgps, c(0L, 1L, 2L, 0L, NA_integer_))
  expect_equal(data$ecog_binary, c(0L, 0L, 1L, 1L, NA_integer_))
  expect_silent(assert_mgps_consistent(data))
  expect_silent(assert_ecog_binary_consistent(data))
  data$mgps[[2L]] <- 2L
  expect_error(assert_mgps_consistent(data), "inconsistent")
  data$mgps[[2L]] <- 1L
  data$ecog_binary[[3L]] <- 0L
  expect_error(assert_ecog_binary_consistent(data), "inconsistent")
})

test_that("OS imputation uses one configured MICE run and reports convergence", {
  notebook <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "01-overall-survival", "02-imputation.qmd"
  ))

  expect_match(notebook, "m = config\\$imputation\\$main\\$m")
  expect_match(notebook, "maxit = config\\$imputation\\$main\\$maxit")
  expect_false(grepl("m = 1L", notebook, fixed = TRUE))
  expect_false(grepl("for (index in indices)", notebook, fixed = TRUE))
  expect_match(notebook, 'methods\\["ecog_fstcnt"\\] <- "polr"')
  expect_false(grepl(
    'predictors[, c("id", "tx"',
    notebook,
    fixed = TRUE
  ))
  expect_match(notebook, "bilirubin")
  expect_match(notebook, "lactate_dehydrogenase")
  expect_match(notebook, "fig-os-imputation-traces")
  expect_match(notebook, "densityplot")
  expect_match(notebook, "stripplot")
})

test_that("OS counterfactual survival curves reuse matched posterior draws", {
  primary <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "01-overall-survival", "04-estimand.qmd"
  ))
  supporting <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "01-os-non-proportional-hazards",
    "02-estimand.qmd"
  ))
  helper <- paste(readLines(
    liqplat_path("R", "survival-helpers.R"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(primary, "seed = prediction_seed")
  expect_match(supporting, "seed = prediction_seed")
  expect_false(grepl("offset = treatment", primary, fixed = TRUE))
  expect_false(grepl("offset = treatment", supporting, fixed = TRUE))
  expect_match(helper, "seed = seed")
  expect_false(grepl("seed + (index - 1L)", helper, fixed = TRUE))
  expect_match(primary, "vapply\\(raw_curves")
  expect_match(supporting, "purrr::map_dbl\\(raw_curves")
  expect_match(helper, "vapply\\(raw")
  expect_false(grepl('attr(raw_curves[[1L]], "times")', primary, fixed = TRUE))
  expect_false(grepl('attr(raw_curves[[1L]], "times")', supporting, fixed = TRUE))
  expect_false(grepl('attr(raw[[1L]], "times")', helper, fixed = TRUE))
})

test_that("OS survival contrasts preserve matched treatment-control draws", {
  estimand <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "01-overall-survival", "04-estimand.qmd"
  ))

  expect_match(
    estimand,
    "merge\\(control, intervention, by = c\\(\"draw\", \"imputation\"\\)\\)"
  )
  expect_match(estimand, "pooled_rmst <- do.call\\(rbind, rmst_frames\\)")
  expect_match(estimand, "pooled_risks <- do.call\\(rbind, risk_frames\\)")
  expect_false(grepl(
    "pooled_rmst <- pool_equal_weight_draws",
    estimand,
    fixed = TRUE
  ))
  expect_false(grepl(
    "pooled_risks <- pool_equal_weight_draws",
    estimand,
    fixed = TRUE
  ))
})

test_that("Markov estimands preserve complete draw-level treatment contrasts", {
  notebooks <- lapply(c(
    file.path("analysis", "02-primary", "03-taooh", "04-estimand.qmd")
  ), liqplat_qmd_text)

  for (notebook in notebooks) {
    expect_match(notebook, "variables = list\\(tx = c\\(0, 1\\)\\)")
    expect_match(notebook, "pooled <- bind_rows\\(comparison_draws\\)")
    expect_false(grepl(
      "pooled <- pool_equal_weight_draws",
      notebook,
      fixed = TRUE
    ))
  }
})

test_that("QoL primary analysis follows the SAP death-inclusive fallback", {
  decision <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "02-quality-of-life", "00-analysis-decision.qmd"
  ))
  preparation <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "02-quality-of-life", "01-preparation.qmd"
  ))
  imputation <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "02-quality-of-life", "02-imputation.qmd"
  ))
  fitting <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "02-quality-of-life", "03-fitting.qmd"
  ))
  estimand <- liqplat_qmd_text(file.path(
    "analysis", "02-primary", "02-quality-of-life", "04-estimand.qmd"
  ))

  expect_match(decision, "ci_66_low >= rmst\\$rope_low")
  expect_match(decision, 'primary <- if \\(equivalent\\) "principal_stratum"')
  expect_match(preparation, "duplicated\\(valid_qol\\$id, fromLast = TRUE\\)")
  expect_match(preparation, "baseline_candidates\\$distance_days <= 14L")
  expect_match(preparation, "baseline_candidates\\$questionnaire_date")
  expect_match(preparation, "prepared\\$event_death == 1L")
  expect_match(preparation, "prepared\\$survival_time_days < config\\$analysis\\$horizon_days")
  expect_match(preparation, "common_diagnoses <- head\\(diagnosis_counts\\$diagnosis_label, 4L\\)")
  expect_match(preparation, '"diagnosis_group", "diagnosis_code"')
  expect_match(preparation, '"Other"')
  expect_false(grepl("week == 26L", preparation, fixed = TRUE))
  expect_match(imputation, "rstanarm::stan_surv")
  expect_match(imputation, "prepared\\$age_10 <- \\(prepared\\$age - mean\\(prepared\\$age\\)\\) / 10")
  expect_match(imputation, "tx \\+ age_10 \\+ gender \\+ diagnosis_group \\+ stage_binary")
  expect_match(imputation, "vital_status_diagnostics\\$Rhat")
  expect_match(imputation, "vital_status_diagnostics\\$n_eff")
  expect_match(imputation, "condition = TRUE")
  expect_match(imputation, 'last_time = "survival_time_days"')
  expect_match(imputation, "draws = 1L")
  expect_match(imputation, "stats::rbinom")
  expect_match(imputation, "for \\(index in seq_len\\(config\\$imputation\\$main\\$m\\)\\)")
  expect_match(imputation, "m = 1L")
  expect_match(imputation, 'where\\[, "landmark_q30"\\]')
  expect_match(imputation, "imputation_data\\$death_182 == 0L")
  expect_match(imputation, '"diagnosis_code"')
  expect_match(imputation, "ecog_fstcnt = 0:4")
  expect_match(imputation, "baseline_q30 = 1:7")
  expect_match(imputation, "landmark_q30 = 1:7")
  expect_match(imputation, "imputation_data\\[\\[variable\\]\\] <- ordered")
  expect_match(imputation, 'methods\\[ordinal_columns\\] <- "polr"')
  expect_match(imputation, 'predictors\\[setdiff\\(imputed_columns, "landmark_q30"\\), "landmark_q30"\\] <- 0L')
  expect_match(imputation, "plan_code")
  expect_match(imputation, "c_reactive_protein")
  expect_match(fitting, "y_landmark ~ tx \\+ baseline_q30")
  expect_match(fitting, "pcontrast = pcontrast_tx")
  expect_match(fitting, "sd = 0.5")
  expect_match(estimand, "probability_benefit = mean\\(draws\\$log_odds_ratio_worse < 0\\)")
})

test_that("QoL supporting analyses use their prespecified imputation counts", {
  unadjusted <-liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "03-qol-unadjusted-landmark", "01-fitting.qmd"
  ))
  unadjusted_descriptive <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "03-qol-unadjusted-landmark",
    "04-descriptive.qmd"
  ))
  unadjusted_po_check <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "03-qol-unadjusted-landmark",
    "05-proportional-odds-check.qmd"
  ))
  longitudinal_imputation <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "04-qol-death-inclusive-longitudinal",
    "01-imputation.qmd"
  ))
  longitudinal_imputation_diagnostics <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "04-qol-death-inclusive-longitudinal",
    "02-imputation-diagnostics.qmd"
  ))
  longitudinal_fitting <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "04-qol-death-inclusive-longitudinal",
    "03-fitting.qmd"
  ))
  longitudinal_model_diagnostics <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "04-qol-death-inclusive-longitudinal",
    "04-model-diagnostics.qmd"
  ))
  longitudinal_estimand <- liqplat_qmd_text(file.path(
    "analysis", "03-supporting", "04-qol-death-inclusive-longitudinal",
    "05-estimand.qmd"
  ))

  expect_match(unadjusted, "config\\$imputation\\$supporting\\$indices")
  expect_match(unadjusted, '"artifacts", "primary", "quality-of-life", "imputations.rds"')
  expect_match(unadjusted, "y_landmark ~ tx")
  expect_match(unadjusted_descriptive, "config\\$imputation\\$supporting\\$indices")
  expect_match(unadjusted_descriptive, '"artifacts", "primary", "quality-of-life", "prepared.parquet"')
  expect_match(unadjusted_po_check, "config\\$imputation\\$supporting\\$indices")
  expect_match(unadjusted_po_check, "y_landmark ~ tx")
  expect_match(unadjusted_po_check, "binary_worse ~ tx")
  expect_match(
    longitudinal_imputation,
    "Independent long-format imputation using the configured completed datasets"
  )
  expect_match(longitudinal_imputation, "n_imputations <- as.integer\\(config\\$imputation\\$main\\$m\\)")
  expect_match(longitudinal_imputation, "m = n_imputations")
  expect_match(
    longitudinal_imputation,
    "indices <- seq_len\\(n_imputations\\)"
  )
  expect_match(longitudinal_imputation, "baseline_fit_is_current")
  expect_match(longitudinal_imputation, "history_proxy")
  expect_match(
    longitudinal_imputation,
    'methods\\["q30"\\] <- "polr"'
  )
  expect_match(
    longitudinal_imputation,
    "factor\\(yprev_imputation, levels = 1:7\\)"
  )
  expect_match(longitudinal_imputation, "polr.to.loggedEvents = TRUE")
  expect_false(grepl("2l.pmm", longitudinal_imputation, fixed = TRUE))
  expect_match(longitudinal_imputation, "death_template")
  expect_match(longitudinal_imputation, "does not preserve all patient IDs")
  expect_match(longitudinal_fitting, "factor\\(diagnosis_code\\)")
  expect_match(longitudinal_fitting, "pcontrast = pcontrast_tx")
  expect_match(longitudinal_fitting, "default_retained_draws <- 3000L")
  expect_match(longitudinal_fitting, "iter = default_iterations")
  expect_match(longitudinal_fitting, "warmup = default_warmup")
  expect_match(longitudinal_fitting, "identical\\(cached_specification, fit_specification\\)")
  expect_match(longitudinal_fitting, "r_version = as.character\\(getRversion\\(\\)\\)")
  expect_match(longitudinal_fitting, "fit-attempts-%02d.parquet")
  expect_match(longitudinal_fitting, "diagnostic_failure_reason")
  expect_match(longitudinal_imputation_diagnostics, "raw-qol-trajectories.png")
  expect_match(longitudinal_imputation_diagnostics, "pairwise_agreement")
  expect_match(longitudinal_imputation_diagnostics, "longitudinal-q30-traces.png")
  expect_false(grepl("plot_correlation", longitudinal_imputation_diagnostics, fixed = TRUE))
  expect_false(grepl("plot_variogram", longitudinal_imputation_diagnostics, fixed = TRUE))
  expect_match(longitudinal_model_diagnostics, "rmsb::stanDx")
  expect_match(longitudinal_model_diagnostics, "rmsb::stanDxplot")
  expect_match(longitudinal_model_diagnostics, "fit-attempts-all.parquet")
  expect_match(longitudinal_estimand, "mostr::avg_sops")
  expect_match(longitudinal_estimand, "include_re = TRUE")
  expect_match(longitudinal_estimand, "validate_sop_intervals")
  expect_match(longitudinal_estimand, "geom_ribbon")
  expect_match(longitudinal_estimand, 'state %in% 1:3 ~ "Good"')
  expect_match(longitudinal_estimand, "draw = tx_1 - tx_0")
  expect_match(longitudinal_estimand, "computationally_reduced = isTRUE\\(config\\$imputation\\$development\\)")
})

test_that("technical validity is derived from the LOD fields, not a source flag", {
  data <- data.frame(
    id = c("p01", "p02", "p03", "p04"),
    sample_id = paste0("s0", 1:4),
    sample_date = as.Date(c("2026-01-01", NA, "2026-01-03", "2026-01-04")),
    lod_q1 = c(0.2, NA, NA, NA),
    lod_q3 = c(0.3, NA, 0.4, NA),
    ctdna_error = c(1L, 0L, 1L, 0L)
  )
  out <- derive_ctdna_technical_validity(data)
  expect_equal(out$attempted_sample, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(out$ctdna_error_derived, c(0L, 0L, 0L, 1L))
  expect_equal(out$ctdna_error_qc, c(1, 0, 1, 0))
  summary <- summarize_technical_error(out, attempted_col = "attempted_sample")
  expect_equal(summary$samples, 3L)
  expect_equal(summary$patients, 3L)
  expect_equal(summary$errors, 1L)
  expect_equal(summary$no_error, 2L)
})

test_that("actionability levels stay exact, non-exclusive, and CHIP-aware", {
  data <- data.frame(
    id = c("p01", "p01", "p02", "p03", "p04"),
    alteration_id = paste0("a0", 1:5),
    alteration_type = c("mutation", "fusion", "mutation", "mutation", "mutation"),
    sensitivity = c("Level 1", "Level 2", "unknown", "", "Level 3"),
    resistance = c("Level R", "unknown", "Level R", "", "unknown"),
    valid_ctdna_result = c(1L, 1L, 1L, 1L, 1L),
    chip_suspicion = c(0L, 0L, 1L, 0L, 0L),
    stringsAsFactors = FALSE
  )
  expect_equal(actionability_exact_levels(c("Level 2", "Level 1", "Level 2")),
               c("Level 1", "Level 2"))
  expect_false(any(is_recorded_actionability_evidence(
    c(NA, "", "unknown", "not assessed", "not tested", "no evidence")
  )))
  summary <- summarize_actionability_levels(data, alteration_type_col = "alteration_type")
  expect_equal(summary$denominator, 4L)
  expect_true(all(c("Level 1", "Level 2", "Level 3", "Level R") %in%
                    summary$alteration_levels$actionability_level))
  patient <- summary$patient_summary
  expect_true(patient$any_sensitivity[patient$id == "p01"])
  expect_true(patient$any_resistance[patient$id == "p01"])
  expect_true(patient$no_actionable_finding[patient$id == "p02"])
  expect_true(patient$no_actionable_finding[patient$id == "p03"])
  expect_equal(summarize_actionability(data, chip_col = "chip_suspicion")$denominator, 4L)
})

test_that("SAP and production actionability tables name every OncoKB output", {
  sap <- liqplat_qmd_text(file.path("sap", "SAP.qmd"))
  notebook <- liqplat_qmd_text(file.path(
    "analysis", "04-implementation", "09-actionability.qmd"
  ))

  for (level in c("1", "2", "3A", "3B", "4", "R1", "R2")) {
    expect_match(sap, paste0("\\| ", level, " \\|"))
  }
  expect_match(sap, "1, 2, 3A, 3B, and 4")
  expect_match(sap, "R1 and R2")
  expect_match(sap, "patients with at least one mutation")
  expect_match(sap, "patients with at least one CNV")
  expect_match(sap, "patients with at least one fusion")
  expect_match(sap, "no actionable finding", ignore.case = TRUE)

  expect_match(notebook, "No\\s+regression model is fitted")
  expect_match(notebook, "mutation_patient_count")
  expect_match(notebook, "cnv_patient_count")
  expect_match(notebook, "fusion_patient_count")
  expect_match(notebook, "actionability_qc")
  expect_match(notebook, "is_recorded_actionability_evidence")
  expect_match(notebook, 'type_values %in% c\\("cnv", "fusion"\\)')
})

test_that("MTB summaries retain missing discussion counts and validate counts", {
  data <- data.frame(
    id = c("p01", "p02", "p03", "p04"),
    mtb_reg = c(1L, 0L, 1L, 0L),
    n_mtb = c(2L, 0L, NA_integer_, 1L)
  )
  out <- summarize_mtb_referral(data)
  expect_equal(out$patients, 4L)
  expect_equal(out$mtb_registered, 2L)
  expect_equal(out$mtb_discussed, 2L)
  expect_equal(out$missing_discussion_count, 1L)
  expect_error(
    summarize_mtb_referral(transform(data, n_mtb = c(1, 0, -1, 2))),
    "non-negative integers"
  )
})
