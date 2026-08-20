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

test_that("baseline selection chooses the earliest valid pre-treatment sample", {
  data <- data.frame(
    id = c("p01", "p01", "p01", "p01", "p02"),
    sample_id = c("s02", "s01", "s03", "s04", "s05"),
    sample_date = as.Date(c("2026-01-10", "2025-12-20", "2026-02-01", "2026-03-01", "2026-01-01")),
    treatment_start_date = as.Date(c(rep("2026-01-15", 4), "2025-12-31")),
    valid_ctdna_result = c(1L, 1L, 1L, 0L, 1L),
    stringsAsFactors = FALSE
  )
  expect_warning({
    out <- select_baseline_samples(
      data, origin_date_col = "randomization_date", window_days = 14
    )
  }, "ignored")
  expect_equal(out$id, "p01")
  expect_equal(out$sample_id, "s01")
  expect_true(out$sample_date < out$treatment_start_date)
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
