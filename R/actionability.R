# Shared implementation-outcome derivations.
#
# These functions deliberately keep source fields unchanged. Derived flags
# are named explicitly so that an observed QC flag cannot silently replace a
# prespecified analysis definition.

actionability_sensitivity_levels <- function(data = NULL, sensitivity_col = "sensitivity") {
  if (is.null(data)) return(character())
  if (!sensitivity_col %in% names(data)) return(character())
  actionability_exact_levels(data[[sensitivity_col]])
}

actionability_resistance_levels <- function(data = NULL, resistance_col = "resistance") {
  if (is.null(data)) return(character())
  if (!resistance_col %in% names(data)) return(character())
  actionability_exact_levels(data[[resistance_col]])
}

actionability_exact_levels <- function(x) {
  values <- trimws(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]
  sort(unique(values), method = "radix")
}

validate_actionability_levels <- function(data, sensitivity_col = "sensitivity",
                                           resistance_col = "resistance") {
  for (column_name in intersect(c(sensitivity_col, resistance_col), names(data))) {
    values <- data[[column_name]]
    if (!(is.character(values) || is.factor(values))) {
      stop("Actionability level column must be character or factor: ", column_name)
    }
  }
  invisible(TRUE)
}

reshape_actionability_long <- function(data, id_col = "id", alteration_col = "alteration_id",
                                       sensitivity_col = "sensitivity", resistance_col = "resistance",
                                       chip_col = NULL, alteration_type_col = NULL) {
  required <- c(id_col, alteration_col, sensitivity_col, resistance_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Actionability columns are missing: ", paste(missing, collapse = ", "))
  }
  validate_actionability_levels(data, sensitivity_col, resistance_col)
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")
  long <- tidyr::pivot_longer(
    data,
    cols = tidyr::all_of(c(sensitivity_col, resistance_col)),
    names_to = "actionability_type",
    values_to = "actionability_level",
    values_drop_na = FALSE
  )
  long$actionability_level <- trimws(as.character(long$actionability_level))
  long$recorded_exact_level <- !is.na(long$actionability_level) &
    nzchar(long$actionability_level)
  if (!is.null(chip_col)) {
    if (!chip_col %in% names(long)) stop("CHIP column is missing: ", chip_col)
    chip <- validate_binary(long[[chip_col]], chip_col)
    long$non_chip <- !is.na(chip) & chip == 0
  } else {
    long$non_chip <- NA
  }
  if (!is.null(alteration_type_col)) {
    if (!alteration_type_col %in% names(long)) {
      stop("Alteration-type column is missing: ", alteration_type_col)
    }
    long$alteration_type <- as.character(long[[alteration_type_col]])
  }
  long
}

# Unknown/not-assessed entries are retained as exact recorded levels in the
# alteration table, but are not evidence of an actionable level. No ordering
# or ranking is applied to source levels.
is_recorded_actionability_evidence <- function(x) {
  values <- trimws(as.character(x))
  missing_or_non_evidence <- is.na(values) | !nzchar(values) |
    tolower(values) %in% c("unknown", "not_assessed", "not assessed",
                           "not available", "not_applicable", "not applicable",
                           "none", "na", "n/a")
  !missing_or_non_evidence
}

is_sensitivity <- function(x) is_recorded_actionability_evidence(x)
is_resistance <- function(x) is_recorded_actionability_evidence(x)

valid_patient_denominator <- function(data, id_col = "id", valid_col = NULL) {
  if (!id_col %in% names(data)) stop("Patient identifier is missing: ", id_col)
  valid <- if (is.null(valid_col)) rep(TRUE, nrow(data)) else {
    if (!valid_col %in% names(data)) stop("Validity column is missing: ", valid_col)
    !is.na(data[[valid_col]]) & as.logical(data[[valid_col]])
  }
  unique_ids <- unique(data[[id_col]][valid])
  unique_ids <- unique_ids[!is.na(unique_ids)]
  length(unique_ids)
}

patient_actionability_summary <- function(data, id_col = "id",
                                          sensitivity_col = "sensitivity",
                                          resistance_col = "resistance",
                                          valid_col = NULL,
                                          chip_col = NULL,
                                          non_chip_only = !is.null(chip_col)) {
  required <- c(id_col, sensitivity_col, resistance_col, valid_col,
                if (isTRUE(non_chip_only)) chip_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Actionability columns are missing: ", paste(missing, collapse = ", "))
  }
  validate_actionability_levels(data, sensitivity_col, resistance_col)
  valid <- if (is.null(valid_col)) rep(TRUE, nrow(data)) else {
    !is.na(data[[valid_col]]) & as.logical(data[[valid_col]])
  }
  valid_ids <- unique(data[[id_col]][valid & !is.na(data[[id_col]])])
  chip <- if (isTRUE(non_chip_only)) validate_binary(data[[chip_col]], chip_col) else NULL
  out <- lapply(valid_ids, function(identifier) {
    rows <- valid & !is.na(data[[id_col]]) & data[[id_col]] == identifier
    if (isTRUE(non_chip_only)) rows <- rows & !is.na(chip) & chip == 0
    sensitivity <- any(is_recorded_actionability_evidence(
      data[[sensitivity_col]][rows]
    ))
    resistance <- any(is_recorded_actionability_evidence(
      data[[resistance_col]][rows]
    ))
    data.frame(
      id = identifier,
      any_sensitivity = sensitivity,
      any_resistance = resistance,
      any_actionable = sensitivity || resistance,
      no_actionable_finding = !sensitivity && !resistance,
      stringsAsFactors = FALSE
    )
  })
  if (length(out) == 0L) {
    empty_id <- data[[id_col]][FALSE]
    return(data.frame(
      id = empty_id,
      any_sensitivity = logical(),
      any_resistance = logical(),
      any_actionable = logical(),
      no_actionable_finding = logical(),
      stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, out)
  names(result)[1L] <- id_col
  rownames(result) <- NULL
  result
}

summarize_actionability_levels <- function(data, id_col = "id",
                                           alteration_col = "alteration_id",
                                           sensitivity_col = "sensitivity",
                                           resistance_col = "resistance",
                                           valid_col = "valid_ctdna_result",
                                           chip_col = "chip_suspicion",
                                           alteration_type_col = NULL) {
  required <- c(id_col, alteration_col, sensitivity_col, resistance_col,
                valid_col, chip_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Actionability columns are missing: ", paste(missing, collapse = ", "))
  }
  valid <- !is.na(data[[valid_col]]) & as.logical(data[[valid_col]]) &
    !is.na(data[[id_col]])
  denominator <- length(unique(data[[id_col]][valid]))
  long <- reshape_actionability_long(
    data[valid, , drop = FALSE], id_col, alteration_col,
    sensitivity_col, resistance_col, chip_col, alteration_type_col
  )
  long <- long[!is.na(long$actionability_level) &
                 nzchar(long$actionability_level) &
                 long$non_chip, , drop = FALSE]
  if (nrow(long) == 0L) {
    level_summary <- data.frame(
      actionability_type = character(), actionability_level = character(),
      alteration_count = integer(), patient_count = integer(),
      patient_proportion = numeric(), stringsAsFactors = FALSE
    )
  } else {
    level_key <- paste(long[[id_col]], long[[alteration_col]],
                       long$actionability_type, long$actionability_level,
                       sep = "\r")
    long_unique <- long[!duplicated(level_key), , drop = FALSE]
    level_key <- interaction(long_unique$actionability_type,
                             long_unique$actionability_level,
                             drop = TRUE, lex.order = TRUE)
    level_rows <- split(seq_len(nrow(long_unique)), level_key)
    level_summary <- do.call(rbind, lapply(level_rows, function(index) {
      values <- long_unique[index, , drop = FALSE]
      data.frame(
        actionability_type = as.character(values$actionability_type[[1L]]),
        actionability_level = as.character(values$actionability_level[[1L]]),
        alteration_count = nrow(values),
        patient_count = length(unique(values[[id_col]])),
        patient_proportion = if (denominator == 0L) NA_real_ else
          length(unique(values[[id_col]])) / denominator,
        stringsAsFactors = FALSE
      )
    }))
    rownames(level_summary) <- NULL
  }
  patient_summary <- patient_actionability_summary(
    data, id_col, sensitivity_col, resistance_col, valid_col,
    chip_col = chip_col, non_chip_only = TRUE
  )
  list(
    denominator = denominator,
    alteration_levels = level_summary,
    patient_summary = patient_summary
  )
}

summarize_actionability <- function(data, id_col = "id",
                                    sensitivity_col = "sensitivity",
                                    resistance_col = "resistance",
                                    valid_col = NULL, chip_col = NULL) {
  patient <- patient_actionability_summary(
    data, id_col, sensitivity_col, resistance_col, valid_col,
    chip_col = chip_col, non_chip_only = !is.null(chip_col)
  )
  denominator <- nrow(patient)
  if (denominator == 0L) {
    return(data.frame(denominator = 0L, any_sensitivity = 0L,
                      any_resistance = 0L, any_actionable = 0L,
                      no_actionable_finding = 0L, stringsAsFactors = FALSE))
  }
  data.frame(
    denominator = denominator,
    any_sensitivity = sum(patient$any_sensitivity),
    any_resistance = sum(patient$any_resistance),
    any_actionable = sum(patient$any_actionable),
    no_actionable_finding = sum(patient$no_actionable_finding),
    stringsAsFactors = FALSE
  )
}

# LOD-derived technical-error definition. The explicitly entered source flag
# is retained as QC only and is never used to define the analysis endpoint.
derive_ctdna_technical_validity <- function(data, sample_date_col = "sample_date",
                                             lod_q1_col = "lod_q1",
                                             lod_q3_col = "lod_q3",
                                             explicit_error_col = "ctdna_error") {
  required <- c(sample_date_col, lod_q1_col, lod_q3_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Technical-validity columns are missing: ", paste(missing, collapse = ", "))
  }
  sample_date <- parse_analysis_date(data[[sample_date_col]], sample_date_col)
  attempted <- !is.na(sample_date)
  lod_q1 <- suppressWarnings(as.numeric(as.character(data[[lod_q1_col]])))
  lod_q3 <- suppressWarnings(as.numeric(as.character(data[[lod_q3_col]])))
  out <- data
  out$attempted_sample <- attempted
  out$ctdna_error_derived <- as.integer(attempted & is.na(lod_q1) & is.na(lod_q3))
  if (!is.null(explicit_error_col) && explicit_error_col %in% names(data)) {
    out$ctdna_error_qc <- validate_binary(data[[explicit_error_col]], explicit_error_col)
  } else {
    out$ctdna_error_qc <- NA_real_
  }
  out
}

summarize_technical_error <- function(data, error_col = "ctdna_error_derived",
                                      sample_col = "sample_id", id_col = "id",
                                      attempted_col = NULL) {
  required <- c(error_col, sample_col, id_col, attempted_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Technical-error columns are missing: ", paste(missing, collapse = ", "))
  }
  attempted <- if (is.null(attempted_col)) rep(TRUE, nrow(data)) else
    !is.na(data[[attempted_col]]) & as.logical(data[[attempted_col]])
  values <- validate_binary(data[[error_col]], error_col)
  values <- values[attempted]
  if (anyNA(values)) stop("Derived technical error must be observed for attempted samples.")
  sample_ids <- data[[sample_col]][attempted]
  patient_ids <- data[[id_col]][attempted]
  data.frame(
    samples = length(unique(sample_ids[!is.na(sample_ids)])),
    patients = length(unique(patient_ids[!is.na(patient_ids)])),
    errors = sum(values == 1),
    no_error = sum(values == 0),
    error_rate = if (length(values) == 0L) NA_real_ else mean(values == 1),
    stringsAsFactors = FALSE
  )
}

# Earliest valid pre-treatment sample. The old randomization-day window is
# intentionally not accepted: it can select post-treatment material.
select_baseline_samples <- function(data, id_col = "id", sample_date_col = "sample_date",
                                    treatment_date_col = "treatment_start_date",
                                    valid_col = "valid_ctdna_result",
                                    origin_date_col = NULL, window_days = NULL) {
  if (!is.null(origin_date_col) || !is.null(window_days)) {
    warning("`origin_date_col` and `window_days` are ignored; baseline is defined before treatment.",
            call. = FALSE)
  }
  required <- c(id_col, sample_date_col, treatment_date_col, valid_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Baseline-sample columns are missing: ", paste(missing, collapse = ", "))
  }
  sample_date <- parse_analysis_date(data[[sample_date_col]], sample_date_col)
  treatment_date <- parse_analysis_date(data[[treatment_date_col]], treatment_date_col)
  valid <- !is.na(data[[id_col]]) & !is.na(sample_date) & !is.na(treatment_date) &
    sample_date < treatment_date &
    !is.na(data[[valid_col]]) & as.logical(data[[valid_col]])
  out <- data[valid, , drop = FALSE]
  if (nrow(out) == 0L) return(out)
  out$.sample_date_order <- sample_date[valid]
  tie_columns <- intersect(c("sample_id", "alteration_id"), names(out))
  tie_values <- if (length(tie_columns) == 0L) rep("", nrow(out)) else
    do.call(paste, c(out[tie_columns], sep = "\r"))
  order_idx <- order(out[[id_col]], out$.sample_date_order, tie_values,
                     na.last = TRUE, method = "radix")
  out <- out[order_idx, , drop = FALSE]
  out <- out[!duplicated(out[[id_col]]), , drop = FALSE]
  out$.sample_date_order <- NULL
  rownames(out) <- NULL
  out
}

summarize_baseline_detection <- function(cohort, baseline_samples,
                                         id_col = "id", detected_col = "ctdna_detected") {
  if (!id_col %in% names(cohort)) {
    stop("Baseline-detection cohort columns are missing: ", id_col)
  }
  required <- c(id_col, detected_col)
  missing <- setdiff(required, names(baseline_samples))
  if (length(missing) > 0L) {
    stop("Baseline-detection sample columns are missing: ", paste(missing, collapse = ", "))
  }
  ids <- unique(cohort[[id_col]][!is.na(cohort[[id_col]])])
  observed <- validate_binary(baseline_samples[[detected_col]], detected_col)
  lookup <- stats::setNames(observed, baseline_samples[[id_col]])
  detection <- unname(lookup[as.character(ids)])
  data.frame(
    id = ids,
    baseline_status = ifelse(is.na(detection), "no_qualifying_sample", "valid_baseline"),
    ctdna_detected = detection,
    stringsAsFactors = FALSE
  )
}

summarize_chip <- function(data, chip_col = "chip_suspicion", variant_col = "variant_id",
                           id_col = "id") {
  required <- c(chip_col, variant_col, id_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("CHIP columns are missing: ", paste(missing, collapse = ", "))
  chip <- validate_binary(data[[chip_col]], chip_col)
  observed <- !is.na(chip)
  data.frame(
    variants = length(unique(data[[variant_col]][observed & !is.na(data[[variant_col]])])),
    patients = length(unique(data[[id_col]][observed & !is.na(data[[id_col]])])),
    chip_variants = sum(chip == 1, na.rm = TRUE),
    chip_rate = if (sum(observed) == 0L) NA_real_ else mean(chip[observed] == 1),
    stringsAsFactors = FALSE
  )
}

summarize_chip_patient_follow_up <- function(data, id_col = "id",
                                             chip_col = "chip_suspicion",
                                             valid_col = "valid_ctdna_result",
                                             detected_col = "ctdna_detected") {
  required <- c(id_col, chip_col, valid_col, detected_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("CHIP follow-up columns are missing: ", paste(missing, collapse = ", "))
  valid <- !is.na(data[[valid_col]]) & as.logical(data[[valid_col]]) &
    !is.na(data[[detected_col]]) & validate_binary(data[[detected_col]], detected_col) == 1 &
    !is.na(data[[id_col]])
  chip <- validate_binary(data[[chip_col]], chip_col)
  ids <- unique(data[[id_col]][valid])
  any_chip <- vapply(ids, function(identifier) {
    any(chip[valid & data[[id_col]] == identifier] == 1, na.rm = TRUE)
  }, logical(1))
  data.frame(
    denominator = length(ids),
    patients_with_chip = sum(any_chip),
    proportion_with_chip = if (length(ids) == 0L) NA_real_ else mean(any_chip),
    stringsAsFactors = FALSE
  )
}

summarize_mtb_referral <- function(data, id_col = "id", referral_col = "mtb_reg",
                                   discussion_count_col = "n_mtb") {
  required <- c(id_col, referral_col, discussion_count_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("MTB columns are missing: ", paste(missing, collapse = ", "))
  referral <- validate_binary(data[[referral_col]], referral_col)
  discussion_count <- suppressWarnings(as.numeric(as.character(data[[discussion_count_col]])))
  invalid_count <- !is.na(discussion_count) &
    (!is.finite(discussion_count) | discussion_count < 0 | discussion_count != floor(discussion_count))
  if (any(invalid_count)) stop("MTB discussion counts must be non-negative integers.")
  unique_ids <- unique(data[[id_col]][!is.na(data[[id_col]])])
  distribution <- as.data.frame(table(discussion_count, useNA = "ifany"),
                                 stringsAsFactors = FALSE)
  names(distribution) <- c("n_mtb", "patients")
  distribution$n_mtb <- as.character(distribution$n_mtb)
  summary <- data.frame(
    patients = length(unique_ids),
    mtb_registered = length(unique(data[[id_col]][referral == 1 & !is.na(data[[id_col]])])),
    mtb_registered_proportion = if (length(unique_ids) == 0L) NA_real_ else
      length(unique(data[[id_col]][referral == 1 & !is.na(data[[id_col]])])) / length(unique_ids),
    mtb_discussed = length(unique(data[[id_col]][!is.na(discussion_count) & discussion_count >= 1 &
                                                    !is.na(data[[id_col]])])),
    mtb_discussed_proportion = if (length(unique_ids) == 0L) NA_real_ else
      length(unique(data[[id_col]][!is.na(discussion_count) & discussion_count >= 1 &
                                   !is.na(data[[id_col]])])) / length(unique_ids),
    missing_discussion_count = sum(is.na(discussion_count)),
    stringsAsFactors = FALSE
  )
  attr(summary, "discussion_distribution") <- distribution
  summary
}
