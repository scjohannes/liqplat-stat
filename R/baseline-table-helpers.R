# Same item-wise fallback rule as primary QoL preparation.
complete_table_baseline_qol <- function(cohort, qol) {
  visit_field <- intersect(c("date_visit", "visit_date"), names(cohort))
  stopifnot(length(visit_field) == 1L, !anyDuplicated(cohort$id),
    all(c("baseline_q29", "baseline_q30") %in% names(cohort)),
    all(c("id", "assessment_id", "questionnaire_date", "q29", "q30") %in% names(qol)))
  visits <- data.frame(id = cohort$id, date_visit = as.Date(cohort[[visit_field]]))
  for (item in c("q29", "q30")) {
    baseline_item <- paste0("baseline_", item)
    candidates <- qol |>
      dplyr::filter(id %in% cohort$id[is.na(cohort[[baseline_item]])],
                    !is.na(.data[[item]]), !is.na(questionnaire_date)) |>
      dplyr::inner_join(visits, by = "id") |>
      dplyr::mutate(distance_days = abs(as.integer(as.Date(questionnaire_date) - date_visit))) |>
      dplyr::filter(!is.na(distance_days), distance_days <= 14L) |>
      dplyr::arrange(id, distance_days, questionnaire_date, assessment_id) |>
      dplyr::distinct(id, .keep_all = TRUE)
    index <- match(cohort$id, candidates$id)
    missing <- is.na(cohort[[baseline_item]])
    cohort[[baseline_item]][missing] <- candidates[[item]][index[missing]]
  }
  cohort
}

# Exported baseline QoL items run from 1 (best) to 7 (worst).
baseline_ql2 <- function(q29, q30) {
  if (length(q29) != length(q30)) stop("Baseline item lengths differ.")
  if (any(!is.na(q29) & !q29 %in% 1:7) || any(!is.na(q30) & !q30 %in% 1:7)) {
    stop("Baseline QoL items must be integers from 1 to 7 or missing.")
  }
  score <- (rowMeans(cbind(8 - q29, 8 - q30), na.rm = TRUE) - 1) / 6 * 100
  score[is.nan(score)] <- NA_real_
  score
}

# Broad source categories, carried forward from the original Table 1 prototype.
snomed_to_diagnosis <- function(snomed_code) {
  dplyr::case_when(
    snomed_code == "SNM_126952004" ~ "Primary brain tumor",
    snomed_code == "SNM_255055008" ~ "Head and neck",
    snomed_code == "SNM_93880001" ~ "Lung tumor (including mesothelioma)",
    snomed_code == "SNM_254837009" ~ "Breast cancer",
    snomed_code == "SNM_363349007_363402007" ~ "Stomach/esophagus cancer",
    snomed_code == "SNM_363402007" ~ "Esophageal cancer",
    snomed_code == "SNM_363349007" ~ "Stomach cancer",
    snomed_code == "SNM_363418001_312104005" ~ "Pancreatic cancer / Cholangiocarcinoma",
    snomed_code == "SNM_363418001" ~ "Pancreatic cancer",
    snomed_code == "SNM_312104005" ~ "Cholangiocarcinoma",
    snomed_code == "SNM_109841003" ~ "Hepatocellular carcinoma",
    snomed_code == "SNM_363509000" ~ "Small intestine cancer",
    snomed_code == "SNM_781382000" ~ "Colorectal cancer",
    snomed_code == "SNM_702391001" ~ "Kidney tumour",
    snomed_code == "SNM_448233000" ~ "Cancer of the urinary tract",
    snomed_code == "SNM_399068003" ~ "Prostate cancer",
    snomed_code == "SNM_363514001" ~ "Gynecological tumors",
    snomed_code == "SNM_372130007" ~ "Primary skin tumor",
    snomed_code == "SNM_424413001" ~ "Soft tissue sarcoma",
    snomed_code == "SNM_448710000" ~ "Bone sarcoma",
    snomed_code == "SNM_118600007" ~ "Lymphoma",
    snomed_code == "SNM_109989006" ~ "Multiple myeloma",
    snomed_code == "SNM_93143009" ~ "Leukemia",
    snomed_code == "SNM_255046005" ~ "Neuroendocrine tumors",
    snomed_code == "SNM_126900000" ~ "Testicular cancer",
    snomed_code == "SNM_255052006" ~ "Cancer of unknown primary",
    snomed_code == "SNM_74964007" ~ "Other",
    snomed_code == "SNM_261665006" ~ "Unclear",
    snomed_code == "SNM_110396000" ~ "No malignant disease",
    TRUE ~ snomed_code  # Return as-is if no match found
  )
}
