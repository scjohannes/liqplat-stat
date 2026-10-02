# Covariate derivations shared by the analysis notebooks.
#
# Packages the calling notebook must attach: tidyverse (dplyr).


# Modified Glasgow Prognostic Score: 0 if CRP <= 10 mg/L, otherwise 1 if
# albumin >= 35 g/L and 2 if albumin < 35 g/L; missing if either is missing.
derive_mgps <- function(albumin, c_reactive_protein) {
  # Factors (e.g. from mice) must be converted via their labels, not codes.
  albumin <- if (is.factor(albumin)) {
    as.numeric(as.character(albumin))
  } else {
    as.numeric(albumin)
  }
  c_reactive_protein <- if (is.factor(c_reactive_protein)) {
    as.numeric(as.character(c_reactive_protein))
  } else {
    as.numeric(c_reactive_protein)
  }

  if (length(albumin) != length(c_reactive_protein)) {
    stop("Albumin and C-reactive protein must have equal lengths for mGPS.")
  }

  case_when(
    is.na(albumin) | is.na(c_reactive_protein) ~ NA_integer_,
    c_reactive_protein <= 10 ~ 0L,
    albumin >= 35 ~ 1L,
    .default = 2L
  )
}


# Binary ECOG performance status: 1 for ECOG >= 2, 0 for ECOG 0-1, NA if
# missing.
derive_ecog_binary <- function(ecog_fstcnt) {
  ecog_fstcnt <- if (is.factor(ecog_fstcnt)) {
    as.numeric(as.character(ecog_fstcnt))
  } else {
    as.numeric(ecog_fstcnt)
  }

  # A missing ECOG gives NA > 1 = NA, which stays NA_integer_.
  as.integer(ecog_fstcnt > 1L)
}


# Broad diagnosis categories from SNOMED source codes (carried forward from
# the original Table 1 prototype). Unknown codes are returned unchanged.
snomed_to_diagnosis <- function(snomed_code) {
  case_when(
    snomed_code == "SNM_126952004" ~ "Primary brain tumor",
    snomed_code == "SNM_255055008" ~ "Head and neck",
    snomed_code == "SNM_93880001" ~ "Lung tumor (including mesothelioma)",
    snomed_code == "SNM_254837009" ~ "Breast cancer",
    snomed_code == "SNM_363349007_363402007" ~ "Stomach/esophagus cancer",
    snomed_code == "SNM_363402007" ~ "Esophageal cancer",
    snomed_code == "SNM_363349007" ~ "Stomach cancer",
    snomed_code == "SNM_363418001_312104005" ~
      "Pancreatic cancer / Cholangiocarcinoma",
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
    .default = snomed_code
  )
}
