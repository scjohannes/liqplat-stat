# CHIP classification of ctDNA alteration records from matched solid-biopsy
# and buffy-coat findings. Used by analysis/10-implementation/08-chip.qmd and
# 09-actionability.qmd.
#
# Returns one of: "not_applicable" (CNV, fusion and other non-mutation types),
# "possible_germline", "definite_chip", "definite_non_chip",
# "tissue_positive_buffy_missing", "ctdna_only_unknown", or "unresolved".
# Later assignments overwrite earlier ones, so the order of the assignments
# below is part of the definition (for example, buffy coat negative is
# "definite_non_chip" whatever the solid-biopsy result).
classify_chip_status <- function(alteration_type,
                                 found_in_solid_biopsy,
                                 found_in_buffy_coat) {
  type <- tolower(trimws(as.character(alteration_type)))
  type[type %in% c("snv", "snp", "indel", "small_variant")] <- "mutation"

  tissue <- suppressWarnings(as.numeric(as.character(found_in_solid_biopsy)))
  if (any(!is.na(tissue) & !tissue %in% c(0, 1))) {
    stop("found_in_solid_biopsy must be coded 0/1.")
  }
  buffy <- suppressWarnings(as.numeric(as.character(found_in_buffy_coat)))
  if (any(!is.na(buffy) & !buffy %in% c(0, 1))) {
    stop("found_in_buffy_coat must be coded 0/1.")
  }

  mutation <- !is.na(type) & type == "mutation"

  status <- rep("unresolved", length(type))
  status[which(!is.na(type) & type != "mutation")] <- "not_applicable"
  status[which(mutation & tissue == 1 & buffy == 1)] <- "possible_germline"
  status[which(mutation & tissue == 0 & buffy == 1)] <- "definite_chip"
  status[which(mutation & !is.na(buffy) & buffy == 0)] <- "definite_non_chip"
  status[which(mutation & tissue == 1 & is.na(buffy))] <-
    "tissue_positive_buffy_missing"
  status[which(mutation & tissue == 0 & is.na(buffy))] <- "ctdna_only_unknown"
  status
}
