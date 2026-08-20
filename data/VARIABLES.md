# Variable decisions and pending verification

This note is part of the public data contract. It records source-system
questions that must be resolved in the private preparation environment before
the corresponding public Parquet can be marked analyzable. No value is
imputed merely because a field is absent.

## Baseline and treatment fields

- `plan_fstcnt_coded` must be confirmed as treatment intent at baseline. The
  reference date, category coding (including unknown, undecided, and
  non-applicable), reference category, version changes, and inclusion in the
  QoL imputation model remain to be documented.
- `bsc_start_date` must mean the documented clinical start/decision for best
  supportive care, not an arbitrary referral or concurrent palliative-care
  date. The time origin is first-line treatment initiation. If several
  candidate dates exist, the source hierarchy must be recorded. `death_date`
  and `follow_up_end_date` are required to derive censoring reproducibly; the
  treatment of death before BSC remains a prespecified competing-event issue.
- `treatment_switch_date` must identify the first qualifying switch/stop from
  first-line treatment. Dose changes, temporary interruptions, maintenance,
  substitutions, completion, death, and censoring need separate event types.
  Retain treatment start/stop, death, and follow-up-end dates. The source
  definition and the BSC/death handling must be reconciled with the SAP.
- `targeted_therapy` requires an auditable treatment classification, therapy
  name, start date, treatment line, molecular target, and whether treatment was
  linked to an actionable alteration. Distinguish no therapy from missing
  treatment information and incomplete follow-up. The SAP's unadjusted
  group-only comparison is the current analysis rule; the process prototype's
  age/cancer adjustment is not silently substituted. Any planned 6-, 12-, and
  24-month follow-up definitions must be fixed before analysis.

## ctDNA technical validity

- `ctdna_error` is a source QC flag whose unit (sample, laboratory analysis,
  run, or patient), failure categories, repeat-attempt representation, and
  coding direction must be documented with `sample_id`, pseudonymous patient
  identifier, sampling/report dates, and failure reason in the private source.
- The public technical endpoint is derived for attempted samples only: an
  attempt requires a sampling date and the analysis error is one exactly when
  both LOD Q1 and LOD Q3 are missing. The entered `ctdna_error` flag is retained
  for QC comparison and does not replace this definition.

## CHIP and actionability

- `chip_suspicion` is expected at variant level. Preserve links to variant,
  gene, sample, and pseudonymous patient, retain the sample time point, record
  coding for uncertain/unclassifiable variants, and document duplicate
  variants across samples. The canonical public name is `chip_suspicion`.
- Actionability is taken from exact OncoKB sensitivity-level and
  resistance-level fields. Keep mutation, CNV, and fusion records in a long
  table; do not rank or collapse levels. Alteration-level counts include every
  exact recorded level. Patient-level proportions use patients with at least
  one valid ctDNA result as denominator and allow a patient to occupy multiple
  levels. CHIP alterations are excluded from actionability summaries.
- “No actionable finding” means no non-CHIP alteration has any recorded exact
  sensitivity/resistance evidence. Unknown, not-assessed, and missing values
  remain visible as recorded/missing states and are not treated as evidence.

## Solid pathology outcomes (pending)

- `solid_msi_status` and `solid_mmr_status` are pending because the
  2026-08-20 REDCap dictionary contains a repeating `solid_biopsy` form but no
  identifiable MSI/MMR result fields. If added, retain every specimen,
  sampling/report dates, assay method, source report, quantitative result or
  protein-loss detail, and explicit categories including not tested and
  indeterminate. MSI categories are `MSS`, `MSI-low`, `MSI-high`,
  indeterminate/not evaluable, and not tested; MMR categories are `pMMR`,
  `dMMR`, indeterminate/not evaluable, and not tested. Do not equate MMR IHC
  with a directly measured MSI result. Baseline specimen selection must use
  the documented sampling date rule; follow-up specimens are not copied into a
  patient-level baseline field.
- `solid_tmb_value` and `solid_tmb_category` are pending for the same reason.
  When available, retain numeric `mut/Mb` value, assay/panel version,
  laboratory, report-specific high-TMB threshold, specimen code, sampling and
  report dates, and explicit untested/indeterminate states. Never apply a
  universal threshold at extraction.

## Follow-up pathology utilization

Event-level non-liquid pathology records must retain pseudonymous patient and
sample codes, extraction/sampling date, material type, order identifier, and
topography. Use the pathology extraction date as the preferred sampling date;
maintain an explicit exclusion list for liquid-biopsy material/codes. The
primary count is unique patient/date/sample combinations after randomization,
with strict `sample_date > randomization_date` when timestamps are unavailable;
same-day records are examined separately. A sample count is not called a
procedure count. Common horizons or time-at-risk adjustment are required, and
the event-level count must be reconciled to any aggregate biopsy field. The
private extraction should join
`CDWH.V_IL_DIM_LABOR_SAMPLE_PATHOLOGY_CID` to
`CDWH.V_IL_DIM_LABOR_ORDER_PATHOLOGY_CID` on `LOP_BK`, link `PAT_BK` through
the authorized crosswalk, and retain the CDWH extraction/data-lock version.
Suggested repeating fields are `other_biopsy_sample_code`,
`other_biopsy_sample_date`, `other_biopsy_material_type_code`,
`other_biopsy_order_id`, `other_biopsy_topography_code`,
`other_biopsy_source`, and `other_biopsy_extract_date`.

## MTB

MTB registration uses `mtb_reg`; MTB discussion success uses numeric `n_mtb >=
1`. The distribution of discussion counts and missingness are reported. Other
referral fields are not used as substitutes.
