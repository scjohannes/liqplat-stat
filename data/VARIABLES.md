# Variable decisions and pending verification

This note is part of the public data contract. It records source-system
questions that must be resolved in the private preparation environment before
the corresponding public Parquet can be marked analyzable. No value is
imputed merely because a field is absent.

## Public data boundary

- `cohort_follow_up.parquet` is the single participant-level wide table. It
  contains recruitment and invitation fields, baseline covariates, follow-up
  and event dates, MTB fields, and the aggregate blood-product, CT, MRI,
  PET/PET-CT, total-imaging, and unique-biopsy-day outcomes.
- `recruitment.parquet` contains only `informed_consent_date`,
  `randomization_date`, and `group_assignment` (0 = comparator, 1 = invitation).
  It is used for calendar recruitment plots without joining to shifted cohort
  dates. Identical rows are valid and must not be deduplicated.
- Separate Parquet files also hold repeated observations:
  `qol.parquet`, `taooh.parquet`, `ctdna_samples.parquet`, and
  `alterations.parquet`.
- Missing participant-level counts remain missing. The private build does not
  infer zero from the absence of an event row.

## TAOOH observation rule

- Weekly exports continue to valid death by the lock, or last-known-alive
  censoring capped at the lock (2026-09-05), without a 182-day/26-week cap.
- The public pipeline uses `survival_time_days_unrestricted` and
  `status_death_unrestricted` from the private export. Shifted dates must not
  be compared with the unshifted lock. The primary `follow_up_end_date`
  remains capped at 182 days and must not truncate unrestricted TAOOH.
- Positive weeks run from day 1, with terminal week `ceiling(days / 7)`;
  a day-zero death is assigned week 1. Baseline weeks are retained.
- No exported rows follow the patient endpoint. The final week may be partial;
  its state summarizes only observed days. Death is state 5 in the death week.
- Empirical SOPs carry death forward through the selected plotting horizon.
  Model fitting uses observed rows only, retaining entry into death but no
  post-death transitions. Living censored states are never carried forward.
- QoL and all three blood-product counts remain capped at 182 days.
  `n_blood_products` is the combined count; `n_erythrocyte_concentrates` and
  `n_thrombocyte_concentrates` are its components. Missing counts remain missing.

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

- `chip_suspicion` is expected at mutation level and comes from the slot-specific
  `ctdna_chip1`--`ctdna_chip20` fields, not the sample-level
  `ctdna_suspicion_chip` summary. Preserve links to variant,
  gene, sample, and pseudonymous patient, retain the sample time point, record
  coding for uncertain/unclassifiable variants, and document duplicate
  variants across samples. The canonical public name is `chip_suspicion`.
  CHIP status is not applicable to CNV and fusion records; these records remain
  eligible for actionability summaries rather than being excluded as missing.
- Actionability is taken from exact OncoKB sensitivity-level and
  resistance-level fields. Keep mutation, CNV, and fusion records in a long
  table; do not collapse levels or use their order as a numeric analysis
  variable. Display and explain the published sensitivity order 1, 2, 3A, 3B,
  4 and the separate resistance order R1, R2. Show these seven canonical rows
  even when their count is zero, and append every other exact non-empty string
  as a separate row. Preserve the recorded OncoKB version and update date for
  every alteration. CHIP alterations are excluded from actionability summaries.
- The alteration-count table reports evidence direction, exact level, OncoKB
  category, position within direction, definition, recorded version(s), update
  range, mutation/CNV/fusion record counts, and total alteration-record count.
- The evidence-level patient table reports unique patients with at least one
  mutation, CNV, fusion, and any alteration at each exact level, together with
  the common denominator of patients with at least one valid ctDNA result and
  the any-alteration patient proportion. Type-specific patient counts may
  overlap and are not summed.
- A separate overall patient table reports any sensitivity evidence, any
  resistance evidence, their union, their intersection, and no actionable
  finding, with patient count, common valid-ctDNA denominator, and proportion.
- “No actionable finding” means no non-CHIP alteration has any recorded exact
  sensitivity/resistance evidence. Unknown, not-assessed, and missing values
  remain visible in a separate completeness/QC table with alteration-record and
  patient counts; they are not treated as evidence.

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

The analysis uses the REDCap participant-level field
`n_solid_biopsy_analyses`, defined in the data dictionary as the number of
unique days on which tissue was sampled. Its public name is
`unique_biopsy_days`. It is analyzed with the prespecified exposure offset;
missing values remain missing. No separate biopsy-event export is required.

## MTB

MTB registration uses `mtb_reg`; MTB discussion success uses numeric `n_mtb >=
1`. The distribution of discussion counts and missingness are reported. Other
referral fields are not used as substitutes.
