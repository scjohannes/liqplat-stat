# Secondary outcomes

These modules implement the public contracts for exploratory outcomes in the
SAP.  They are intentionally separate from the primary stage registry because
the current public checkout contains schemas and code only, not patient-level
data or model artifacts.

| Module | Primary contract |
| --- | --- |
| `01-pfs/` | Randomized ITT PFS from randomization; first progression/death; Bayesian PH; hazard ratio primary |
| `02-survival-rates/` | Six- and 12-month rates derived from OS posterior curves; later horizons require a sufficiency gate |
| `03-best-supportive-care/` | `bsc_date` for every participant; first-line treatment origin; death before BSC censored |
| `04-blood-products/` | Adjusted Bayesian negative-binomial rate model with observed-time offset |
| `05-tissue-biopsy/` | Unique biopsy days; treatment, linear age, ECOG 0/1/2/3+; survival-days offset |
| `06-imaging/` | Adjusted total imaging count and CT/MRI/PET-CT descriptive summaries |

The count modules require a complete, positive exposure and preserve the
distinction between missing and zero.  No module creates model objects or
results while being rendered as documentation; production stages must invoke
the functions explicitly and write compact artifacts under `results/`.

Unplanned admissions and emergency-room visits are not modeled as independent
secondary outcomes.  They are already contained in the TAOOH ordinal outcome.
