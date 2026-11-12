# LIQPLAT statistical analysis

LIQPLAT is a random invitation trial at the University Hospital Basel. Patients
from a prospective research registry were randomly selected for an invitation to
ctDNA-guided care; patients not selected received usual care. This repository
contains the Statistical Analysis Plan (SAP) and the analysis code. It contains
no clinical data.

## Structure

```
sap/                 Statistical Analysis Plan (SAP.qmd) and its released PDFs
config/analysis.yml  shared settings: imputations, posterior draws, MCMC, seeds,
                     diagnostic thresholds
R/                   the few helper functions used by several notebooks;
                     plot-style.R is the one plot style for notebooks and report
scripts/run-all.R    renders all analysis notebooks in dependency order
analysis/
  00-data/                       validation and analysis datasets
  01-population/                 recruitment, CONSORT flow, baseline table
  02-overall-survival/           one subfolder per way OS was analysed
  03-quality-of-life/            analysis decision, preparation, landmark and
                                 longitudinal models
  04-taooh/                      time alive and out of hospital
  05-progression-free-survival/
  06-best-supportive-care/
  07-blood-products/
  08-tissue-biopsy/
  09-imaging/
  10-implementation/             invitation, ctDNA sampling, technical validity,
                                 baseline detection, turnaround, MTB, CHIP,
                                 actionability
  90-pending/                    SAP outcomes without analysis code yet
```

Each analysis folder holds numbered Quarto notebooks (the numbers give the run
order) and writes everything it produces to its own `results/` folder.

## Data

The pseudonymised data exports go in `data/private/`. The whole `data/` folder,
all `results/` folders and the report in `reports/` are ignored by git and must
never be committed.

## Requirements

- R 4.6.1 with Rtools45, Quarto 1.10.19 and CmdStan 2.39.0 (via cmdstanr)
- R packages: pinned with [renv](https://rstudio.github.io/renv/) in
  `renv.lock` (CRAN, the Stan R-universe for rstanarm, posterior and cmdstanr,
  and GitHub for [`mostr`](https://github.com/scjohannes/mostr))

### R packages (renv)

The project `.Rprofile` activates renv, so every R session started in the
repository, including the ones Quarto starts for the notebooks, uses the project
library in `renv/library/` instead of the user library. On a new machine,
install the locked versions once:

```r
renv::restore()
```

To add or update a package, install it in the project
(`renv::install("pkg")`), use it in the code, and record it with
`renv::snapshot()`; commit `renv.lock` together with the code that needs it.
`renv::status()` shows whether the library and the lockfile agree. renv finds
the packages in the code; `.renvignore` excludes data, renders and the
historical notebooks in `sap/development/`, and `DESCRIPTION` lists the
packages that the code uses only indirectly (svglite for SVG figures).

rstanarm is a development build (branch `survival-rstantools`, commit
`5f9a4e43`) from the Stan R-universe. If R-universe no longer serves that
version, install it from GitHub with
`renv::install("stan-dev/rstanarm@5f9a4e43a3f57c2661d5943e475fb02bbde99e02")`
(a long source build). CmdStan itself is not managed by renv.

## Running the analyses

From the repository root, with R 4.6.1:

```bash
Rscript scripts/run-all.R
```

`--from <path>` starts at the first notebook under a path and `--only <path>`
renders only the notebooks under it, e.g.
`Rscript scripts/run-all.R --from analysis/04-taooh`.

Imputations and model fits are saved per imputation in each analysis's
`results/` folder and reused on the next run; everything else is recomputed.
Delete an analysis's `results/` folder to recompute it from scratch, for
example after the data change.

The three main analyses (overall survival, the six-month QoL landmark and TAOOH)
currently use 5 imputations. To use 50, set `imputation: m_main: 50` in
`config/analysis.yml` and run again from `analysis/02-overall-survival`:
imputations 6 to 50 are added and imputations 1 to 5 are reused. The QoL
analysis decision depends on the OS result and is re-evaluated in that run.

## Report and SAP

```bash
quarto render reports
```

```bash
quarto render sap/SAP.qmd
```

The report quotes the SAP verbatim; its render stops if a quotation does not
match `sap/SAP.qmd`.
