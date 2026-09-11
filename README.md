# LIQPLAT Statistical Analysis

This repository contains the public, reproducible analysis pipeline and
Statistical Analysis Plan (SAP) for LIQPLAT, a single-arm trial evaluating
implementation of ctDNA in routine cancer care with an external comparator.
It contains code and schemas only; it does not contain clinical data or
identifiers.

## Architecture

The pipeline is organized as numbered Quarto stages registered explicitly in
[`config/stages.yml`](config/stages.yml). Shared, testable behavior lives in
[`R/`](R/), constants in [`config/analysis.yml`](config/analysis.yml), and
machine-readable interfaces in [`data/schema/`](data/schema/). The runner is a
small ordered orchestrator, not a targets/drake graph: a stage is rendered only
when it is selected in the registry.

The primary endpoints are overall survival (RMST through 182 days),
longitudinal quality of life (first-order ordinal Markov model), and time alive
and out of hospital (second-order ordinal Markov model). Posterior draws are
pooled with equal weight across imputations. The production analysis uses
MICE `m = 50`, `maxit = 50`; supporting analyses intentionally use imputations
1:5 only and must not be described as the primary analysis.

## Privacy and data contract

The private preparation environment must export only pseudonymized Parquet
files conforming to the YAML schemas and locked at `2026-09-05`. See
[`data/README.md`](data/README.md) for the boundary. Raw extracts, source
linkage keys, direct identifiers, and free-text reports remain outside this
repository. The ignored `data/private/` directory is only a local hand-off
location; it is never a place to commit or archive data.

## Prerequisites

- R 4.6.1, Quarto, and a working Stan toolchain
- Packages specified by the project lockfile, including `yaml`, `here`,
  `arrow`, `mice`, `miceadds`, `rmsb`, `rstanarm`, `posterior`, `ggsurvfit`,
  and `digest`
- The reviewed clean `markov.misc` release exposing `blrm_markov()`,
  `avg_sops()`, and `sops()`

Restore the environment with `renv::restore()`. `renv.lock` is a valid skeleton
for the current R version; it intentionally does not invent package hashes.
Before a production run, update it and the `markov_misc` entries in
[`config/analysis.yml`](config/analysis.yml) with the reviewed clean package
version and its exact 40-hex Git SHA. Production preflight rejects the current
placeholders.

Check the local toolchain with:

```text
Rscript scripts/check-environment.R
```

Use `--strict` only after the clean package provenance has been recorded.

## Running and resuming stages

From the repository root, render all registered stages in order:

```text
Rscript scripts/run-all.R
```

Run a bounded range by one-based index or stage id:

```text
Rscript scripts/run-all.R --from primary_qol_imputation --to primary_qol_estimand
```

`--force` (or `LIQPLAT_FORCE=1`) re-creates outputs that already exist.
Otherwise, expensive imputation and model loops keep completed branch files
and continue with the first missing branch. A failed stage stops the run.

Run the no-PHI structural/contract suite and report smoke test with the
project's pinned R 4.6.1 runtime:

```text
Rscript tests/testthat.R
Rscript scripts/synthetic-smoke-test.R
```

The smoke test renders the artifact-only report to both HTML and Typst without
fitting Stan models or reading `data/private/`.

## Reports and SAP

The default Quarto project supports the public report and SAP as resources but
does not implicitly render every notebook. Render either explicitly:

```text
quarto render reports
quarto render sap/SAP.qmd
```

Generated reports, caches, logs, private data, and model outputs are ignored by
policy. Public schemas, source QMD/R/YAML/R code, and intentionally compact
non-confidential Parquet/RDS artifacts remain eligible for version control.
