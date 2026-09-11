# TAOOH horizons

The shared input retains unrestricted weekly follow-up through each patient's
death or censoring endpoint. The primary stages default to 182 days (26 weeks).
QoL and blood-product counts remain on their 182-day horizon.

To run a 364-day (52-week) supporting analysis, render the same five stages in
order, passing the same parameter to each. Use any positive whole-week horizon
supported by the observed follow-up; the model assumptions still require review.

```powershell
$env:PATH = 'C:\Program Files\R\R-4.6.1\bin\x64;' + $env:PATH
$env:QUARTO_R = 'C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe'
$env:MAKEFLAGS = 'PATH=/x86_64-w64-mingw32.static.posix/bin:/usr/bin'
$env:LC_ALL = 'C'
quarto check knitr
# Verify the preceding output reports R 4.6.1 before rendering.
quarto render analysis/02-primary/03-taooh/01-preparation.qmd -P horizon_days:364
quarto render analysis/02-primary/03-taooh/02-imputation.qmd -P horizon_days:364
quarto render analysis/02-primary/03-taooh/03-fitting.qmd -P horizon_days:364
quarto render analysis/02-primary/03-taooh/04-estimand.qmd -P horizon_days:364
quarto render analysis/02-primary/03-taooh/05-diagnostics.qmd -P horizon_days:364
```

The 182-day artifacts remain under `artifacts/primary/taooh` and
`results/primary/taooh`. Longer runs use, for example,
`artifacts/supporting/taooh-52-weeks` and `results/supporting/taooh-52-weeks`.
The report's primary TAOOH section continues to read the primary artifacts.
Rendered stage documents are regenerated for the requested horizon.

Run input validation and dataset building again after replacing exports or
changing the lock. Rerun preparation and imputation before fitting. Primary
TAOOH model/estimand checkpoints are refreshed when their inputs are newer.
For other analyses with existing checkpoints, set `LIQPLAT_FORCE=1` when
regenerating results after the data update.

Empirical plots alone extend death through the plotting horizon. Living
participants disappear after censoring, including a partially observed final
week. Transition-model data retain the death week and exclude later rows.
The weekly outcome does not turn a partially observed week into seven observed
days; model-derived time-in-state summaries remain expressed in weeks.
