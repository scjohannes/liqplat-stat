# Statistical Analysis Plan

`SAP.qmd` is the current LIQPLAT Statistical Analysis Plan (version 1.1).
The rendered, trackable document is `SAP-v1.1.pdf`; `archive/SAP-v1.0.pdf` is
the preserved version 1.0 archive. Figures used by the SAP are kept in
`figures/`, and historical analysis notebooks are preserved under
`development/`.

## Render the current SAP

From the repository root, use Quarto directly with the SAP directory as the
project context. This avoids walking unrelated project resources while still
rendering the source at `sap/SAP.qmd`:

```powershell
$env:QUARTO_PROJECT_DIR = (Resolve-Path sap).Path
$env:R_HOME = 'C:\Program Files\R\R-4.6.1'
$env:LC_ALL = 'C'
quarto render sap/SAP.qmd --to typst --no-clean --no-execute
Copy-Item _site/sap/SAP.pdf sap/SAP-v1.1.pdf -Force
```

The document sets its own Typst layout and resolves the bibliography and CSL
file relative to `sap/SAP.qmd`. The render writes `_site/sap/SAP.pdf`; copy the
verified file to `sap/SAP-v1.1.pdf`. Do not overwrite
`archive/SAP-v1.0.pdf`.

The clean clone contains public source and figures but no trial data or model
objects. SAP code chunks are therefore non-evaluated during rendering; the
historical notebooks in `development/` are reference sources and are not
implicitly rendered by the project.
