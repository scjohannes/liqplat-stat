# Statistical Analysis Plan

`SAP.qmd` is the current LIQPLAT Statistical Analysis Plan (version 1.2).
The rendered, trackable document is `SAP-v1.2.pdf`; `archive/` preserves the
version 1.0 and 1.1 PDFs. Figures used by the SAP are kept in
`figures/`, and historical analysis notebooks are preserved under
`development/`.

## Render the current SAP

From the repository root, use Quarto directly with the SAP directory as the
project context. This avoids walking unrelated project resources while still
rendering the source at `sap/SAP.qmd`:

```powershell
$env:QUARTO_PROJECT_DIR = (Resolve-Path sap).Path
$env:PATH = 'C:\Program Files\R\R-4.6.1\bin\x64;' + $env:PATH
$env:QUARTO_R = 'C:\Program Files\R\R-4.6.1\bin'
$env:R_HOME = 'C:\Program Files\R\R-4.6.1'
$env:LC_COLLATE = 'C'
$env:LC_CTYPE = 'English_United States.utf8'
quarto render sap/SAP.qmd --to typst --no-clean
```

Do not pass `--no-execute`: it removes the operating-characteristics tables. Do
not set `LC_ALL=C`. On Windows the last step of the render can fail with "path
must not contain a backslash". In that case, change `"\../chicago` to
`"../chicago` in the generated `sap/SAP.typ` and compile it directly:

```powershell
quarto typst compile sap/SAP.typ sap/SAP-v1.2.pdf --root .
```

Check that the tables are present, then keep the file as `sap/SAP-v1.2.pdf`. Do
not overwrite the PDFs in `archive/`.

The clean clone contains public source and figures but no trial data or model
objects. SAP code chunks are therefore non-evaluated during rendering, except the
operating-characteristics tables, which read `artifacts/`; the
historical notebooks in `development/` are reference sources and are not
implicitly rendered by the project.
