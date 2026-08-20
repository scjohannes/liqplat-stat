# SAP artifacts

This directory is reserved for compact, non-confidential SAP artifacts that
are deliberately part of the public reproducibility record. It is not a
staging location for private trial data, model objects, caches, or generated
intermediate files.

To reproduce the current SAP PDF, render from the repository root with:

```powershell
$env:QUARTO_PROJECT_DIR = (Resolve-Path sap).Path
$env:R_HOME = 'C:\Program Files\R\R-4.6.1'
$env:LC_ALL = 'C'
quarto render sap/SAP.qmd --to typst --no-clean --no-execute
Copy-Item _site/sap/SAP.pdf sap/SAP-v1.1.pdf -Force
```

The render writes `_site/sap/SAP.pdf`; after verifying the output, place the
current document at `sap/SAP-v1.1.pdf`. Historical analysis code is retained under
`sap/development/` but remains non-evaluated when rendering `sap/SAP.qmd`.
