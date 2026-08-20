# Data interface

The public pipeline consumes pseudonymized Parquet files. Each file must be
validated against the corresponding YAML document in [`schema/`](schema/)
before an analysis stage reads it. Schemas define the primary key, type,
requiredness, unit/coding, and whether a field is sensitive or private.

Raw extracts, direct identifiers, source-system linkage tables, free-text
clinical notes, and any re-identification key stay outside this repository in
the private preparation environment. They must never be copied into
`data/private/`, committed, or written to a report artifact. The private
preparation contract exports only the approved pseudonymized columns and the
fixed data-lock metadata (`2026-08-21`).

Dates are ISO-8601 dates and are interpreted relative to the documented study
origin. Missing values are not silently converted to zero. Patient identifiers
are pseudonyms, not names, medical record numbers, or contact details.

`data/private/` is an ignored hand-off location for a local run. It is not a
backup and is not a substitute for the private preparation environment.

Variable-level caveats, pending fields, and operational definitions are kept
in [`VARIABLES.md`](VARIABLES.md). In particular, MSI, MMR, TMB, and the
treatment-line fields are pending until the private extraction contract
provides identifiable source fields and approved coding. The public pipeline
does not treat those domains as zero or silently omit them.
