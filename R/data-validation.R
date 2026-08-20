# Schema-aware validation for pseudonymized Parquet interfaces.

read_data_schema <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read data schemas.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  schema <- yaml::read_yaml(path)
  validate_schema_document(schema, path = path)
  schema
}

schema_columns <- function(schema) {
  columns <- schema$columns
  if (is.null(columns) || !is.list(columns) || is.null(names(columns))) {
    stop("Schema must contain a named `columns` mapping.")
  }
  columns
}

validate_schema_document <- function(schema, path = "schema") {
  if (!is.list(schema)) stop("Schema must be a YAML mapping: ", path)
  if (is.null(schema$name) || is.null(schema$primary_key)) {
    stop("Schema requires `name` and `primary_key`: ", path)
  }
  columns <- schema_columns(schema)
  for (column_name in names(columns)) {
    column <- columns[[column_name]]
    if (!is.list(column) || is.null(column$type) ||
        is.null(column$required) || is.null(column$private)) {
      stop("Column ", column_name,
           " must specify type, required, and private in ", path)
    }
    if (!is.logical(column$required) || length(column$required) != 1L ||
        is.na(column$required) || !is.logical(column$private) ||
        length(column$private) != 1L || is.na(column$private)) {
      stop("Column ", column_name, " has invalid required/private flags in ", path)
    }
    if (is.null(column$unit) || is.null(column$coding)) {
      stop("Column ", column_name, " must specify unit and coding in ", path)
    }
  }
  primary_key <- as.character(schema$primary_key)
  missing_keys <- setdiff(primary_key, names(columns))
  if (length(missing_keys) > 0L) {
    stop("Primary key columns missing from schema: ", paste(missing_keys, collapse = ", "))
  }
  invisible(schema)
}

schema_type_ok <- function(x, type) {
  type <- tolower(as.character(type))
  switch(
    type,
    string = is.character(x),
    character = is.character(x),
    integer = is.integer(x) || (is.numeric(x) && all(is.na(x) | x == floor(x))),
    number = is.numeric(x),
    numeric = is.numeric(x),
    logical = is.logical(x),
    date = inherits(x, "Date"),
    datetime = inherits(x, c("POSIXct", "POSIXlt")),
    factor = is.factor(x) || is.character(x),
    list = is.list(x),
    FALSE
  )
}

validate_data_schema <- function(data, schema, check_missing = TRUE,
                                  check_primary_key = TRUE) {
  if (!is.data.frame(data)) stop("Data must be a data frame.")
  if (is.character(schema) && length(schema) == 1L) schema <- read_data_schema(schema)
  validate_schema_document(schema)
  columns <- schema_columns(schema)
  required <- names(columns)[vapply(columns, function(x) isTRUE(x$required), logical(1))]
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop("Required data columns are missing: ", paste(missing_columns, collapse = ", "))
  }
  for (column_name in intersect(names(columns), names(data))) {
    specification <- columns[[column_name]]
    if (!schema_type_ok(data[[column_name]], specification$type)) {
      stop("Column ", column_name, " does not have schema type ", specification$type)
    }
    if (isTRUE(check_missing) && isTRUE(specification$required) &&
        anyNA(data[[column_name]])) {
      stop("Required column contains missing values: ", column_name)
    }
  }
  if (isTRUE(check_primary_key)) {
    key <- as.character(schema$primary_key)
    if (any(!stats::complete.cases(data[, key, drop = FALSE]))) {
      stop("Primary key contains missing values.")
    }
    if (anyDuplicated(data[, key, drop = FALSE])) {
      stop("Primary key is not unique: ", paste(key, collapse = ", "))
    }
  }
  invisible(data)
}

assert_pseudonymized_columns <- function(data, schema) {
  columns <- schema_columns(if (is.character(schema)) read_data_schema(schema) else schema)
  sensitive <- names(columns)[vapply(columns, function(x) isTRUE(x$private), logical(1))]
  present <- intersect(sensitive, names(data))
  if (length(present) > 0L) {
    stop("Private columns cannot cross the public analysis interface: ",
         paste(present, collapse = ", "))
  }
  assert_no_phi_columns(data)
  invisible(data)
}

# Names on this deny-list are never accepted in a public Parquet, including as
# extra columns not present in the domain schema.  The list is intentionally
# conservative about direct identifiers and free text while allowing the
# approved pseudonymous `id` and `sample_id` fields.
default_phi_denylist <- function() {
  c(
    "record_id", "pat_id", "patient_id", "hospital_id", "medical_record",
    "mrn", "case_id", "study_id_linkage", "linkage_key", "reidentification",
    "patient_name", "pat_name", "first_name", "last_name", "firstname",
    "lastname", "date_of_birth", "dob", "birth_date", "email", "phone",
    "telephone", "mobile", "address", "street", "postcode", "zip_code",
    "free_text", "freetext", "clinical_note", "clinical_notes", "notes",
    "comment", "comments", "source_report", "raw_report", "token"
  )
}

phi_denylist_match <- function(column_names, denylist = default_phi_denylist()) {
  normalized <- tolower(gsub("[^a-z0-9]+", "_", as.character(column_names)))
  exact <- normalized %in% tolower(denylist)
  pattern <- grepl(
    "(^|_)(patient|pat)_(name|firstname|lastname|dob|birth|id)$|(^|_)(mrn|medical_record|hospital_id|record_id|linkage_key|reidentification|email|phone|telephone|mobile|address|free_text|freetext|clinical_note|clinical_notes|source_report|raw_report|token)(_|$)",
    normalized,
    perl = TRUE
  )
  exact | pattern
}

assert_no_phi_columns <- function(data, denylist = default_phi_denylist()) {
  if (!is.data.frame(data)) stop("Data must be a data frame.")
  present <- names(data)[phi_denylist_match(names(data), denylist)]
  if (length(present) > 0L) {
    stop("PHI/direct-identifier columns cannot cross the public analysis interface: ",
         paste(present, collapse = ", "))
  }
  invisible(data)
}

read_data_manifest <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read the data manifest.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  manifest <- yaml::read_yaml(path)
  if (!is.list(manifest)) stop("Data manifest must be a YAML mapping: ", path)
  manifest
}

validate_data_manifest <- function(manifest, expected_lock = "2026-08-21",
                                   private_dir = NULL) {
  if (!is.list(manifest)) stop("Data manifest must be a YAML mapping.")
  lock <- manifest$data_lock
  if (is.null(lock)) lock <- manifest$lock_date
  if (is.null(lock) && is.list(manifest$analysis)) lock <- manifest$analysis$data_lock
  if (!identical(as.character(lock), as.character(expected_lock))) {
    stop("Data manifest lock does not match the configured lock: ", expected_lock)
  }
  domains <- manifest$domains
  if (!is.list(domains)) stop("Data manifest must contain a `domains` mapping.")
  if (!is.null(private_dir)) {
    private_dir <- normalizePath(private_dir, winslash = "/", mustWork = FALSE)
    for (domain_name in names(domains)) {
      record <- domains[[domain_name]]
      if (!is.list(record)) stop("Manifest domain record is invalid: ", domain_name)
      file_name <- record$file
      if (is.null(file_name)) file_name <- paste0(domain_name, ".parquet")
      file_name <- as.character(file_name)
      file_path <- if (grepl("^[A-Za-z]:|^[/\\\\]", file_name)) {
        file_name
      } else if (startsWith(gsub("\\\\", "/", file_name),
                            paste0(gsub("\\\\", "/", private_dir), "/"))) {
        file_name
      } else {
        file.path(private_dir, file_name)
      }
      if (isTRUE(record$available) && !file.exists(file_path)) {
        stop("Manifest marks domain as available but the file is missing: ", file_path)
      }
    }
  }
  invisible(manifest)
}

validate_private_parquet <- function(path, schema_path, denylist = default_phi_denylist()) {
  schema <- read_data_schema(schema_path)
  data <- read_pseudonymized_parquet(path, schema = schema)
  assert_no_phi_columns(data, denylist = denylist)
  list(data = data, schema = schema)
}

read_pseudonymized_parquet <- function(path, schema = NULL, require_schema = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to read Parquet data.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!identical(tolower(tools::file_ext(path)), "parquet")) {
    stop("The public data interface accepts Parquet files only.")
  }
  if (isTRUE(require_schema) && is.null(schema)) {
    stop("A schema is required for public Parquet input.")
  }
  data <- arrow::read_parquet(path)
  if (!is.null(schema)) {
    validate_data_schema(data, schema)
    assert_pseudonymized_columns(data, schema)
  }
  data
}
