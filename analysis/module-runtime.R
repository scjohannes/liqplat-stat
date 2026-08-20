# Runtime utilities shared by the numbered LIQPLAT analysis notebooks.
#
# This file deliberately contains no endpoint-specific transformations.  The
# notebooks source the project helpers themselves and use these functions for
# deterministic configuration, schema-gated inputs, and recoverable branches.

module_force <- function(force = NULL) {
  if (!is.null(force)) return(isTRUE(force))
  identical(Sys.getenv("LIQPLAT_FORCE"), "1") ||
    isTRUE(getOption("liqplat.force", FALSE))
}

module_runtime <- function(force = NULL, production = FALSE) {
  config <- read_analysis_config(production = production)
  paths <- analysis_paths(config)
  list(
    config = config,
    paths = paths,
    force = module_force(force),
    root = analysis_root()
  )
}

module_input <- function(path, schema_path = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    stop(
      "Required private/derived input is missing: ", path,
      ". Supply the locked, schema-conforming Parquet input before rendering."
    )
  }
  if (is.null(schema_path)) {
    return(read_artifact(path))
  }
  schema_path <- normalizePath(schema_path, winslash = "/", mustWork = FALSE)
  if (!file.exists(schema_path)) stop("Input schema is missing: ", schema_path)
  read_pseudonymized_parquet(path, schema = read_data_schema(schema_path))
}

module_require_columns <- function(data, columns, label = "analysis input") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(data)
}

module_assert_private_free <- function(data, schema, label = "analysis input") {
  tryCatch(
    assert_pseudonymized_columns(data, schema),
    error = function(error) stop(label, " failed the privacy boundary: ", error$message)
  )
  invisible(data)
}

module_checkpoint_path <- function(runtime, stage_id) {
  assert_artifact_path(project_path(runtime$paths$checkpoints, paste0(stage_id, ".yml")))
}

module_decision <- function(runtime, stage_id, input_paths, output_paths,
                            code_paths = character()) {
  checkpoint_decision(
    manifest_path = module_checkpoint_path(runtime, stage_id),
    stage_id = stage_id,
    input_paths = input_paths,
    output_paths = output_paths,
    code_paths = code_paths,
    config = runtime$config,
    force = runtime$force
  )
}

module_branch <- function(runtime, stage_id, input_paths, output_paths,
                          worker, code_paths = character(), metadata = list()) {
  if (!is.function(worker)) stop("A branch worker must be a function.")
  decision <- module_decision(runtime, stage_id, input_paths, output_paths, code_paths)
  if (identical(decision, "skip")) return(invisible(list(stage_id = stage_id, skipped = TRUE)))
  worker()
  missing_outputs <- output_paths[!file.exists(output_paths)]
  if (length(missing_outputs) > 0L) {
    stop("Branch ", stage_id, " did not create outputs: ", paste(missing_outputs, collapse = ", "))
  }
  finalize_checkpoint(
    manifest_path = module_checkpoint_path(runtime, stage_id),
    stage_id = stage_id,
    input_paths = input_paths,
    output_paths = output_paths,
    code_paths = code_paths,
    config = runtime$config,
    metadata = metadata
  )
  invisible(list(stage_id = stage_id, skipped = FALSE, decision = decision))
}

module_exact_indices <- function(config, supporting = FALSE) {
  if (isTRUE(supporting)) {
    indices <- as.integer(config$imputation$supporting$indices)
    if (!identical(indices, 1:5)) stop("Supporting branches must use imputations exactly 1:5.")
    return(indices)
  }
  m <- as.integer(config$imputation$main$m)
  if (is.na(m) || m != 50L) stop("Primary branches must use exactly 50 imputations.")
  seq_len(m)
}

module_read_parquet <- function(path) {
  if (!file.exists(path)) stop("Required Parquet artifact is missing: ", path)
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Package 'arrow' is required for Parquet artifacts.")
  arrow::read_parquet(path)
}

module_save_parquet <- function(data, path) {
  assert_artifact_path(path)
  write_atomic_parquet(data, path)
}

module_save_rds <- function(object, path) {
  assert_artifact_path(path)
  write_atomic_rds(object, path)
}

module_code_paths <- function(...) {
  paths <- c(...)
  paths[file.exists(paths)]
}
