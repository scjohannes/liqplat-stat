# Common source helpers for the public LIQPLAT modules. These helpers never
# read raw REDCap data; they only resolve the pseudonymized Parquet contract.

liqplat_source_public_helpers <- function() {
  root <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
  helper_paths <- file.path(root, "R", c(
    "analysis-config.R", "paths-artifacts.R", "checkpoints.R",
    "data-helpers.R", "data-validation.R", "model-helpers.R",
    "actionability.R"
  ))
  missing <- helper_paths[!file.exists(helper_paths)]
  if (length(missing) > 0L) stop("Public helper files are missing: ", paste(missing, collapse = ", "))
  invisible(lapply(helper_paths, source, local = FALSE))
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

module_force <- function() identical(Sys.getenv("LIQPLAT_FORCE", unset = ""), "1")

module_result_dir <- function(module_path) {
  ensure_directory(project_path(module_path, "results"))
}

module_checkpoint_path <- function(stage_id) {
  ensure_directory(project_path("artifacts", "checkpoints")) |>
    file.path(paste0(stage_id, ".yml"))
}

private_domain_path <- function(config, domain) {
  private_dir <- config_get(config, "paths", "private_data", default = "data/private")
  project_path(private_dir, paste0(domain, ".parquet"))
}

schema_domain_path <- function(config, domain) {
  schema_dir <- config_get(config, "paths", "schema_dir", default = "data/schema")
  project_path(schema_dir, paste0(domain, ".yml"))
}

private_manifest_candidates <- function(config) {
  private_dir <- config_get(config, "paths", "private_data", default = "data/private")
  file.path(
    project_path(private_dir),
    c("manifest.yml", "MANIFEST.yml", "data-manifest.yml", "data_manifest.yml")
  )
}

read_private_manifest_if_present <- function(config) {
  candidates <- private_manifest_candidates(config)
  manifest_path <- candidates[file.exists(candidates)][1L]
  if (is.na(manifest_path)) return(NULL)
  list(path = manifest_path, manifest = read_data_manifest(manifest_path))
}

load_private_domain <- function(config, domain, required = FALSE) {
  path <- private_domain_path(config, domain)
  schema_path <- schema_domain_path(config, domain)
  if (!file.exists(path)) {
    if (isTRUE(required)) stop("Required private Parquet is missing for domain '", domain,
                              "': ", path)
    return(NULL)
  }
  if (!file.exists(schema_path)) {
    stop("Schema is missing for private domain '", domain, "': ", schema_path)
  }
  validated <- validate_private_parquet(path, schema_path)
  validated$data
}

require_public_columns <- function(data, required, context = "analysis") {
  if (!is.data.frame(data)) stop(context, " requires a data frame.")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(context, " is missing required canonical columns: ", paste(missing, collapse = ", "))
  }
  invisible(data)
}

coalesce_checked <- function(left, right, column_name) {
  both <- !is.na(left) & !is.na(right)
  if (any(both & as.character(left) != as.character(right))) {
    stop("Conflicting values for canonical column: ", column_name)
  }
  out <- left
  out[is.na(out)] <- right[is.na(out)]
  out
}

collapse_recruitment_domain <- function(recruitment) {
  require_public_columns(recruitment, c("id", "screened_date", "eligible"), "recruitment")
  recruitment$screened_date <- parse_analysis_date(recruitment$screened_date, "screened_date",
                                                   allow_missing = FALSE)
  recruitment$eligible <- validate_binary(recruitment$eligible, "eligible", allow_missing = FALSE)
  if ("randomization_date" %in% names(recruitment)) {
    recruitment$randomization_date <- parse_analysis_date(recruitment$randomization_date,
                                                           "randomization_date")
  }
  if ("enrolled_date" %in% names(recruitment)) {
    recruitment$enrolled_date <- parse_analysis_date(recruitment$enrolled_date, "enrolled_date")
  }
  if ("tx" %in% names(recruitment)) recruitment$tx <- validate_binary(recruitment$tx, "tx")
  groups <- split(seq_len(nrow(recruitment)), recruitment$id, drop = TRUE)
  rows <- lapply(groups, function(index) {
    part <- recruitment[index, , drop = FALSE]
    result <- data.frame(
      id = as.character(part$id[[1L]]),
      screened_date = min(part$screened_date, na.rm = TRUE),
      eligible = as.integer(max(part$eligible, na.rm = TRUE)),
      recruitment_records = nrow(part),
      stringsAsFactors = FALSE
    )
    if ("randomization_date" %in% names(part)) {
      values <- part$randomization_date[!is.na(part$randomization_date)]
      result$randomization_date <- if (length(values) == 0L) as.Date(NA) else min(values)
    }
    if ("enrolled_date" %in% names(part)) {
      values <- part$enrolled_date[!is.na(part$enrolled_date)]
      result$enrolled_date <- if (length(values) == 0L) as.Date(NA) else min(values)
    }
    if ("tx" %in% names(part)) {
      values <- unique(part$tx[!is.na(part$tx)])
      if (length(values) > 1L) stop("Conflicting recruitment assignment for patient.")
      result$tx <- if (length(values) == 0L) NA_integer_ else as.integer(values[[1L]])
    }
    result
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

merge_cohort_and_recruitment <- function(cohort, recruitment) {
  require_public_columns(cohort, c("id", "randomization_date", "tx", "diagnosis"),
                         "cohort_follow_up")
  cohort$randomization_date <- parse_analysis_date(cohort$randomization_date,
                                                   "randomization_date", allow_missing = FALSE)
  cohort$tx <- validate_binary(cohort$tx, "tx", allow_missing = FALSE)
  if (is.null(recruitment)) return(cohort)
  recruitment <- collapse_recruitment_domain(recruitment)
  if (anyDuplicated(cohort$id)) stop("cohort_follow_up must contain one row per pseudonymized patient.")
  joined <- merge(cohort, recruitment, by = "id", all = TRUE, suffixes = c("", ".recruitment"))
  for (column_name in intersect(c("randomization_date", "tx"), names(recruitment))) {
    recruitment_name <- paste0(column_name, ".recruitment")
    if (recruitment_name %in% names(joined)) {
      joined[[column_name]] <- coalesce_checked(joined[[column_name]],
                                                joined[[recruitment_name]], column_name)
      joined[[recruitment_name]] <- NULL
    }
  }
  joined
}

derive_valid_ctdna_result <- function(samples, error_col = "ctdna_error_derived") {
  require_public_columns(samples, c("sample_id", "id", error_col), "ctDNA samples")
  error <- validate_binary(samples[[error_col]], error_col)
  samples$valid_ctdna_result <- !is.na(error) & error == 0
  samples
}

write_module_table <- function(data, path) {
  if (!is.data.frame(data)) stop("Module output must be a data frame.")
  assert_artifact_path(path)
  write_atomic_parquet(data, path)
}

write_module_draws <- function(draws, path) {
  draws <- as.data.frame(draws, stringsAsFactors = FALSE)
  write_module_table(draws, path)
}

finalize_module_checkpoint <- function(stage_id, checkpoint_path, input_paths,
                                       output_paths, config, metadata = list(),
                                       code_paths = character()) {
  finalize_checkpoint(
    manifest_path = checkpoint_path,
    stage_id = stage_id,
    input_paths = input_paths,
    output_paths = output_paths,
    code_paths = code_paths,
    config = config,
    metadata = metadata
  )
}

safe_calendar_difference <- function(start, end, start_name = "sample_date",
                                     end_name = "report_date") {
  start <- parse_analysis_date(start, start_name)
  end <- parse_analysis_date(end, end_name)
  out <- as.numeric(end - start)
  out[!is.na(out) & out < 0] <- NA_real_
  out
}
