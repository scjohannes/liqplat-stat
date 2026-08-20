# Content-addressed checkpoint manifests used to skip only reproducible stages.

sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for SHA-256 checkpoints.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (dir.exists(path)) stop("SHA-256 file hashing does not accept directories.")
  unname(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}

sha256_object <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for SHA-256 checkpoints.")
  }
  digest::digest(object, algo = "sha256", serialize = TRUE)
}

hash_path <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) {
    return(NA_character_)
  }
  if (!dir.exists(path)) return(sha256_file(path))
  files <- list.files(path, all.files = FALSE, full.names = TRUE,
                      recursive = TRUE, include.dirs = FALSE)
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))
  if (length(files) == 0L) return(sha256_object(character()))
  relative <- substring(files, nchar(normalizePath(path, winslash = "/", mustWork = TRUE)) + 2L)
  sha256_object(stats::setNames(vapply(files, sha256_file, character(1)), relative))
}

hash_paths <- function(paths) {
  paths <- as.character(paths)
  if (length(paths) == 0L) return(character())
  values <- vapply(paths, hash_path, character(1))
  stats::setNames(values, normalizePath(paths, winslash = "/", mustWork = FALSE))
}

checkpoint_manifest <- function(stage_id, input_paths = character(),
                                output_paths = character(), code_paths = character(),
                                config = NULL, metadata = list()) {
  if (length(stage_id) != 1L || !nzchar(stage_id)) stop("`stage_id` must be non-empty.")
  hash_records <- function(values) {
    if (length(values) == 0L) return(list())
    lapply(names(values), function(path) {
      list(path = path, sha256 = unname(values[[path]]))
    })
  }
  manifest <- list(
    schema_version = 1L,
    stage_id = as.character(stage_id),
    inputs = hash_records(hash_paths(input_paths)),
    outputs = hash_records(hash_paths(output_paths)),
    code = hash_records(hash_paths(code_paths)),
    config_sha256 = if (is.null(config)) NA_character_ else sha256_object(config),
    metadata = metadata
  )
  manifest
}

write_checkpoint_manifest <- function(manifest, path, overwrite = TRUE) {
  if (!is.list(manifest) || is.null(manifest$stage_id)) {
    stop("A checkpoint manifest must include `stage_id`.")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for checkpoint manifests.")
  }
  write_atomic_text(yaml::as.yaml(manifest), path, overwrite = overwrite)
}

read_checkpoint_manifest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for checkpoint manifests.")
  }
  out <- yaml::read_yaml(path)
  if (!is.list(out) || is.null(out$stage_id)) stop("Invalid checkpoint manifest: ", path)
  out
}

same_named_hashes <- function(expected, observed) {
  canonical <- function(value) {
    if (is.null(value) || length(value) == 0L) return(character())
    if (is.list(value)) {
      valid <- vapply(value, function(item) {
        is.list(item) && !is.null(item$path) && !is.null(item$sha256)
      }, logical(1))
      if (!all(valid)) return(character())
      out <- vapply(value, function(item) as.character(item$sha256), character(1))
      names(out) <- vapply(value, function(item) as.character(item$path), character(1))
      return(out)
    }
    as.character(value)
  }
  expected <- canonical(expected)
  observed <- canonical(observed)
  if (!identical(sort(names(expected)), sort(names(observed)))) return(FALSE)
  identical(unname(expected[sort(names(expected))]),
            unname(observed[sort(names(observed))]))
}

checkpoint_is_current <- function(manifest_path, stage_id, input_paths = character(),
                                  output_paths = character(), code_paths = character(),
                                  config = NULL) {
  if (!file.exists(manifest_path)) return(FALSE)
  manifest <- tryCatch(read_checkpoint_manifest(manifest_path), error = function(e) NULL)
  if (is.null(manifest) || !identical(as.character(manifest$stage_id), stage_id)) {
    return(FALSE)
  }
  expected <- checkpoint_manifest(
    stage_id = stage_id,
    input_paths = input_paths,
    output_paths = output_paths,
    code_paths = code_paths,
    config = config
  )
  same_named_hashes(manifest$inputs %||% character(), expected$inputs %||% character()) &&
    same_named_hashes(manifest$outputs %||% character(), expected$outputs %||% character()) &&
    same_named_hashes(manifest$code %||% character(), expected$code %||% character()) &&
    identical(as.character(manifest$config_sha256), as.character(expected$config_sha256))
}

checkpoint_decision <- function(manifest_path, stage_id, input_paths = character(),
                                output_paths = character(), code_paths = character(),
                                config = NULL, force = FALSE) {
  if (isTRUE(force)) return("force")
  if (checkpoint_is_current(manifest_path, stage_id, input_paths, output_paths,
                            code_paths, config)) return("skip")
  if (file.exists(manifest_path)) return("resume")
  "run"
}

should_skip_stage <- function(...) identical(checkpoint_decision(...), "skip")

finalize_checkpoint <- function(manifest_path, stage_id, input_paths = character(),
                                output_paths = character(), code_paths = character(),
                                config = NULL, metadata = list(), overwrite = TRUE) {
  manifest <- checkpoint_manifest(stage_id, input_paths, output_paths, code_paths,
                                  config, metadata)
  write_checkpoint_manifest(manifest, manifest_path, overwrite = overwrite)
  invisible(manifest)
}
