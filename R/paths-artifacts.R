# Paths and recoverable/atomic artifact writers.

project_path <- function(..., root = analysis_root()) {
  pieces <- list(...)
  if (length(pieces) == 0L) return(normalizePath(root, winslash = "/", mustWork = FALSE))
  if (any(vapply(pieces, function(x) length(x) != 1L || is.na(x), logical(1)))) {
    stop("Path components must be non-missing scalar strings.")
  }
  if (any(grepl("^[A-Za-z]:|^[/\\\\]", pieces))) {
    stop("Path components must be relative to the project root.")
  }
  out <- file.path(root, do.call(file.path, pieces))
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

analysis_paths <- function(config = read_analysis_config()) {
  path_names <- names(config$paths)
  paths <- lapply(config$paths, function(value) project_path(value))
  stats::setNames(paths, path_names)
}

ensure_directory <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create directory: ", path)
  }
  path
}

assert_artifact_path <- function(path, root = analysis_root()) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (!identical(path, root) && !startsWith(path, prefix)) {
    stop("Artifact path is outside the project root: ", path)
  }
  path
}

write_atomic <- function(path, writer, overwrite = TRUE) {
  if (!is.function(writer)) stop("`writer` must be a function taking one path.")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  ensure_directory(dirname(path))
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Artifact already exists and overwrite = FALSE: ", path)
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.exists(temporary)) stop("Artifact writer did not create: ", temporary)
  if (file.exists(path) && unlink(path, force = TRUE) != 0L) {
    stop("Could not replace existing artifact: ", path)
  }
  if (!file.rename(temporary, path)) stop("Could not move artifact into place: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_atomic_rds <- function(object, path, version = 3L, overwrite = TRUE) {
  write_atomic(path, function(destination) {
    saveRDS(object, destination, version = version)
  }, overwrite = overwrite)
}

write_atomic_parquet <- function(data, path, overwrite = TRUE, ...) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write Parquet artifacts.")
  }
  if (!is.data.frame(data)) stop("Parquet artifacts must be data frames.")
  write_atomic(path, function(destination) {
    arrow::write_parquet(data, sink = destination, ...)
  }, overwrite = overwrite)
}

write_atomic_text <- function(text, path, overwrite = TRUE, encoding = "UTF-8") {
  if (length(text) == 0L) text <- character()
  write_atomic(path, function(destination) {
    con <- file(destination, open = "wb")
    on.exit(close(con), add = TRUE)
    writeLines(as.character(text), con = con, useBytes = TRUE)
  }, overwrite = overwrite)
}

read_artifact <- function(path, format = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!file.exists(path)) stop("Artifact does not exist: ", path)
  format <- format %||% tolower(tools::file_ext(path))
  switch(
    format,
    rds = readRDS(path),
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required to read Parquet artifacts.")
      }
      arrow::read_parquet(path)
    },
    yaml = {
      if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("Package 'yaml' is required to read YAML artifacts.")
      }
      yaml::read_yaml(path)
    },
    txt = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    stop("Unsupported artifact format: ", format)
  )
}
