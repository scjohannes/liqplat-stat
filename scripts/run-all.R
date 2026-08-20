#!/usr/bin/env Rscript

# Explicit ordered Quarto-stage runner.  This intentionally has no target
# graph: stage order and paths come only from config/stages.yml.

script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- script_args[grepl("^--file=", script_args)]
script_file <- if (length(script_file_arg) == 0L) "" else
  sub("^--file=", "", script_file_arg[[1L]])
script_root <- if (nzchar(script_file)) {
  normalizePath(dirname(script_file), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "scripts"), winslash = "/", mustWork = FALSE)
}
project_root <- normalizePath(file.path(script_root, ".."), winslash = "/", mustWork = FALSE)
config_file <- file.path(project_root, "config", "stages.yml")
required_r_version <- "4.6.1"

runner_usage <- function() {
  cat(paste(
    "Usage: Rscript scripts/run-all.R [options]",
    "",
    "Options:",
    "  --from VALUE   First stage index or stage id (default: 1)",
    "  --to VALUE     Last stage index or stage id (default: final stage)",
    "  --force        Re-render selected stages and set LIQPLAT_FORCE=1",
    "  --help         Show this help",
    "",
    "The LIQPLAT_FORCE=1 environment variable has the same effect as --force.",
    sep = "\n"
  ), "\n")
}

parse_runner_args <- function(args) {
  result <- list(from = NULL, to = NULL, force = identical(Sys.getenv("LIQPLAT_FORCE"), "1"))
  index <- 1L
  while (index <= length(args)) {
    arg <- args[[index]]
    if (arg %in% c("--help", "-h")) {
      runner_usage()
      quit(save = "no", status = 0L)
    }
    if (arg == "--force") {
      result$force <- TRUE
      index <- index + 1L
      next
    }
    option <- sub("=.*$", "", arg)
    if (option %in% c("--from", "--to")) {
      value <- sub("^[^=]+=", "", arg)
      if (identical(value, arg)) {
        index <- index + 1L
        if (index > length(args)) stop("Missing value for ", option)
        value <- args[[index]]
      }
      if (!nzchar(value)) stop("Empty value for ", option)
      result[[sub("^--", "", option)]] <- value
      index <- index + 1L
      next
    }
    stop("Unknown option: ", arg)
  }
  result
}

load_stage_registry <- function(path = config_file) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.")
  if (!file.exists(path)) stop("Stage registry does not exist: ", path)
  registry <- yaml::read_yaml(path)
  stages <- registry$stages
  if (!is.list(stages) || length(stages) == 0L) stop("Stage registry is empty.")
  ids <- vapply(stages, function(x) as.character(x$id), character(1))
  numbers <- vapply(stages, function(x) as.integer(x$number), integer(1))
  paths <- vapply(stages, function(x) as.character(x$path), character(1))
  if (anyDuplicated(ids) || anyDuplicated(numbers) || anyDuplicated(paths)) {
    stop("Stage registry ids, numbers, and paths must be unique.")
  }
  if (!identical(numbers, sort(numbers))) stop("Stage registry must be numerically ordered.")
  if (any(!grepl("\\.qmd$", paths, ignore.case = TRUE))) stop("Every stage path must be a .qmd file.")
  stages
}

resolve_stage_index <- function(value, stages, option_name) {
  if (is.null(value)) return(if (option_name == "from") 1L else length(stages))
  as_index <- suppressWarnings(as.integer(value))
  if (!is.na(as_index) && identical(as.character(as_index), as.character(value))) {
    if (as_index < 1L || as_index > length(stages)) {
      stop(option_name, " index must be between 1 and ", length(stages), ".")
    }
    return(as_index)
  }
  ids <- vapply(stages, function(x) as.character(x$id), character(1))
  match_index <- match(value, ids)
  if (is.na(match_index)) stop("Unknown ", option_name, " stage: ", value)
  match_index
}

render_stage <- function(stage, force = FALSE) {
  stage_path <- file.path(project_root, stage$path)
  stage_path <- normalizePath(stage_path, winslash = "/", mustWork = FALSE)
  if (!file.exists(stage_path)) stop("Stage file does not exist: ", stage_path)
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto is required to render stage: ", stage$id)
  old_force_exists <- nzchar(Sys.getenv("LIQPLAT_FORCE", unset = ""))
  old_force <- Sys.getenv("LIQPLAT_FORCE", unset = "")
  on.exit({
    if (old_force_exists) Sys.setenv(LIQPLAT_FORCE = old_force)
    else Sys.unsetenv("LIQPLAT_FORCE")
  }, add = TRUE)
  if (isTRUE(force)) Sys.setenv(LIQPLAT_FORCE = "1")
  status <- system2(quarto, args = c("render", stage_path), stdout = "", stderr = "")
  if (!identical(status, 0L)) stop("Quarto render failed for stage ", stage$id, " (status ", status, ").")
  invisible(TRUE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!identical(as.character(getRversion()), required_r_version)) {
    stop("LIQPLAT requires exactly R ", required_r_version,
         "; found ", as.character(getRversion()), ".")
  }
  options <- parse_runner_args(args)
  stages <- load_stage_registry()
  first <- resolve_stage_index(options$from, stages, "--from")
  last <- resolve_stage_index(options$to, stages, "--to")
  if (first > last) stop("--from must not be after --to.")
  for (index in seq.int(first, last)) {
    stage <- stages[[index]]
    message("Rendering stage ", index, "/", length(stages), ": ", stage$id)
    render_stage(stage, force = options$force)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv())) main()
