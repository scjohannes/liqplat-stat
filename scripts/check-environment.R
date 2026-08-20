#!/usr/bin/env Rscript

script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- script_args[grepl("^--file=", script_args)]
script_file <- if (length(script_file_arg) == 0L) "" else
  sub("^--file=", "", script_file_arg[[1L]])
project_root <- if (nzchar(script_file)) {
  normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
config_path <- file.path(project_root, "config", "analysis.yml")
required_r_version <- "4.6.1"
source(file.path(project_root, "R", "analysis-config.R"), local = FALSE)

environment_usage <- function() {
  cat(paste(
    "Usage: Rscript scripts/check-environment.R [--strict] [--help]",
    "",
    "  --strict  Require the reviewed clean markov.misc provenance.",
    "  --help    Show this help.",
    sep = "\n"
  ), "\n")
}

check_environment <- function(strict = FALSE) {
  if (!identical(as.character(getRversion()), required_r_version)) {
    stop("LIQPLAT requires exactly R ", required_r_version,
         "; found ", as.character(getRversion()), ".")
  }
  if (!file.exists(config_path)) stop("Missing analysis configuration: ", config_path)
  config <- read_analysis_config(config_path, production = FALSE)
  required <- c("yaml", "here", "digest")
  optional_analysis <- c("arrow", "mice", "miceadds", "rmsb", "rstanarm", "posterior")
  package_status <- stats::setNames(
    vapply(c(required, optional_analysis, "markov.misc"), requireNamespace,
           logical(1), quietly = TRUE),
    c(required, optional_analysis, "markov.misc")
  )
  quarto <- Sys.which("quarto")
  cat("R:", R.version.string, "\n")
  cat("Quarto:", if (nzchar(quarto)) quarto else "NOT FOUND", "\n")
  cat("Packages:\n")
  for (name in names(package_status)) cat("  ", name, ": ", package_status[[name]], "\n", sep = "")
  if (!nzchar(quarto)) stop("Quarto is required.")
  if (any(!package_status[required])) stop("Required environment packages are missing.")
  if (isTRUE(strict)) analysis_preflight(config, production = TRUE, check_packages = TRUE)
  invisible(package_status)
}

args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c("--help", "-h"))) {
  environment_usage()
  quit(save = "no", status = 0L)
}
unknown <- setdiff(args, "--strict")
if (length(unknown) > 0L) stop("Unknown option: ", unknown[[1L]])
check_environment(strict = "--strict" %in% args)
