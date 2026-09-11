# Configuration and production provenance checks for the LIQPLAT pipeline.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

analysis_root <- function() {
  if (requireNamespace("here", quietly = TRUE)) {
    return(here::here())
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

analysis_config_path <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(analysis_root(), "config", "analysis.yml")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

read_analysis_config <- function(path = NULL, production = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read the analysis configuration.")
  }
  path <- analysis_config_path(path)
  if (!file.exists(path)) {
    stop("Analysis configuration does not exist: ", path)
  }
  config <- yaml::read_yaml(path)
  validate_analysis_config(config, production = production)
  attr(config, "config_path") <- path
  class(config) <- c("liqplat_analysis_config", class(config))
  config
}

config_get <- function(config, ..., default = NULL) {
  keys <- list(...)
  value <- config
  for (key in keys) {
    if (!is.list(value) || is.null(value[[key]])) return(default)
    value <- value[[key]]
  }
  value
}

validate_analysis_config <- function(config, production = FALSE) {
  if (!is.list(config)) stop("Analysis configuration must be a YAML mapping.")

  required_sections <- c("analysis", "imputation", "markov", "seeds",
                        "diagnostics", "provenance", "paths")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0L) {
    stop("Analysis configuration is missing: ",
         paste(missing_sections, collapse = ", "))
  }

  if (!identical(as.character(config_get(config, "analysis", "data_lock")),
                 "2026-09-05")) {
    stop("The data lock must be exactly 2026-09-05.")
  }
  if (!identical(as.integer(config_get(config, "analysis", "horizon_days")),
                 182L)) {
    stop("The primary horizon must be exactly 182 days.")
  }

  main_m <- as.integer(config_get(config, "imputation", "main", "m"))
  main_maxit <- as.integer(config_get(config, "imputation", "main", "maxit"))
  supporting <- as.integer(config_get(config, "imputation", "supporting", "indices"))
  supporting_draws <- as.integer(config_get(
    config,
    "imputation", "supporting", "post_estimation_draws"
  ))
  if (!identical(main_m, 50L) || !identical(main_maxit, 50L)) {
    stop("Main imputation settings must be m = 50 and maxit = 50.")
  }
  if (!identical(supporting, 1:5)) {
    stop("Supporting imputations must be exactly 1:5.")
  }
  if (!identical(supporting_draws, 500L)) {
    stop("Five-imputation supporting analyses must use 500 posterior draws per imputation.")
  }

  markov <- config$markov
  expected_markov <- c(chains = 4L, iterations = 1000L, warmup = 500L,
                       retry_iterations = 2000L,
                       post_estimation_draws = 200L)
  actual_markov <- vapply(names(expected_markov), function(name) {
    as.integer(markov[[name]])
  }, integer(1))
  if (!identical(actual_markov, expected_markov)) {
    stop("Markov settings must be 4 chains, 1000 iterations, 500 warmup, ",
         "2000 retry iterations, and 200 post-estimation draws.")
  }

  thresholds <- config$diagnostics
  expected_thresholds <- list(max_rhat = 1.01, min_ess_bulk = 400,
                              min_ess_tail = 400,
                              max_divergent_transitions = 0,
                              max_treedepth_exceeded = 0,
                              min_bfmi = 0.3)
  for (name in names(expected_thresholds)) {
    if (!isTRUE(all.equal(as.numeric(thresholds[[name]]),
                          as.numeric(expected_thresholds[[name]])))) {
      stop("Diagnostic threshold '", name, "' is not approved.")
    }
  }
  if (!identical(as.integer(thresholds$posterior_draws), 200L)) {
    stop("Diagnostic posterior draws must be exactly 200.")
  }

  provenance <- config_get(config, "provenance", "markov_misc")
  if (is.null(provenance) ||
      !identical(as.character(provenance$package), "markov.misc")) {
    stop("markov.misc provenance is missing or names the wrong package.")
  }
  sha <- as.character(provenance$git_sha %||% "")
  version <- as.character(provenance$version %||% "")
  valid_sha <- grepl("^[0-9a-fA-F]{40}$", sha)
  valid_version <- nzchar(version) &&
    !grepl("REPLACE|REQUIRED|INVALID|FUTURE|0\\.0\\.0", version,
           ignore.case = TRUE)
  if (isTRUE(production) && (!valid_sha || !valid_version)) {
    stop("Production preflight requires the reviewed clean markov.misc ",
         "version and 40-hex Git SHA; the configured values are placeholders.")
  }
  invisible(config)
}

analysis_seed <- function(config, family, imputation = NULL, offset = 0L) {
  base_seed <- config_get(config, "seeds", family)
  if (is.null(base_seed) || length(base_seed) != 1L ||
      !is.finite(as.numeric(base_seed))) {
    stop("Unknown or invalid deterministic seed family: ", family)
  }
  if (!is.null(imputation)) {
    imputation <- as.integer(imputation)
    if (is.na(imputation) || imputation < 1L) {
      stop("`imputation` must be a positive integer.")
    }
  } else {
    imputation <- 1L
  }
  seed <- as.numeric(base_seed) + (imputation - 1) * 1000 + as.numeric(offset)
  if (!is.finite(seed) || seed < 1 || seed > .Machine$integer.max) {
    stop("Derived analysis seed is outside R's valid integer range.")
  }
  as.integer(seed)
}

package_provenance <- function(package = "markov.misc") {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(list(package = package, installed = FALSE, version = NA_character_,
                library = NA_character_, git_sha = NA_character_))
  }
  description <- utils::packageDescription(package)
  library_path <- dirname(find.package(package, quiet = TRUE)[1L])
  list(
    package = package,
    installed = TRUE,
    version = as.character(utils::packageVersion(package)),
    library = normalizePath(library_path, winslash = "/", mustWork = FALSE),
    git_sha = NA_character_,
    source = description$RemoteType %||% description$Repository %||% NA_character_
  )
}

validate_package_provenance <- function(config, package = "markov.misc") {
  expected <- config_get(config, "provenance", "markov_misc")
  observed <- package_provenance(package)
  if (!isTRUE(observed$installed)) {
    stop("Required package is not installed: ", package)
  }
  if (!identical(as.character(observed$version), as.character(expected$version))) {
    stop("Installed ", package, " version ", observed$version,
         " does not match the reviewed version ", expected$version, ".")
  }
  observed
}

analysis_preflight <- function(config = read_analysis_config(),
                               production = TRUE,
                               check_packages = TRUE) {
  validate_analysis_config(config, production = production)
  if (isTRUE(check_packages)) validate_package_provenance(config)
  invisible(TRUE)
}
