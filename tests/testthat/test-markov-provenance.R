test_that("Markov package equivalence is explicitly deferred until reviewed provenance exists", {
  config <- read_analysis_config(liqplat_path("config", "analysis.yml"))
  provenance <- config$provenance$markov_misc
  valid_sha <- grepl("^[0-9a-fA-F]{40}$", as.character(provenance$git_sha %||% ""))
  valid_version <- nzchar(as.character(provenance$version %||% "")) &&
    !grepl("REPLACE|REQUIRED|INVALID|FUTURE|0\\.0\\.0", provenance$version,
           ignore.case = TRUE)
  if (!requireNamespace("markov.misc", quietly = TRUE) ||
      !valid_sha || !valid_version) {
    skip("Reviewed clean markov.misc version and exact 40-hex SHA are not installed.")
  }
  installed_version <- as.character(utils::packageVersion("markov.misc"))
  if (!identical(installed_version, as.character(provenance$version))) {
    skip("Installed markov.misc does not match the reviewed lock provenance.")
  }
  skip("Equivalence fixtures are intentionally pending the reviewed clean package.")
})
