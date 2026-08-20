# Standalone testthat entry point for this analysis project (which is not an
# installed R package).  The project root is passed explicitly so tests are
# independent of the caller's working directory.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run the LIQPLAT test suite.")
}

test_file <- commandArgs(trailingOnly = FALSE)
test_file_arg <- test_file[grepl("^--file=", test_file)]
test_file <- if (length(test_file_arg) == 0L) "" else
  sub("^--file=", "", test_file_arg[[1L]])
project_root <- if (nzchar(test_file)) {
  normalizePath(file.path(dirname(test_file), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
Sys.setenv(LIQPLAT_PROJECT_ROOT = project_root)

testthat::test_dir(
  file.path(project_root, "tests", "testthat"),
  reporter = "progress",
  stop_on_failure = TRUE
)
