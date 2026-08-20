# Shared loading for a source-oriented analysis repository.  The files in R/
# are intentionally sourced rather than installed as a package because the
# production QMDs call their endpoint code directly.

.liqplat_project_root <- Sys.getenv("LIQPLAT_PROJECT_ROOT", unset = "")
if (!nzchar(.liqplat_project_root)) {
  .liqplat_project_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    winslash = "/", mustWork = TRUE
  )
}
.liqplat_project_root <- normalizePath(.liqplat_project_root, winslash = "/", mustWork = TRUE)

.liqplat_source_files <- sort(list.files(
  file.path(.liqplat_project_root, "R"),
  pattern = "\\.R$",
  full.names = TRUE
))
for (.liqplat_file in .liqplat_source_files) {
  sys.source(.liqplat_file, envir = .GlobalEnv)
}

# Secondary contracts and the runtime's exact-imputation guard are kept next to
# their modules rather than in R/, but are inexpensive and safe to source for
# tests.
.liqplat_secondary_helpers <- file.path(
  .liqplat_project_root, "analysis", "05-secondary", "secondary-helpers.R"
)
if (file.exists(.liqplat_secondary_helpers)) {
  sys.source(.liqplat_secondary_helpers, envir = .GlobalEnv)
}
.liqplat_runtime <- file.path(.liqplat_project_root, "analysis", "module-runtime.R")
if (file.exists(.liqplat_runtime)) sys.source(.liqplat_runtime, envir = .GlobalEnv)

liqplat_path <- function(...) {
  file.path(.liqplat_project_root, ...)
}

liqplat_qmd_text <- function(relative_path) {
  path <- liqplat_path(relative_path)
  if (!file.exists(path)) stop("Missing project file: ", relative_path)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
