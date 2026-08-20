test_that("all R sources, scripts, and analysis QMD R chunks parse", {
  r_paths <- c(
    list.files(liqplat_path("R"), "\\.R$", full.names = TRUE),
    list.files(liqplat_path("scripts"), "\\.R$", full.names = TRUE)
  )
  for (path in r_paths) {
    expect_no_error(parse(file = path, keep.source = FALSE))
  }

  testthat::skip_if_not_installed("knitr")
  qmd_paths <- list.files(liqplat_path("analysis"), "\\.qmd$", recursive = TRUE,
                          full.names = TRUE)
  expect_gte(length(qmd_paths), 40L)
  for (path in qmd_paths) {
    extracted <- tempfile(fileext = ".R")
    on.exit(unlink(extracted), add = TRUE)
    expect_no_error(
      knitr::purl(path, output = extracted, documentation = 0,
                  quiet = TRUE),
    )
    expect_no_error(parse(file = extracted, keep.source = FALSE))
  }
})

test_that("primary QMDs call reviewed model APIs directly", {
  qmd_paths <- list.files(liqplat_path("analysis", "02-primary"), "\\.qmd$",
                          recursive = TRUE, full.names = TRUE)
  text <- paste(vapply(qmd_paths, function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_match(text, "markov\\.misc::blrm_markov")
  expect_match(text, "rstanarm::stan_surv")
  expect_false(grepl("fit_markov_model\\(", text))
  expect_false(grepl("fit_survival_model\\(", text))
})
