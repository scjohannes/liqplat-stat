test_that("TAOOH QMD preserves weekly precedence and absorbing death", {
  text <- liqplat_qmd_text("analysis/02-primary/03-taooh/01-preparation.qmd")
  expect_match(text, "observed <- observed\\[!duplicated\\(observed\\[c\\(\\\"id\\\", \\\"week\\\"\\)\\], fromLast = TRUE\\),")
  expect_match(text, "dplyr::lag\\(y, 1L, default = dplyr::first\\(y\\)\\)")
  expect_match(text, "dplyr::lag\\(y, 2L, default = dplyr::first\\(y\\)\\)")

  sentinel <- liqplat_qmd_text("analysis/02-primary/03-taooh/00-no-treatment-sentinel.qmd")
  expect_match(sentinel, "value == 5L")
  expect_match(sentinel, "death state 5 must be absorbing")
})

test_that("TAOOH history uses first-observation fallback and death remains absorbing", {
  observed <- data.frame(
    id = c("p01", "p01", "p01", "p01"),
    week = c(1L, 2L, 2L, 3L),
    y = c(1L, 2L, 3L, 5L)
  )
  observed <- observed[!duplicated(observed[c("id", "week")], fromLast = TRUE), , drop = FALSE]
  observed <- observed[order(observed$id, observed$week), , drop = FALSE]
  observed$yprev <- c(observed$y[[1L]], head(observed$y, -1L))
  observed$ypprev <- c(observed$y[[1L]], observed$y[[1L]], head(observed$y, -2L))
  expect_equal(observed$y, c(1L, 3L, 5L))
  expect_equal(observed$yprev, c(1L, 1L, 3L))
  expect_equal(observed$ypprev, c(1L, 1L, 1L))
  death <- which(observed$y == 5L)
  expect_true(length(death) == 1L && all(observed$y[death[[1L]]:nrow(observed)] == 5L))
})
