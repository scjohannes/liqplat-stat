source(here::here("R", "survival-helpers.R"))
source(here::here("R", "censoring-helpers.R"))
x <- data.frame(id = 1:4, tx = c(0, 1, 0, 1),
  survival_time_days_unrestricted = c(0, 10, 20, 30), status_death_unrestricted = c(0, 1, 0, 1),
  ecog_fstcnt = c(0, 2, NA, NA), stage_binary = c(0, 1, 0, 1), diagnosis = c("a", "a", "b", "b"),
  albumin = c(40, 20, NA, 40), c_reactive_protein = c(5, 20, 5, NA))
y <- prepare_censoring_data(x)
stopifnot(identical(y$data$censoring_event, c(1L, 0L, 1L, 0L)),
  identical(y$data$follow_up_days, x$survival_time_days_unrestricted),
  all(y$data$ecog_binary == c(0, 1, 0, 0)),
  all(as.integer(as.character(y$data$mgps)) == c(0, 2, 0, 0)),
  all(y$replacements$missing == 2), all(y$replacements$replacement == 0))
x$stage_binary[1] <- NA
stopifnot(inherits(try(prepare_censoring_data(x), silent = TRUE), "try-error"))
cat("Censoring recoding, pooled modal replacement, tie handling, and validation passed.\n")
