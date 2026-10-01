source(here::here("R", "survival-helpers.R"))

# Gaps of 0, 1, 13 and 14 days assume survival to cutoff in both scenarios;
# a 15-day gap becomes a death only in the worst case.
# Also covers day-zero censoring/death and preservation of recorded deaths.
cohort <- data.frame(
  id = 1:10, tx = rep(c(0, 1), 5),
  survival_time_days_unrestricted = c(0, 7, 8, 14, 40, 0, 39, 26, 25, 27),
  status_death_unrestricted = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
  potential_follow_up_days = c(100, 100, 100, 14, 40, 0, 40, 40, 40, 40)
)
scenarios <- prepare_os_censoring_scenarios(cohort)
best <- scenarios[scenarios$scenario == "best-case", ]
worst <- scenarios[scenarios$scenario == "worst-case", ]
stopifnot(
  nrow(scenarios) == 2 * nrow(cohort),
  identical(best$id, worst$id), identical(best$tx, worst$tx),
  identical(best$assumed_alive_14_days,
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)),
  identical(best$assumed_alive_14_days, worst$assumed_alive_14_days),
  identical(best$survival_time_days, c(100, 100, 100, 14, 40, 0, 40, 40, 40, 40)),
  identical(best$event_death, cohort$status_death_unrestricted),
  identical(worst$survival_time_days, c(0, 7, 8, 14, 40, 0, 40, 40, 25, 40)),
  identical(worst$event_death, c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L)),
  identical(worst$fitting_time_days, c(0.5, 7, 8, 14, 40, 0, 40, 40, 25, 40)),
  all(worst$survival_time_days <= best$survival_time_days)
)
for (bad in list(
  transform(cohort, status_death_unrestricted = NA_integer_),
  transform(cohort, tx = 2),
  transform(cohort, potential_follow_up_days = 0),
  transform(cohort, survival_time_days_unrestricted = -1),
  transform(cohort, potential_follow_up_days = Inf),
  transform(cohort, id = 1)
)) {
  stopifnot(inherits(try(prepare_os_censoring_scenarios(bad), silent = TRUE), "try-error"))
}
cat("OS scenarios: survival to cutoff for gaps of 0-14 days, 15-day boundary, unchanged recorded deaths, day zero, and validation passed.\n")
