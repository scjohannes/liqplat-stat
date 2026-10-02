# Time alive and out of hospital (TAOOH) data checks and death carry-forward.
#
# Packages the calling notebook must attach: tidyverse (dplyr, tidyr, purrr).


# Check the weekly TAOOH states against the unrestricted survival endpoints:
# valid keys and states (1-5), no rows after a patient's final (possibly
# partial) week, death (state 5) exactly in the death week of every patient
# who died, and a row for every patient's final week. Exported durations
# already incorporate the unshifted data lock, so shifted cohort dates are
# never compared with the calendar lock here. Returns `taooh` invisibly.
validate_taooh_follow_up <- function(taooh, cohort) {
  if (!all(c("id", "week", "y_taooh") %in% names(taooh)) ||
      !all(c(
        "id",
        "survival_time_days_unrestricted",
        "status_death_unrestricted"
      ) %in% names(cohort))) {
    stop("TAOOH validation requires weekly states and unrestricted endpoints.")
  }
  if (anyNA(cohort$id) || anyDuplicated(cohort$id) || anyNA(taooh$id) ||
      anyDuplicated(taooh[c("id", "week")])) {
    stop("Invalid TAOOH/cohort keys.")
  }

  days <- cohort$survival_time_days_unrestricted
  death <- cohort$status_death_unrestricted
  if (anyNA(days) || any(!is.finite(days) | days < 0) ||
      anyNA(death) || any(!death %in% 0:1)) {
    stop("Invalid unrestricted endpoints.")
  }
  if (anyNA(taooh$week) ||
      any(!is.finite(taooh$week) | taooh$week != floor(taooh$week)) ||
      any(!is.na(taooh$y_taooh) & !taooh$y_taooh %in% 1:5)) {
    stop("Invalid TAOOH week or state.")
  }

  patient <- match(taooh$id, cohort$id)
  if (anyNA(patient)) stop("TAOOH contains an unknown patient.")

  # Final (possibly partial) week of follow-up; day 0 counts as week 1.
  end_week <- pmax(1L, ceiling(days / 7))
  if (any(taooh$week > end_week[patient])) {
    stop("TAOOH rows occur after the patient-specific endpoint.")
  }

  is_death <- !is.na(taooh$y_taooh) & taooh$y_taooh == 5L
  if (any(is_death & (death[patient] != 1L | taooh$week != end_week[patient]))) {
    stop("TAOOH death state must occur in the unrestricted death week only.")
  }
  if (!setequal(taooh$id[is_death], cohort$id[death == 1L])) {
    stop("TAOOH must include state 5 in every patient's death week.")
  }

  observed_end <- paste(taooh$id, taooh$week)
  expected_end <- paste(cohort$id, end_week)
  if (any(!expected_end[days > 0 | death == 1L] %in% observed_end)) {
    stop("TAOOH must extend through each patient's final, possibly partial week.")
  }

  invisible(taooh)
}


# Weekly states up to the horizon, with death (state 5) carried forward to
# every later week up to the horizon. Censored and unobserved living states
# stay unobserved; this expanded object is for summaries of state occupancy
# and is never used for transition-model fitting.
taooh_carry_death_forward <- function(taooh, horizon_days = 182) {
  if (length(horizon_days) != 1L || !is.numeric(horizon_days) ||
      is.na(horizon_days) || !is.finite(horizon_days) ||
      horizon_days < 7 || horizon_days %% 7 != 0) {
    stop(
      "TAOOH horizon_days must be a positive whole number of weeks ",
      "(e.g. 182 or 364)."
    )
  }
  weeks <- as.integer(horizon_days / 7)

  observed <- taooh |>
    select(id, week, y_taooh) |>
    filter(week <= weeks)
  if (!any(observed$y_taooh == 5L, na.rm = TRUE)) return(observed)

  deaths <- observed |>
    filter(y_taooh == 5L) |>
    group_by(id) |>
    summarise(death_week = min(week), .groups = "drop") |>
    filter(death_week < weeks)

  carried <- deaths |>
    mutate(week = map(death_week, function(w) seq.int(w + 1L, weeks))) |>
    unnest(week) |>
    transmute(id, week, y_taooh = 5L) |>
    anti_join(observed, by = c("id", "week"))

  bind_rows(observed, carried) |>
    arrange(id, week)
}
