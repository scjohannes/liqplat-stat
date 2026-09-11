# Exported durations already incorporate the unshifted data lock. Never compare
# shifted cohort dates with the calendar lock in the public preparation.
validate_taooh_follow_up <- function(taooh, cohort) {
  if (!all(c("id", "week", "y_taooh") %in% names(taooh)) ||
      !all(c("id", "survival_time_days_unrestricted",
             "status_death_unrestricted") %in% names(cohort))) {
    stop("TAOOH validation requires weekly states and unrestricted endpoints.")
  }
  if (anyNA(cohort$id) || anyDuplicated(cohort$id) || anyNA(taooh$id) ||
      anyDuplicated(taooh[c("id", "week")])) stop("Invalid TAOOH/cohort keys.")
  days <- cohort$survival_time_days_unrestricted
  death <- cohort$status_death_unrestricted
  if (anyNA(days) || any(!is.finite(days) | days < 0) ||
      anyNA(death) || any(!death %in% 0:1)) stop("Invalid unrestricted endpoints.")
  if (anyNA(taooh$week) || any(!is.finite(taooh$week) |
      taooh$week != floor(taooh$week)) ||
      any(!is.na(taooh$y_taooh) & !taooh$y_taooh %in% 1:5)) {
    stop("Invalid TAOOH week or state.")
  }
  patient <- match(taooh$id, cohort$id)
  if (anyNA(patient)) stop("TAOOH contains an unknown patient.")
  end_week <- pmax(1L, ceiling(days / 7))
  if (any(taooh$week > end_week[patient])) {
    stop("TAOOH rows occur after the patient-specific endpoint.")
  }
  is_death <- !is.na(taooh$y_taooh) & taooh$y_taooh == 5L
  if (any(is_death & (death[patient] != 1L |
                     taooh$week != end_week[patient]))) {
    stop("TAOOH death state must occur in the unrestricted death week only.")
  }
  death_ids <- taooh$id[is_death]
  if (!setequal(death_ids, cohort$id[death == 1L])) {
    stop("TAOOH must include state 5 in every patient's death week.")
  }
  observed_end <- paste(taooh$id, taooh$week)
  expected_end <- paste(cohort$id, end_week)
  if (any(!expected_end[days > 0 | death == 1L] %in% observed_end)) {
    stop("TAOOH must extend through each patient's final, possibly partial week.")
  }
  invisible(taooh)
}

taooh_horizon_weeks <- function(horizon_days) {
  if (length(horizon_days) != 1L || !is.numeric(horizon_days) ||
      is.na(horizon_days) || !is.finite(horizon_days) ||
      horizon_days < 7 || horizon_days %% 7 != 0) {
    stop("TAOOH horizon_days must be a positive whole number of weeks (e.g. 182 or 364).")
  }
  as.integer(horizon_days / 7)
}

# Longer analyses reuse the primary workflow but cannot overwrite its artifacts.
taooh_path <- function(base, horizon_days, ...) {
  weeks <- taooh_horizon_weeks(horizon_days)
  here::here(base, if (weeks == 26L) "primary" else "supporting",
             if (weeks == 26L) "taooh" else paste0("taooh-", weeks, "-weeks"), ...)
}

taooh_outputs_current <- function(outputs, inputs) {
  all(file.exists(c(outputs, inputs))) &&
    min(file.info(outputs)$mtime) >= max(file.info(inputs)$mtime)
}

taooh_empirical_history <- function(taooh, horizon_days = 182) {
  weeks <- taooh_horizon_weeks(horizon_days)
  observed <- taooh |>
    dplyr::select(id, week, y_taooh) |>
    dplyr::filter(week <= weeks)
  if (!any(observed$y_taooh == 5L, na.rm = TRUE)) return(observed)
  # Only death is carried forward. Censored and unobserved living states stay
  # unobserved; this expanded object is never used for transition-model fitting.
  deaths <- observed |>
    dplyr::filter(y_taooh == 5L) |>
    dplyr::group_by(id) |>
    dplyr::summarise(death_week = min(week), .groups = "drop") |>
    dplyr::filter(death_week < weeks)
  carried <- deaths |>
    dplyr::mutate(week = lapply(death_week, function(w) seq.int(w + 1L, weeks))) |>
    tidyr::unnest(week) |>
    dplyr::transmute(id, week, y_taooh = 5L) |>
    dplyr::anti_join(observed, by = c("id", "week"))
  dplyr::bind_rows(observed, carried) |>
    dplyr::arrange(id, week)
}
