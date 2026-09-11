# First dated event or death on the privacy-shifted date scale.
derive_dated_event_endpoints <- function(cohort, event_date_col, event_indicator_col = NULL,
                                         event_label = "event", data_lock = "2026-09-05") {
  if (!identical(as.character(data_lock), "2026-09-05")) {
    stop("Follow-up must use the 2026-09-05 export lock.")
  }
  required <- c(
    "id", "tx", "randomization_date", event_date_col,
    "survival_time_days_unrestricted", "status_death_unrestricted",
    "potential_follow_up_days"
  )
  if (!is.data.frame(cohort) || !all(required %in% names(cohort))) {
    stop("Endpoint cohort is missing required endpoint columns.")
  }
  if (anyNA(cohort$id) || anyDuplicated(cohort$id)) {
    stop("Endpoint cohort must have one non-missing ID per participant.")
  }
  out <- as.data.frame(cohort)
  out$tx <- validate_binary(out$tx, "tx", allow_missing = FALSE)
  status <- validate_binary(
    out$status_death_unrestricted, "status_death_unrestricted", allow_missing = FALSE
  )
  for (column in c("survival_time_days_unrestricted", "potential_follow_up_days")) {
    if (!is.numeric(out[[column]]) || anyNA(out[[column]]) ||
        any(!is.finite(out[[column]]) | out[[column]] < 0)) {
      stop(column, " must contain finite non-negative follow-up durations.")
    }
  }
  randomization <- parse_analysis_date(
    out$randomization_date, "randomization_date", allow_missing = FALSE
  )
  out$origin_date <- randomization
  out[[event_date_col]] <- parse_analysis_date(out[[event_date_col]], event_date_col)
  event_date <- out[[event_date_col]]
  if (!is.null(event_indicator_col) && event_indicator_col %in% names(out) &&
      any(out[[event_indicator_col]] == 1 & is.na(event_date), na.rm = TRUE)) {
    stop("A recorded ", event_label, " event has no ", event_label, " date; resolve it before analysis.")
  }

  # Durations already encode the calendar lock in the private export.
  # potential_follow_up_days is capped at death for participants who died.
  out$follow_up_limit <- randomization + out$potential_follow_up_days
  observed_end <- randomization + pmin(
    out$survival_time_days_unrestricted, out$potential_follow_up_days
  )
  death_observed <- status == 1 &
    out$survival_time_days_unrestricted <= out$potential_follow_up_days
  death_end <- observed_end
  death_end[!death_observed] <- as.Date(NA)
  event_observed <- !is.na(event_date) &
    event_date <= out$follow_up_limit &
    (is.na(death_end) | event_date <= death_end)

  # A dated event is itself evidence of follow-up, even when the separate
  # last-known-alive field ends earlier. It cannot extend past the lock.
  out$extends_recorded_follow_up <- event_observed & event_date > observed_end
  out$extends_recorded_follow_up[is.na(out$extends_recorded_follow_up)] <- FALSE
  out$after_follow_up_limit <- !is.na(event_date) &
    event_date > out$follow_up_limit
  out$event_or_censor_date <- observed_end
  out$event_or_censor_date[event_observed] <- event_date[event_observed]
  out$event_first <- as.integer(event_observed)
  out$event_death_before_first <- as.integer(death_observed & !event_observed)
  out$event_composite <- out$event_first + out$event_death_before_first
  out$censor_reason <- ifelse(
    event_observed, "event_observed",
    ifelse(death_observed, "death_before_event", "end_of_observed_follow_up")
  )
  out$time_days <- as.numeric(out$event_or_censor_date - out$origin_date)
  out$exclusion_reason <- dplyr::case_when(
    event_observed & event_date < out$origin_date ~ "event_before_randomization",
    out$time_days <= 0 ~ "no_positive_follow_up",
    TRUE ~ "included"
  )
  out$in_analysis <- out$exclusion_reason == "included"
  out$data_lock <- as.character(data_lock)
  out
}

# PFS uses only the composite of progression or death for modelling.
derive_pfs_endpoint <- function(cohort, data_lock = "2026-09-05") {
  out <- derive_dated_event_endpoints(cohort, "progression_date", "progression", "progression", data_lock)
  names(out)[match(c("origin_date", "follow_up_limit", "extends_recorded_follow_up",
    "after_follow_up_limit", "event_or_censor_date", "event_first", "event_death_before_first",
    "event_composite", "censor_reason", "time_days", "exclusion_reason", "in_analysis"), names(out))] <-
    c("pfs_origin_date", "pfs_follow_up_limit", "progression_extends_recorded_follow_up",
      "progression_after_follow_up_limit", "pfs_event_or_censor_date", "event_progression",
      "event_death_before_progression", "event_pfs", "pfs_censor_reason", "pfs_time_days",
      "pfs_exclusion_reason", "pfs_in_analysis")
  out$pfs_censor_reason[out$pfs_censor_reason == "event_observed"] <- "progression_observed"
  out$pfs_censor_reason[out$pfs_censor_reason == "death_before_event"] <- "death_before_progression"
  out$pfs_exclusion_reason[out$pfs_exclusion_reason == "event_before_randomization"] <- "progression_before_randomization"
  out
}
