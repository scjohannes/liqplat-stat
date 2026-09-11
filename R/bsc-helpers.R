# Retain the BSC endpoint contract while sharing date handling with PFS.
source(here::here("R", "event-survival-helpers.R"))

derive_bsc_endpoints <- function(cohort, data_lock = "2026-09-05") {
  out <- derive_dated_event_endpoints(cohort, "bsc_date", "bsc", "BSC", data_lock)
  names(out)[match(c("origin_date", "follow_up_limit", "extends_recorded_follow_up",
    "after_follow_up_limit", "event_or_censor_date", "event_first", "event_death_before_first",
    "event_composite", "censor_reason", "time_days", "exclusion_reason", "in_analysis"), names(out))] <-
    c("bsc_origin_date", "bsc_follow_up_limit", "bsc_extends_recorded_follow_up",
      "bsc_after_follow_up_limit", "bsc_event_or_censor_date", "event_bsc",
      "event_death_before_bsc", "event_bsc_or_death", "bsc_censor_reason", "time_to_bsc_days",
      "bsc_exclusion_reason", "bsc_in_analysis")
  out$bsc_censor_reason[out$bsc_censor_reason == "event_observed"] <- "bsc_observed"
  out$bsc_censor_reason[out$bsc_censor_reason == "death_before_event"] <- "death_before_bsc"
  out$bsc_exclusion_reason[out$bsc_exclusion_reason == "event_before_randomization"] <- "bsc_before_randomization"
  out
}
