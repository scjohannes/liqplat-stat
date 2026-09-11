# Fixed-lock censoring, date handling, exposure construction, and explicit
# missing-versus-zero semantics.

parse_analysis_date <- function(x, field_name = "date", allow_missing = TRUE) {
  if (inherits(x, "Date")) out <- x
  else if (inherits(x, c("POSIXct", "POSIXlt"))) out <- as.Date(x)
  else if (is.character(x)) {
    value <- trimws(x)
    value[value == ""] <- NA_character_
    out <- as.Date(rep(NA_character_, length(value)))
    formats <- c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d.%m.%Y")
    for (format in formats) {
      pending <- is.na(out) & !is.na(value)
      if (!any(pending)) break
      parsed <- as.Date(value[pending], format = format)
      out[pending] <- parsed
    }
  } else {
    stop(field_name, " must be Date, POSIXct, or an ISO-like character vector.")
  }
  if (!isTRUE(allow_missing) && anyNA(out)) {
    stop(field_name, " contains missing or invalid dates.")
  }
  out
}

invert_q30 <- function(x, strict = TRUE) {
  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  invalid <- !is.na(numeric_x) & !(numeric_x %in% 1:7)
  if (isTRUE(strict) && any(invalid)) {
    stop("Q30 values must be in the EORTC range 1 to 7.")
  }
  result <- 8 - numeric_x
  result[is.na(numeric_x) | invalid] <- NA_real_
  if (is.integer(x)) as.integer(result) else result
}

q30_to_analysis_state <- function(x, strict = TRUE) invert_q30(x, strict = strict)

validate_binary <- function(x, field_name = "indicator", allow_missing = TRUE) {
  values <- suppressWarnings(as.numeric(as.character(x)))
  invalid <- !is.na(values) & !(values %in% c(0, 1))
  if (any(invalid)) stop(field_name, " must be coded 0/1.")
  if (!isTRUE(allow_missing) && anyNA(values)) stop(field_name, " cannot be missing.")
  values
}

censor_at_lock <- function(data, origin_col, event_date_col,
                           follow_up_end_col = NULL,
                           lock_date = as.Date("2026-09-05"),
                           horizon_days = 182,
                           time_col = "follow_up_days",
                           event_col = "event") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  needed <- c(origin_col, event_date_col, follow_up_end_col)
  if (any(!is.na(needed) & !needed %in% names(data))) {
    stop("Missing censoring columns: ", paste(setdiff(needed, names(data)), collapse = ", "))
  }
  lock_date <- parse_analysis_date(lock_date, "lock_date", allow_missing = FALSE)
  if (length(lock_date) != 1L) stop("`lock_date` must be one date.")
  horizon_days <- as.integer(horizon_days)
  if (is.na(horizon_days) || horizon_days < 1L) stop("`horizon_days` must be positive.")
  out <- data
  origin <- parse_analysis_date(out[[origin_col]], origin_col, allow_missing = FALSE)
  event_date <- parse_analysis_date(out[[event_date_col]], event_date_col)
  if (any(!is.na(event_date) & event_date < origin)) {
    stop("Event date precedes the analysis origin.")
  }
  follow_up_end <- if (is.null(follow_up_end_col)) {
    rep(lock_date, nrow(out))
  } else {
    parse_analysis_date(out[[follow_up_end_col]], follow_up_end_col)
  }
  # A missing source end date does not create an unknown risk interval: the
  # fixed data lock is the administrative end in that case.
  follow_up_end[is.na(follow_up_end)] <- lock_date
  follow_up_end <- pmin(follow_up_end, lock_date)
  administrative_end <- pmin(origin + horizon_days, follow_up_end)
  bad_end <- !is.na(administrative_end) & administrative_end < origin
  if (any(bad_end)) stop("Follow-up end precedes the analysis origin.")
  event <- !is.na(event_date) & event_date <= administrative_end
  event_or_censor_date <- administrative_end
  event_or_censor_date[event] <- event_date[event]
  time_days <- as.numeric(event_or_censor_date - origin)
  out[[time_col]] <- time_days
  out[[event_col]] <- as.integer(event)
  out$censor_date <- administrative_end
  out$event_or_censor_date <- event_or_censor_date
  out
}

derive_survival_endpoint <- function(data, origin_col = "randomization_date",
                                     death_date_col = "death_date",
                                     follow_up_end_col = NULL,
                                     lock_date = as.Date("2026-09-05"),
                                     horizon_days = 182) {
  censor_at_lock(
    data = data,
    origin_col = origin_col,
    event_date_col = death_date_col,
    follow_up_end_col = follow_up_end_col,
    lock_date = lock_date,
    horizon_days = horizon_days,
    time_col = "survival_time_days",
    event_col = "event_death"
  )
}

construct_exposure <- function(data, assignment_col = "tx",
                               invitation_col = NULL,
                               exposure_col = "exposure") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  source_col <- if (!is.null(invitation_col)) invitation_col else assignment_col
  if (!source_col %in% names(data)) stop("Exposure column is missing: ", source_col)
  exposure <- validate_binary(data[[source_col]], source_col)
  out <- data
  out[[exposure_col]] <- as.integer(exposure)
  out
}

missing_zero_status <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  status <- rep("missing", length(numeric_x))
  status[!is.na(numeric_x) & numeric_x == 0] <- "zero"
  status[!is.na(numeric_x) & numeric_x != 0] <- "nonzero"
  status
}

assert_missing_not_zero <- function(x, field_name = "value") {
  status <- missing_zero_status(x)
  if (any(status == "missing")) {
    stop(field_name, " has missing values; missing is not equivalent to zero.")
  }
  invisible(x)
}

coalesce_missing_zero <- function(x, missing_is_zero = FALSE, field_name = "value") {
  if (!isTRUE(missing_is_zero) && anyNA(x)) {
    stop("Missing values in ", field_name,
         " cannot be converted to zero without an explicit rule.")
  }
  if (isTRUE(missing_is_zero)) {
    x[is.na(x)] <- 0
  }
  x
}

build_exposure_intervals <- function(data, id_col = "id", start_col,
                                     end_col, exposure_col) {
  required <- c(id_col, start_col, end_col, exposure_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("Missing exposure interval columns: ", paste(missing, collapse = ", "))
  out <- data
  out[[start_col]] <- parse_analysis_date(out[[start_col]], start_col, allow_missing = FALSE)
  out[[end_col]] <- parse_analysis_date(out[[end_col]], end_col, allow_missing = FALSE)
  if (any(out[[end_col]] < out[[start_col]])) stop("Exposure interval end precedes start.")
  validate_binary(out[[exposure_col]], exposure_col, allow_missing = FALSE)
  out
}
