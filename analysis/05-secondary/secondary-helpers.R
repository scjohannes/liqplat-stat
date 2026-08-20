# Shared contracts for secondary outcomes.
#
# These helpers are deliberately data-and-contract only.  They do not read
# private data, write model objects, or invent a zero when an event field is
# missing.  Model wrappers in the endpoint modules call the shared Bayesian
# fitting helpers only when a production stage explicitly invokes them.

secondary_require_columns <- function(data, columns, object_name = "data") {
  if (!is.data.frame(data)) stop(object_name, " must be a data frame.")
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(object_name, " is missing required columns: ",
         paste(missing_columns, collapse = ", "))
  }
  invisible(data)
}

secondary_parse_date <- function(x, field_name = "date", allow_missing = TRUE) {
  if (exists("parse_analysis_date", mode = "function")) {
    return(parse_analysis_date(x, field_name = field_name,
                              allow_missing = allow_missing))
  }
  if (inherits(x, "Date")) out <- x
  else if (inherits(x, c("POSIXct", "POSIXlt"))) out <- as.Date(x)
  else if (is.character(x)) out <- as.Date(x, format = "%Y-%m-%d")
  else stop(field_name, " must be Date, POSIXct, POSIXlt, or ISO character data.")
  if (!isTRUE(allow_missing) && anyNA(out)) {
    stop(field_name, " contains missing or invalid dates.")
  }
  out
}

secondary_lock_date <- function(lock_date = as.Date("2026-08-21")) {
  parsed <- secondary_parse_date(lock_date, "lock_date", allow_missing = FALSE)
  if (length(parsed) != 1L) stop("lock_date must contain one date.")
  parsed
}

secondary_date_column <- function(data, column, field_name = column,
                                  allow_missing = TRUE) {
  if (!column %in% names(data)) return(rep(as.Date(NA), nrow(data)))
  secondary_parse_date(data[[column]], field_name, allow_missing = allow_missing)
}

secondary_first_date <- function(values) {
  values <- secondary_parse_date(values, "event_date")
  if (length(values) == 0L || all(is.na(values))) return(as.Date(NA))
  min(values, na.rm = TRUE)
}

secondary_observed_follow_up <- function(data, origin_col = "randomization_date",
                                         death_date_col = "death_date",
                                         follow_up_end_col = "follow_up_end_date",
                                         lock_date = as.Date("2026-08-21"),
                                         horizon_days = NULL,
                                         output_col = "observed_follow_up_days") {
  secondary_require_columns(data, origin_col, "cohort")
  origin <- secondary_date_column(data, origin_col, origin_col,
                                  allow_missing = FALSE)
  death <- secondary_date_column(data, death_date_col, death_date_col)
  follow_up_end <- secondary_date_column(data, follow_up_end_col,
                                         follow_up_end_col)
  lock_date <- secondary_lock_date(lock_date)
  follow_up_end[is.na(follow_up_end)] <- lock_date
  end_date <- pmin(follow_up_end, lock_date)
  event_end <- !is.na(death) & death >= origin & death <= end_date
  end_date[event_end] <- death[event_end]
  if (!is.null(horizon_days)) {
    horizon_days <- as.integer(horizon_days)
    if (length(horizon_days) != 1L || is.na(horizon_days) || horizon_days < 1L) {
      stop("horizon_days must be one positive integer.")
    }
    end_date <- pmin(end_date, origin + horizon_days)
  }
  if (any(end_date < origin, na.rm = TRUE)) {
    stop("Observed follow-up ends before the analysis origin.")
  }
  out <- data
  out[[output_col]] <- as.numeric(end_date - origin)
  out$observed_end_date <- end_date
  out$observed_death <- as.integer(event_end)
  out
}

secondary_collapse_ecog <- function(x, output_levels = c("0", "1", "2", "3+")) {
  values <- suppressWarnings(as.numeric(as.character(x)))
  invalid <- !is.na(values) | is.na(values)
  invalid <- !is.na(values) & (values < 0 | values > 4 | values != floor(values))
  if (any(invalid)) stop("ECOG values must be integer scores from 0 to 4.")
  collapsed <- ifelse(is.na(values), NA_character_,
                      ifelse(values >= 3, "3+", as.character(values)))
  factor(collapsed, levels = output_levels, ordered = TRUE)
}

secondary_count_ready <- function(data, count_col, exposure_col) {
  secondary_require_columns(data, c(count_col, exposure_col), "count data")
  count <- suppressWarnings(as.numeric(data[[count_col]]))
  exposure <- suppressWarnings(as.numeric(data[[exposure_col]]))
  if (anyNA(count)) {
    stop(count_col, " contains missing values; missing is not zero and cannot be fitted.")
  }
  if (any(!is.finite(count) | count < 0 | count != floor(count))) {
    stop(count_col, " must contain non-negative integer counts.")
  }
  if (anyNA(exposure) || any(!is.finite(exposure) | exposure <= 0)) {
    stop(exposure_col, " must contain finite, strictly positive observed exposure.")
  }
  invisible(data)
}

secondary_event_rows <- function(events, id_col = "id", date_col,
                                 event_id_col = NULL, label = "events") {
  secondary_require_columns(events, c(id_col, date_col), label)
  if (!is.null(event_id_col)) {
    secondary_require_columns(events, event_id_col, label)
    if (anyNA(events[[event_id_col]]) || anyDuplicated(events[[event_id_col]])) {
      stop(label, " has a missing or duplicated event identifier.")
    }
  }
  out <- events[, c(id_col, date_col), drop = FALSE]
  names(out) <- c("id", "event_date")
  out$event_date <- secondary_parse_date(out$event_date, date_col)
  out <- out[!is.na(out$event_date), , drop = FALSE]
  unique(out)
}

secondary_join_count <- function(cohort, event_counts, count_col,
                                 count_missing_col = NULL) {
  secondary_require_columns(cohort, "id", "cohort")
  secondary_require_columns(event_counts, c("id", count_col), "event counts")
  if (anyDuplicated(cohort$id)) stop("Cohort id must be unique.")
  if (anyDuplicated(event_counts$id)) stop("Event counts id must be unique.")
  out <- cohort
  index <- match(out$id, event_counts$id)
  out[[count_col]] <- event_counts[[count_col]][index]
  out[[count_col]][is.na(index)] <- 0L
  if (!is.null(count_missing_col) && count_missing_col %in% names(event_counts)) {
    out[[count_missing_col]] <- event_counts[[count_missing_col]][index]
    out[[count_missing_col]][is.na(index)] <- FALSE
  }
  out
}

secondary_count_events <- function(events, id_col = "id", event_id_col,
                                   event_date_col, count_col = "event_count",
                                   unique_days = FALSE, label = "events") {
  secondary_require_columns(events, c(id_col, event_id_col, event_date_col), label)
  if (anyNA(events[[event_id_col]]) || anyDuplicated(events[[event_id_col]])) {
    stop(label, " must contain a unique non-missing event identifier.")
  }
  event_date <- secondary_parse_date(events[[event_date_col]], event_date_col,
                                     allow_missing = FALSE)
  event_ids <- as.character(events[[id_col]])
  if (isTRUE(unique_days)) {
    keys <- paste(event_ids, event_date, sep = "\r")
    keep <- !duplicated(keys)
    event_ids <- event_ids[keep]
  }
  counts <- table(event_ids)
  out <- data.frame(id = names(counts), stringsAsFactors = FALSE)
  out[[count_col]] <- as.integer(counts)
  out
}

secondary_model_fit_args <- function(fit_args = list(), default = list()) {
  if (!is.list(fit_args) || !is.list(default)) stop("Model arguments must be lists.")
  c(default, fit_args[setdiff(names(fit_args), names(default))])
}
