# Survival-model post-estimation and time-to-event endpoint derivation.
#
# Packages the calling notebook must attach: rstanarm, tidyverse (dplyr,
# purrr). rstanarm internals are called with `rstanarm:::`.


# rstanarm::posterior_survfit() for stan_surv fits, including hierarchical
# fits (random intercepts), which fail in the installed survival branch: it
# passes every factor's levels to the separate fixed/random model frames and
# still calls lme4's deprecated mkReTrms(). For hierarchical fits we run
# private copies of the three internal functions involved, with those two
# calls patched.
# ponytail: depends on survival-branch internals; remove when upstream fixes
# these calls.
posterior_survfit_compatible <- function(object, ...) {
  if (!inherits(object, "stansurv") || !isTRUE(object$has_bars)) {
    return(posterior_survfit(object, ...))
  }

  # Copies of the internals whose enclosing environment is `scope`, so they
  # find each other and the patched make_model_frame() defined below.
  scope <- new.env(parent = asNamespace("rstanarm"))
  for (name in c(
    "posterior_survfit.stansurv",
    ".pp_calculate_surv",
    ".pp_data_surv"
  )) {
    fun <- getFromNamespace(name, "rstanarm")
    environment(fun) <- scope
    assign(name, fun, envir = scope)
  }

  # Only pass the levels of factors that appear in the formula at hand.
  scope$make_model_frame <- function(formula, data, xlevs = NULL, ...) {
    rstanarm:::make_model_frame(
      formula,
      data,
      xlevs = xlevs[intersect(names(xlevs), all.vars(formula))],
      ...
    )
  }
  environment(scope$make_model_frame) <- scope

  # mkReTrms() now lives in reformulas.
  body(scope$.pp_data_surv) <- parse(text = gsub(
    "lme4::mkReTrms",
    "reformulas::mkReTrms",
    paste(deparse(body(scope$.pp_data_surv)), collapse = "\n"),
    fixed = TRUE
  ))[[1L]]

  scope$posterior_survfit.stansurv(object, ...)
}


# Posterior draws of the standardised (marginal) survival curve under
# treatment 0 and under treatment 1: everybody in `data` is set to the
# treatment value, the conditional curves are averaged over `data`, on
# `n_points` times from 0 to `horizon`. Returns a long data frame with columns
# draw, time, survival, tx (the tx = 0 block first).
posterior_standardized_survival_draws <- function(model,
                                                  data,
                                                  horizon = 182,
                                                  n_points = 200L,
                                                  n_draws = 200L,
                                                  seed = NULL,
                                                  treatment_col = "tx") {
  if (!inherits(model, c("stanreg", "stan_surv"))) {
    stop("`model` must be a fitted rstanarm survival model.")
  }
  if (!is.data.frame(data) || !treatment_col %in% names(data)) {
    stop("Standardization data must contain the treatment column: ", treatment_col)
  }

  horizon <- as.numeric(horizon)
  n_points <- as.integer(n_points)
  n_draws <- as.integer(n_draws)
  if (length(horizon) != 1L || !is.finite(horizon) || horizon <= 0 ||
      is.na(n_points) || n_points < 2L || is.na(n_draws) || n_draws < 1L) {
    stop("`horizon`, `n_points`, and `n_draws` are invalid.")
  }
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (is.na(seed)) stop("`seed` must be an integer.")
  }

  map(c(0, 1), function(treatment) {
    newdata <- data
    newdata[[treatment_col]] <- treatment

    raw <- posterior_survfit_compatible(
      model,
      newdata = newdata,
      times = 0,
      extrapolate = TRUE,
      standardise = TRUE,
      control = list(edist = horizon, epoints = n_points),
      return_matrix = TRUE,
      draws = n_draws,
      seed = seed
    )

    # rstanarm returns a list with one draws-by-1 matrix per time point when
    # standardising; also accept a single draws-by-time matrix.
    if (is.matrix(raw)) {
      survival <- as.matrix(raw)
      times <- attr(raw, "times")
    } else {
      if (!is.list(raw) || length(raw) == 0L) {
        stop("`posterior_survfit()` returned no standardized curves.")
      }
      survival <- as.matrix(do.call(cbind, raw))
      times <- map_dbl(raw, function(curve) {
        curve_time <- attr(curve, "times")
        if (length(curve_time) != 1L || !is.finite(curve_time)) {
          stop("Each standardized survival curve must contain one time point.")
        }
        as.numeric(curve_time)
      })
    }
    if (is.null(times)) times <- seq(0, horizon, length.out = ncol(survival))
    times <- as.numeric(times)

    # Draws must be rows.
    if (nrow(survival) < n_draws && ncol(survival) == n_draws) {
      survival <- t(survival)
    }
    if (length(times) != ncol(survival)) {
      stop("Survival-curve time points do not match the posterior matrix.")
    }
    if (nrow(survival) < 1L) stop("Standardized survival curves contain no draws.")
    if (nrow(survival) > n_draws) {
      survival <- survival[seq_len(n_draws), , drop = FALSE]
    }

    data.frame(
      draw = rep(seq_len(nrow(survival)), each = length(times)),
      time = rep(times, times = nrow(survival)),
      survival = as.vector(t(survival)),
      tx = treatment,
      stringsAsFactors = FALSE
    )
  }) |>
    list_rbind()
}


# Survival probability at `horizon` for every posterior draw and treatment
# (and imputation, if the curves have an `imputation` column). The horizon must
# be one of the prediction times of every curve. Returns a data frame with columns draw,
# tx, survival (and imputation), ordered by tx, then draw, then imputation.
survival_risk_draws <- function(curves,
                                horizon = 182,
                                treatment_col = "tx",
                                draw_col = "draw",
                                survival_col = "survival") {
  required <- c(treatment_col, draw_col, "time", survival_col)
  missing <- setdiff(required, names(curves))
  if (length(missing) > 0L) {
    stop("Survival curves are missing: ", paste(missing, collapse = ", "))
  }

  keys <- c(treatment_col, draw_col, intersect("imputation", names(curves)))
  if (!nrow(curves) || anyNA(curves[c(keys, "time", survival_col)]) ||
      any(!is.finite(curves$time)) ||
      any(!is.finite(curves[[survival_col]])) ||
      any(curves[[survival_col]] < 0 | curves[[survival_col]] > 1)) {
    stop("Survival curves must contain finite times and probabilities in [0, 1].")
  }
  if (length(horizon) != 1L || !is.finite(horizon)) {
    stop("Survival curves do not cover the requested horizon.")
  }

  risks <- curves |>
    arrange(time) |>
    group_by(across(all_of(keys))) |>
    summarise(
      duplicated_time = anyDuplicated(time) > 0L,
      covers_horizon = sum(time == horizon) == 1L,
      survival = .data[[survival_col]][time == horizon][1],
      .groups = "drop"
    )

  if (any(risks$duplicated_time)) stop("Duplicate times within a survival draw.")
  if (!all(risks$covers_horizon)) {
    stop("The requested horizon is not a prediction time of every curve.")
  }

  risks |>
    select(
      draw = all_of(draw_col),
      tx = all_of(treatment_col),
      survival,
      any_of("imputation")
    ) |>
    as.data.frame()
}


# Restricted mean survival time up to `horizon` for every survival curve
# (one curve per draw and `group_cols` combination), by the trapezoidal rule.
# The curve is extended flat to time 0 and to `horizon` where needed. Returns
# a data frame with columns `draw_col`, `group_cols`, rmst, ordered by draw,
# then the group columns.
compute_rmst_draws <- function(curves,
                               draw_col = "draw",
                               time_col = "time",
                               survival_col = "survival",
                               horizon = 182,
                               group_cols = character()) {
  required <- c(draw_col, time_col, survival_col, group_cols)
  missing <- setdiff(required, names(curves))
  if (length(missing) > 0L) {
    stop("Survival curves are missing: ", paste(missing, collapse = ", "))
  }

  curves |>
    group_by(across(all_of(c(draw_col, group_cols)))) |>
    summarise(
      rmst = {
        curve <- pick(all_of(c(time_col, survival_col)))
        time <- as.numeric(curve[[time_col]])
        survival <- as.numeric(curve[[survival_col]])

        keep <- is.finite(time) & is.finite(survival) &
          time >= 0 & time <= horizon
        time <- time[keep]
        survival <- survival[keep]
        if (length(time) < 2L) {
          stop("At least two survival-curve points are required.")
        }

        sort_order <- order(time)
        time <- time[sort_order]
        survival <- survival[sort_order]
        if (time[1L] > 0) {
          time <- c(0, time)
          survival <- c(survival[1L], survival)
        }
        if (tail(time, 1L) < horizon) {
          time <- c(time, horizon)
          survival <- c(survival, tail(survival, 1L))
        }

        sum(diff(time) * (head(survival, -1L) + tail(survival, -1L)) / 2)
      },
      .groups = "drop"
    ) |>
    as.data.frame()
}


# First dated event (e.g. progression) or death, on the privacy-shifted date
# scale. Durations in the export already encode the calendar data lock;
# potential_follow_up_days is capped at death for participants who died.
# Adds origin_date, follow_up_limit, extends_recorded_follow_up,
# after_follow_up_limit, event_or_censor_date, event_first,
# event_death_before_first, event_composite, censor_reason, time_days,
# exclusion_reason, in_analysis and data_lock; tx becomes numeric 0/1 and the
# event date column becomes a Date.
derive_dated_event_endpoints <- function(cohort,
                                         event_date_col,
                                         event_indicator_col = NULL,
                                         event_label = "event",
                                         data_lock = "2026-09-05") {
  # 0/1 indicator (numeric or character/factor labels) -> numeric 0/1.
  validate_binary <- function(x, field_name) {
    values <- suppressWarnings(as.numeric(as.character(x)))
    if (any(!is.na(values) & !values %in% c(0, 1))) {
      stop(field_name, " must be coded 0/1.")
    }
    if (anyNA(values)) stop(field_name, " cannot be missing.")
    values
  }

  # Date, POSIXct or character in one of four formats -> Date.
  parse_analysis_date <- function(x, field_name, allow_missing = TRUE) {
    if (inherits(x, "Date")) {
      out <- x
    } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
      out <- as.Date(x)
    } else if (is.character(x)) {
      value <- trimws(x)
      value[value == ""] <- NA_character_
      out <- as.Date(rep(NA_character_, length(value)))
      for (format in c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d.%m.%Y")) {
        pending <- is.na(out) & !is.na(value)
        if (!any(pending)) break
        out[pending] <- as.Date(value[pending], format = format)
      }
    } else {
      stop(field_name, " must be Date, POSIXct, or an ISO-like character vector.")
    }
    if (!allow_missing && anyNA(out)) {
      stop(field_name, " contains missing or invalid dates.")
    }
    out
  }

  if (!identical(as.character(data_lock), "2026-09-05")) {
    stop("Follow-up must use the 2026-09-05 export lock.")
  }
  required <- c(
    "id",
    "tx",
    "randomization_date",
    event_date_col,
    "survival_time_days_unrestricted",
    "status_death_unrestricted",
    "potential_follow_up_days"
  )
  if (!is.data.frame(cohort) || !all(required %in% names(cohort))) {
    stop("Endpoint cohort is missing required endpoint columns.")
  }
  if (anyNA(cohort$id) || anyDuplicated(cohort$id)) {
    stop("Endpoint cohort must have one non-missing ID per participant.")
  }

  out <- as.data.frame(cohort)
  out$tx <- validate_binary(out$tx, "tx")
  status <- validate_binary(
    out$status_death_unrestricted,
    "status_death_unrestricted"
  )
  for (column in c("survival_time_days_unrestricted", "potential_follow_up_days")) {
    if (!is.numeric(out[[column]]) || anyNA(out[[column]]) ||
        any(!is.finite(out[[column]]) | out[[column]] < 0)) {
      stop(column, " must contain finite non-negative follow-up durations.")
    }
  }

  randomization <- parse_analysis_date(
    out$randomization_date,
    "randomization_date",
    allow_missing = FALSE
  )
  out$origin_date <- randomization
  out[[event_date_col]] <- parse_analysis_date(out[[event_date_col]], event_date_col)
  event_date <- out[[event_date_col]]

  if (!is.null(event_indicator_col) && event_indicator_col %in% names(out) &&
      any(out[[event_indicator_col]] == 1 & is.na(event_date), na.rm = TRUE)) {
    stop(
      "A recorded ", event_label, " event has no ", event_label,
      " date; resolve it before analysis."
    )
  }

  # End of observation: death or last follow-up, whichever is first.
  out$follow_up_limit <- randomization + out$potential_follow_up_days
  observed_end <- randomization + pmin(
    out$survival_time_days_unrestricted,
    out$potential_follow_up_days
  )
  death_observed <- status == 1 &
    out$survival_time_days_unrestricted <= out$potential_follow_up_days
  death_end <- observed_end
  death_end[!death_observed] <- as.Date(NA)

  # An event counts if it is dated within follow-up and not after death.
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
  out$censor_reason <- case_when(
    event_observed ~ "event_observed",
    death_observed ~ "death_before_event",
    .default = "end_of_observed_follow_up"
  )
  out$time_days <- as.numeric(out$event_or_censor_date - out$origin_date)
  out$exclusion_reason <- case_when(
    event_observed & event_date < out$origin_date ~ "event_before_randomization",
    out$time_days <= 0 ~ "no_positive_follow_up",
    .default = "included"
  )
  out$in_analysis <- out$exclusion_reason == "included"
  out$data_lock <- as.character(data_lock)
  out
}
