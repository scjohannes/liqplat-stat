# Overall-survival model and RMST helpers.

derive_mgps <- function(albumin, c_reactive_protein) {
  albumin <- if (is.factor(albumin)) as.numeric(as.character(albumin)) else as.numeric(albumin)
  c_reactive_protein <- if (is.factor(c_reactive_protein)) as.numeric(as.character(c_reactive_protein)) else as.numeric(c_reactive_protein)
  if (length(albumin) != length(c_reactive_protein)) {
    stop("Albumin and C-reactive protein must have equal lengths for mGPS.")
  }
  ifelse(
    is.na(albumin) | is.na(c_reactive_protein),
    NA_integer_,
    ifelse(c_reactive_protein <= 10, 0L,
           ifelse(albumin >= 35, 1L, 2L))
  )
}

derive_ecog_binary <- function(ecog_fstcnt) {
  ecog_fstcnt <- if (is.factor(ecog_fstcnt)) as.numeric(as.character(ecog_fstcnt)) else as.numeric(ecog_fstcnt)
  ifelse(is.na(ecog_fstcnt), NA_integer_, as.integer(ecog_fstcnt > 1L))
}

assert_ecog_binary_consistent <- function(data, ecog_binary_col = "ecog_binary",
                                          ecog_source_col = "ecog_fstcnt") {
  required <- c(ecog_binary_col, ecog_source_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("ECOG consistency check is missing: ", paste(missing, collapse = ", "))
  }
  expected <- derive_ecog_binary(data[[ecog_source_col]])
  observed <- suppressWarnings(as.integer(as.character(data[[ecog_binary_col]])))
  mismatch <- !is.na(expected) & (is.na(observed) | observed != expected)
  if (any(mismatch)) stop("Binary ECOG is inconsistent with the completed ECOG source values.")
  invisible(data)
}

assert_mgps_consistent <- function(data, mgps_col = "mgps",
                                   albumin_col = "albumin",
                                   c_reactive_protein_col = "c_reactive_protein") {
  required <- c(mgps_col, albumin_col, c_reactive_protein_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("mGPS consistency check is missing: ", paste(missing, collapse = ", "))
  expected <- derive_mgps(data[[albumin_col]], data[[c_reactive_protein_col]])
  observed <- suppressWarnings(as.integer(as.character(data[[mgps_col]])))
  mismatch <- !is.na(expected) & (is.na(observed) | observed != expected)
  if (any(mismatch)) stop("mGPS is inconsistent with the completed albumin and CRP values.")
  invisible(data)
}

fit_survival_model <- function(data, formula, fit_args = list()) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package 'rstanarm' is required for the survival model.")
  }
  if (!is.data.frame(data) || !inherits(formula, "formula")) {
    stop("Survival data and formula are invalid.")
  }
  args <- c(list(formula = formula, data = data), fit_args)
  if (is.null(args$basehaz)) args$basehaz <- "ms"
  do.call(rstanarm::stan_surv, args)
}

# Return standardized posterior survival curves in one stable long format.
# rstanarm currently returns a list of matrices for `posterior_survfit()` when
# standardising over a data frame; this wrapper also accepts the matrix form so
# that the endpoint notebooks do not depend on that implementation detail.
posterior_standardized_survival <- function(model, data, treatment,
                                            treatment_col = "tx",
                                            horizon = 182, n_points = 200L,
                                            n_draws = 200L, seed = NULL) {
  if (!inherits(model, c("stanreg", "stan_surv"))) {
    stop("`model` must be a fitted rstanarm survival model.")
  }
  if (!is.data.frame(data) || !treatment_col %in% names(data)) {
    stop("Standardization data must contain the treatment column: ", treatment_col)
  }
  treatment <- as.numeric(treatment)
  if (length(treatment) != 1L || !is.finite(treatment)) {
    stop("`treatment` must be one finite scalar value.")
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
  newdata <- data
  newdata[[treatment_col]] <- treatment
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  if (!is.null(seed)) set.seed(seed)
  raw <- tryCatch(
    rstanarm::posterior_survfit(
      model,
      newdata = newdata,
      times = 0,
      extrapolate = TRUE,
      standardise = TRUE,
      control = list(edist = horizon, epoints = n_points),
      return_matrix = TRUE,
      draws = n_draws
    ),
    finally = {
      if (old_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }
  )
  matrix_value <- if (is.matrix(raw)) raw else {
    if (!is.list(raw) || length(raw) == 0L) {
      stop("`posterior_survfit()` returned no standardized curves.")
    }
    do.call(cbind, raw)
  }
  matrix_value <- as.matrix(matrix_value)
  times <- attr(raw, "times")
  if (is.null(times) && is.list(raw)) times <- attr(raw[[1L]], "times")
  if (is.null(times)) times <- seq(0, horizon, length.out = ncol(matrix_value))
  times <- as.numeric(times)
  if (length(times) != ncol(matrix_value)) {
    stop("Survival-curve time points do not match the posterior matrix.")
  }
  if (nrow(matrix_value) < n_draws && ncol(matrix_value) == n_draws) {
    matrix_value <- t(matrix_value)
  }
  if (nrow(matrix_value) < 1L) stop("Standardized survival curves contain no draws.")
  if (nrow(matrix_value) > n_draws) matrix_value <- matrix_value[seq_len(n_draws), , drop = FALSE]
  data.frame(
    draw = rep(seq_len(nrow(matrix_value)), each = length(times)),
    time = rep(times, times = nrow(matrix_value)),
    survival = as.vector(t(matrix_value)),
    tx = treatment,
    stringsAsFactors = FALSE
  )
}

posterior_standardized_survival_draws <- function(model, data,
                                                  horizon = 182,
                                                  n_points = 200L,
                                                  n_draws = 200L,
                                                  seed = NULL,
                                                  treatment_col = "tx") {
  treatment_levels <- c(0, 1)
  out <- lapply(seq_along(treatment_levels), function(index) {
    posterior_standardized_survival(
      model = model,
      data = data,
      treatment = treatment_levels[index],
      treatment_col = treatment_col,
      horizon = horizon,
      n_points = n_points,
      n_draws = n_draws,
      seed = if (is.null(seed)) NULL else as.integer(seed + (index - 1L) * 1000L)
    )
  })
  do.call(rbind, out)
}

survival_risk_draws <- function(curves, horizon = 182, treatment_col = "tx",
                                draw_col = "draw", survival_col = "survival") {
  required <- c(treatment_col, draw_col, "time", survival_col)
  missing <- setdiff(required, names(curves))
  if (length(missing) > 0L) stop("Survival curves are missing: ", paste(missing, collapse = ", "))
  pieces <- split(curves, interaction(curves[[draw_col]], curves[[treatment_col]], drop = TRUE))
  values <- lapply(pieces, function(piece) {
    piece <- piece[order(piece$time), , drop = FALSE]
    survival <- stats::approx(piece$time, piece[[survival_col]], xout = horizon,
                              rule = 2, ties = "ordered")$y
    data.frame(
      draw = piece[[draw_col]][1L],
      tx = piece[[treatment_col]][1L],
      survival = as.numeric(survival),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, values)
  rownames(out) <- NULL
  out
}

summarize_survival_risks <- function(risks, treatment_levels = c(0, 1)) {
  if (!is.data.frame(risks) || !all(c("draw", "tx", "survival") %in% names(risks))) {
    stop("Risk draws must contain draw, tx, and survival columns.")
  }
  split_draws <- split(risks, risks$draw)
  risk_difference <- vapply(split_draws, function(piece) {
    values <- piece$survival[match(treatment_levels, piece$tx)]
    if (anyNA(values)) stop("A survival draw is missing a treatment level.")
    values[2L] - values[1L]
  }, numeric(1))
  risk_ratio <- vapply(split_draws, function(piece) {
    values <- piece$survival[match(treatment_levels, piece$tx)]
    if (anyNA(values) || values[1L] == 0) return(NA_real_)
    values[2L] / values[1L]
  }, numeric(1))
  list(
    risk_difference = risk_difference,
    risk_ratio = risk_ratio,
    summary = rbind(
      risk_difference = summarize_posterior_draws(risk_difference),
      risk_ratio = summarize_posterior_draws(risk_ratio[is.finite(risk_ratio)])
    )
  )
}

rmst_from_survival_curve <- function(time, survival, horizon = 182) {
  time <- as.numeric(time)
  survival <- as.numeric(survival)
  keep <- is.finite(time) & is.finite(survival) & time >= 0 & time <= horizon
  time <- time[keep]
  survival <- survival[keep]
  if (length(time) < 2L) stop("At least two survival-curve points are required.")
  order_idx <- order(time)
  time <- time[order_idx]
  survival <- survival[order_idx]
  if (time[1L] > 0) {
    time <- c(0, time)
    survival <- c(survival[1L], survival)
  }
  if (tail(time, 1L) < horizon) {
    time <- c(time, horizon)
    survival <- c(survival, tail(survival, 1L))
  }
  sum(diff(time) * (head(survival, -1L) + tail(survival, -1L)) / 2)
}

compute_rmst_draws <- function(curves, draw_col = "draw", time_col = "time",
                               survival_col = "survival", horizon = 182,
                               group_cols = character()) {
  required <- c(draw_col, time_col, survival_col, group_cols)
  missing <- setdiff(required, names(curves))
  if (length(missing) > 0L) stop("Survival curves are missing: ", paste(missing, collapse = ", "))
  split_key <- interaction(curves[, c(draw_col, group_cols), drop = FALSE],
                           drop = TRUE, lex.order = TRUE)
  pieces <- split(curves, split_key)
  result <- lapply(pieces, function(piece) {
    values <- data.frame(rmst = rmst_from_survival_curve(
      piece[[time_col]], piece[[survival_col]], horizon = horizon
    ))
    values[[draw_col]] <- piece[[draw_col]][1L]
    for (column in group_cols) values[[column]] <- piece[[column]][1L]
    values
  })
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out[, c(draw_col, group_cols, "rmst"), drop = FALSE]
}

summarize_rmst_draws <- function(draws, treatment_col = "tx", draw_col = "draw",
                                 treatment_levels = c(0, 1)) {
  if (!is.data.frame(draws) || !all(c(draw_col, treatment_col, "rmst") %in% names(draws))) {
    stop("RMST draws must contain draw, treatment, and rmst columns.")
  }
  if (!all(treatment_levels %in% draws[[treatment_col]])) {
    stop("RMST draws do not contain both configured treatment levels.")
  }
  split_draws <- split(draws, draws[[draw_col]])
  contrasts <- vapply(split_draws, function(x) {
    values <- x$rmst[match(treatment_levels, x[[treatment_col]])]
    if (anyNA(values)) stop("A posterior draw is missing a treatment level.")
    values[2L] - values[1L]
  }, numeric(1))
  summary <- summarize_posterior_draws(contrasts)
  summary$probability_benefit <- posterior_probability(contrasts)
  list(draws = contrasts, summary = summary)
}
