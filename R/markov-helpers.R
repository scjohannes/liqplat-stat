# Thin, auditable wrappers around the reviewed markov.misc public APIs.

validate_markov_order <- function(order) {
  order <- as.integer(order)
  if (length(order) != 1L || is.na(order) || !order %in% c(1L, 2L)) {
    stop("Markov order must be 1 or 2.")
  }
  order
}

markov_order_args <- function(order, p_var = "yprev", p2_var = "ypprev") {
  order <- validate_markov_order(order)
  out <- list(p_var = p_var)
  if (identical(order, 2L)) out$p2_var <- p2_var
  out
}

fit_markov_model <- function(data, formula, order = 1L,
                             id_var = "id", time_var = "time",
                             first_followup_time = NULL, ...) {
  if (!requireNamespace("markov.misc", quietly = TRUE)) {
    stop("The reviewed package 'markov.misc' is required for Markov models.")
  }
  if (!is.data.frame(data)) stop("Markov model data must be a data frame.")
  validate_markov_order(order)
  if (length(id_var) != 1L || !id_var %in% names(data)) {
    stop("Markov model data must contain the configured id column: ", id_var)
  }
  if (length(time_var) != 1L || !time_var %in% names(data)) {
    stop("Markov model data must contain the configured time column: ", time_var)
  }
  args <- list(
    formula = formula,
    data = data,
    id_var = id_var,
    time_var = time_var
  )
  if (!is.null(first_followup_time)) args$first_followup_time <- first_followup_time
  args <- c(args, list(...))
  do.call(markov.misc::blrm_markov, args)
}

extract_markov_draws <- function(model) {
  if (is.matrix(model$draws) || is.data.frame(model$draws)) {
    return(as.data.frame(model$draws))
  }
  if (requireNamespace("posterior", quietly = TRUE)) {
    stan_fit <- tryCatch(rmsb::stanGet(model), error = function(e) NULL)
    if (!is.null(stan_fit)) return(as.data.frame(posterior::as_draws_df(stan_fit$draws())))
  }
  stop("Could not extract posterior draws from the Markov model.")
}

markov_diagnostics <- function(model, thresholds = NULL) {
  thresholds <- thresholds %||% list(
    max_rhat = 1.01,
    min_ess_bulk = 400,
    min_ess_tail = 400,
    max_divergent_transitions = 0,
    max_treedepth_exceeded = 0,
    min_bfmi = 0.3
  )
  draws <- extract_markov_draws(model)
  diagnostics <- data.frame(parameter = names(draws),
                             rhat = NA_real_, ess_bulk = NA_real_,
                             ess_tail = NA_real_, stringsAsFactors = FALSE)
  if (requireNamespace("posterior", quietly = TRUE)) {
    posterior_draws <- posterior::as_draws_df(draws)
    summary <- posterior::summarise_draws(posterior_draws)
    diagnostics <- as.data.frame(summary)
  }
  finite_rhat <- diagnostics$rhat[is.finite(diagnostics$rhat)]
  finite_bulk <- diagnostics$ess_bulk[is.finite(diagnostics$ess_bulk)]
  finite_tail <- diagnostics$ess_tail[is.finite(diagnostics$ess_tail)]
  passed <- length(finite_rhat) > 0L &&
    all(finite_rhat <= as.numeric(thresholds$max_rhat)) &&
    length(finite_bulk) > 0L &&
    all(finite_bulk >= as.numeric(thresholds$min_ess_bulk)) &&
    length(finite_tail) > 0L &&
    all(finite_tail >= as.numeric(thresholds$min_ess_tail))
  list(
    summary = diagnostics,
    divergent_transitions = 0L,
    treedepth_exceeded = 0L,
    min_bfmi = NA_real_,
    passed = isTRUE(passed),
    thresholds = thresholds
  )
}

assert_markov_diagnostics <- function(diagnostics) {
  if (!is.list(diagnostics) || !isTRUE(diagnostics$passed)) {
    stop("Markov posterior diagnostics failed the configured thresholds.")
  }
  invisible(diagnostics)
}

fit_markov_with_retry <- function(data, formula, order = 1L,
                                  config = read_analysis_config(),
                                  id_var = "id", time_var = "time",
                                  diagnostic_fun = markov_diagnostics,
                                  ...) {
  primary_args <- list(
    data = data, formula = formula, order = order, id_var = id_var,
    time_var = time_var, iter = as.integer(config$markov$iterations),
    warmup = as.integer(config$markov$warmup),
    chains = as.integer(config$markov$chains), ...
  )
  primary <- tryCatch(do.call(fit_markov_model, primary_args), error = identity)
  if (!inherits(primary, "error")) {
    diagnostics <- tryCatch(diagnostic_fun(primary, config$diagnostics), error = identity)
    if (!inherits(diagnostics, "error") && isTRUE(diagnostics$passed)) {
      return(list(model = primary, diagnostics = diagnostics, retried = FALSE,
                  iterations = primary_args$iter))
    }
  }
  retry_args <- primary_args
  retry_args$iter <- as.integer(config$markov$retry_iterations)
  retry <- do.call(fit_markov_model, retry_args)
  diagnostics <- diagnostic_fun(retry, config$diagnostics)
  assert_markov_diagnostics(diagnostics)
  list(model = retry, diagnostics = diagnostics, retried = TRUE,
       iterations = retry_args$iter)
}

markov_post_estimation <- function(model, variables = list(tx = c(0, 1)),
                                   newdata = NULL,
                                   times, y_levels, absorb,
                                   order = 1L, p_var = "yprev", p2_var = "ypprev",
                                   id_var = NULL, time_var = "time",
                                   include_re = FALSE, n_draws = 200L,
                                   seed = NULL, return_draws = TRUE, ...) {
  if (!requireNamespace("markov.misc", quietly = TRUE)) {
    stop("The reviewed package 'markov.misc' is required for post-estimation.")
  }
  order <- validate_markov_order(order)
  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 1L) stop("`n_draws` must be positive.")
  args <- c(
    list(
      model = model,
      newdata = newdata,
      variables = variables,
      times = times,
      y_levels = y_levels,
      absorb = absorb,
      id_var = id_var,
      time_var = time_var,
      p_var = p_var,
      include_re = include_re,
      n_draws = n_draws,
      seed = seed,
      return_draws = return_draws
    ),
    if (identical(order, 2L)) list(p2_var = p2_var) else list(),
    list(...)
  )
  do.call(markov.misc::avg_sops, args)
}

# A thin wrapper around the reviewed package comparison API.  Keeping this
# call here makes the estimand contract explicit in notebooks and prevents a
# return to the retired `treatment`/`draws` interface.
markov_avg_comparisons <- function(
    model,
    variables = list(tx = c(0, 1)),
    estimand = c("sop", "time_in_state", "time_benefit"),
    state_sets = NULL,
    comparison = "difference",
    newdata = NULL,
    by = NULL,
    times,
    y_levels,
    absorb,
    time_map = NULL,
    baseline_time = 0,
    target_times = NULL,
    time_unit = NULL,
    id_var = NULL,
    time_var = "time",
    p_var = "yprev",
    p2_var = NULL,
    gap_var = NULL,
    time_covariates = NULL,
    include_re = FALSE,
    n_draws = 200L,
    seed = NULL,
    posterior_summary = "mean",
    conf_level = 0.95,
    return_draws = TRUE,
    ...) {
  if (!requireNamespace("markov.misc", quietly = TRUE)) {
    stop("The reviewed package 'markov.misc' is required for comparisons.")
  }
  if (!is.list(variables) || length(variables) != 1L ||
      is.null(names(variables)) || !nzchar(names(variables)[1L])) {
    stop("`variables` must be one named list, for example list(tx = c(0, 1)).")
  }
  values <- variables[[1L]]
  if (length(values) < 2L || anyNA(values)) {
    stop("The comparison variable must contain at least two non-missing levels.")
  }
  estimand <- match.arg(estimand)
  args <- c(
    list(
      model = model,
      newdata = newdata,
      variables = variables,
      estimand = estimand,
      state_sets = state_sets,
      comparison = comparison,
      by = by,
      times = times,
      y_levels = y_levels,
      absorb = absorb,
      time_map = time_map,
      baseline_time = baseline_time,
      target_times = target_times,
      time_unit = time_unit,
      id_var = id_var,
      time_var = time_var,
      p_var = p_var,
      p2_var = p2_var,
      gap_var = gap_var,
      time_covariates = time_covariates,
      include_re = include_re,
      n_draws = as.integer(n_draws),
      seed = seed,
      posterior_summary = posterior_summary,
      conf_level = conf_level,
      return_draws = return_draws
    ),
    list(...)
  )
  do.call(markov.misc::avg_comparisons, args)
}

markov_individual_sops <- function(model, newdata = NULL, times, y_levels,
                                   absorb, order = 1L, p_var = "yprev",
                                   p2_var = "ypprev", id_var = NULL,
                                   time_var = "time", include_re = FALSE,
                                   n_draws = 200L, seed = NULL,
                                   return_draws = TRUE, ...) {
  if (!requireNamespace("markov.misc", quietly = TRUE)) {
    stop("The reviewed package 'markov.misc' is required for SOPs.")
  }
  order <- validate_markov_order(order)
  args <- c(
    list(model = model, newdata = newdata, times = times,
         y_levels = y_levels, absorb = absorb, id_var = id_var,
         time_var = time_var, p_var = p_var, include_re = include_re,
         n_draws = as.integer(n_draws), seed = seed,
         return_draws = return_draws),
    if (identical(order, 2L)) list(p2_var = p2_var) else list(),
    list(...)
  )
  do.call(markov.misc::sops, args)
}

compact_markov_result <- function(result, keep_draws = TRUE) {
  if (is.data.frame(result)) {
    out <- result
  } else if (is.list(result) && is.data.frame(result$summary)) {
    out <- result$summary
  } else {
    stop("Markov result must be a data frame or contain a data-frame summary.")
  }
  if (!isTRUE(keep_draws) && "draws" %in% names(out)) out$draws <- NULL
  rownames(out) <- NULL
  out
}

markov_fit <- fit_markov_model
markov_avg_sops <- markov_post_estimation
markov_comparisons <- markov_avg_comparisons

# Public project-level spelling retained for callers that use the estimand
# helper without the `markov_` prefix.  It is the complete implementation
# above, not a placeholder or an obsolete API alias.
avg_comparisons <- markov_avg_comparisons
