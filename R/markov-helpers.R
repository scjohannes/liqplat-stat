# Posterior diagnostics for Markov models fitted with mostr::blrm_markov().

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
  sampler_diagnostics <- model$diagnostics$diagnostic_summary
  divergent_transitions <- if (
    is.data.frame(sampler_diagnostics) &&
      "num_divergent" %in% names(sampler_diagnostics)
  ) {
    sum(sampler_diagnostics$num_divergent, na.rm = TRUE)
  } else {
    0L
  }
  treedepth_exceeded <- if (
    is.data.frame(sampler_diagnostics) &&
      "num_max_treedepth" %in% names(sampler_diagnostics)
  ) {
    sum(sampler_diagnostics$num_max_treedepth, na.rm = TRUE)
  } else {
    0L
  }
  min_bfmi <- if (
    is.data.frame(sampler_diagnostics) &&
      "ebfmi" %in% names(sampler_diagnostics) &&
      any(is.finite(sampler_diagnostics$ebfmi))
  ) {
    min(sampler_diagnostics$ebfmi, na.rm = TRUE)
  } else {
    NA_real_
  }
  passed <- length(finite_rhat) > 0L &&
    all(finite_rhat <= as.numeric(thresholds$max_rhat)) &&
    length(finite_bulk) > 0L &&
    all(finite_bulk >= as.numeric(thresholds$min_ess_bulk)) &&
    length(finite_tail) > 0L &&
    all(finite_tail >= as.numeric(thresholds$min_ess_tail)) &&
    divergent_transitions <= as.numeric(thresholds$max_divergent_transitions) &&
    treedepth_exceeded <= as.numeric(thresholds$max_treedepth_exceeded) &&
    (is.na(min_bfmi) || min_bfmi >= as.numeric(thresholds$min_bfmi))
  list(
    summary = diagnostics,
    divergent_transitions = divergent_transitions,
    treedepth_exceeded = treedepth_exceeded,
    min_bfmi = min_bfmi,
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
