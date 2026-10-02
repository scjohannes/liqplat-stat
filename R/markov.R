# Convergence diagnostics for Bayesian Markov models fitted with
# mostr::blrm_markov() (rmsb::blrm() fits, cmdstan backend).
#
# Packages the calling notebook must attach: posterior.


# Per-parameter R-hat and bulk/tail ESS plus sampler diagnostics, checked
# against `thresholds` (e.g. config$diagnostics). Returns a list with
# summary (posterior::summarise_draws() output, one row per parameter),
# divergent_transitions, treedepth_exceeded, min_bfmi, passed and thresholds.
markov_diagnostics <- function(model, thresholds = NULL) {
  if (is.null(thresholds) || length(thresholds) == 0L) {
    thresholds <- list(
      max_rhat = 1.01,
      min_ess_bulk = 400,
      min_ess_tail = 400,
      max_divergent_transitions = 0,
      max_treedepth_exceeded = 0,
      min_bfmi = 0.3
    )
  }

  draws <- model$draws
  if (!is.matrix(draws) && !is.data.frame(draws)) {
    stop("Could not extract posterior draws from the Markov model.")
  }
  draws <- as.matrix(draws)

  # rmsb stacks the post-warmup draws of the chains on top of each other
  # (chain 1 first). R-hat and ESS need the chains kept apart, so reshape to
  # an iteration x chain x parameter array. The number of chains that
  # returned draws is the length of the per-chain sampler summary; fall back
  # to the requested number of chains.
  sampler_diagnostics <- model$diagnostics$diagnostic_summary
  n_chains <- length(sampler_diagnostics$num_divergent)
  if (n_chains == 0L) n_chains <- as.integer(model$chains)
  if (length(n_chains) != 1L || is.na(n_chains) || n_chains < 1L ||
      nrow(draws) %% n_chains != 0L) {
    stop("Could not split the Markov model draws into chains.")
  }
  draws_array <- array(
    draws,
    dim = c(nrow(draws) / n_chains, n_chains, ncol(draws)),
    dimnames = list(NULL, NULL, colnames(draws))
  )
  diagnostics <- as.data.frame(summarise_draws(as_draws_array(draws_array)))

  finite_rhat <- diagnostics$rhat[is.finite(diagnostics$rhat)]
  finite_bulk <- diagnostics$ess_bulk[is.finite(diagnostics$ess_bulk)]
  finite_tail <- diagnostics$ess_tail[is.finite(diagnostics$ess_tail)]

  # rmsb stores cmdstanr's diagnostic_summary(): a list with one value per
  # chain for num_divergent, num_max_treedepth and ebfmi. (The original
  # helper only read a data frame, so these checks were silently skipped.)
  if (!is.list(sampler_diagnostics)) {
    stop("Could not find the sampler diagnostics of the Markov model.")
  }

  divergent_transitions <- sum(sampler_diagnostics$num_divergent, na.rm = TRUE)
  treedepth_exceeded <- sum(sampler_diagnostics$num_max_treedepth, na.rm = TRUE)

  min_bfmi <- if (any(is.finite(sampler_diagnostics$ebfmi))) {
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


# Stop if markov_diagnostics() did not pass the configured thresholds.
assert_markov_diagnostics <- function(diagnostics) {
  if (!is.list(diagnostics) || !isTRUE(diagnostics$passed)) {
    stop("Markov posterior diagnostics failed the configured thresholds.")
  }
  invisible(diagnostics)
}
