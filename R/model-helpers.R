# Shared modeling and posterior-summary primitives.

as_numeric_draws <- function(draws, value_col = "estimate") {
  if (is.numeric(draws)) return(as.numeric(draws))
  if (is.matrix(draws) && ncol(draws) == 1L) return(as.numeric(draws[, 1L]))
  if (is.data.frame(draws) && value_col %in% names(draws)) {
    return(as.numeric(draws[[value_col]]))
  }
  stop("Posterior draws must be numeric or contain `", value_col, "`.")
}

summarize_posterior_draws <- function(draws, value_col = "estimate",
                                      probs = c(0.025, 0.5, 0.975)) {
  values <- as_numeric_draws(draws, value_col = value_col)
  values <- values[is.finite(values)]
  if (length(values) == 0L) stop("Posterior draws contain no finite values.")
  quantiles <- stats::quantile(values, probs = probs, names = FALSE, type = 8)
  out <- data.frame(
    mean = mean(values),
    median = stats::median(values),
    sd = if (length(values) > 1L) stats::sd(values) else NA_real_,
    stringsAsFactors = FALSE
  )
  names(quantiles) <- paste0("q", formatC(probs * 100, format = "fg", digits = 3))
  cbind(out, as.data.frame(as.list(quantiles), check.names = FALSE))
}

posterior_probability <- function(draws, threshold = 0,
                                  direction = c("greater", "less"),
                                  value_col = "estimate") {
  direction <- match.arg(direction)
  values <- as_numeric_draws(draws, value_col = value_col)
  values <- values[is.finite(values)]
  if (length(values) == 0L) stop("Posterior draws contain no finite values.")
  if (identical(direction, "greater")) mean(values > threshold) else mean(values < threshold)
}

credible_interval <- function(draws, level = 0.95, value_col = "estimate") {
  if (length(level) != 1L || level <= 0 || level >= 1) stop("`level` must be between 0 and 1.")
  values <- as_numeric_draws(draws, value_col = value_col)
  alpha <- (1 - level) / 2
  as.numeric(stats::quantile(values, c(alpha, 1 - alpha), names = FALSE, type = 8))
}

fit_general_bayesian <- function(data, formula, fit_function,
                                 fit_args = list()) {
  if (!is.data.frame(data) || !inherits(formula, "formula") || !is.function(fit_function)) {
    stop("`data`, `formula`, and `fit_function` are invalid.")
  }
  args <- c(list(formula = formula, data = data), fit_args)
  do.call(fit_function, args)
}

stack_imputation_results <- function(results, n_draws = NULL, seed = NULL) {
  pool_equal_weight_draws(results, n_draws = n_draws, seed = seed)
}

fit_bayesian_logistic <- function(data, outcome_col, covariate_cols = character(),
                                  seed, file = NULL, iter = 2000L,
                                  chains = 4L, cores = 1L, refresh = 0L) {
  required <- c(outcome_col, covariate_cols)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Bayesian logistic-model columns are missing: ", paste(missing, collapse = ", "))
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for Bayesian logistic implementation models.")
  }
  outcome <- validate_binary(data[[outcome_col]], outcome_col, allow_missing = FALSE)
  model_data <- data[!is.na(outcome), , drop = FALSE]
  model_data[[outcome_col]] <- as.numeric(outcome[!is.na(outcome)])
  rhs <- if (length(covariate_cols) == 0L) "1" else paste(covariate_cols, collapse = " + ")
  formula <- stats::as.formula(paste(outcome_col, "~", rhs))
  args <- list(
    formula = formula,
    data = model_data,
    family = brms::bernoulli(link = "logit"),
    iter = as.integer(iter),
    chains = as.integer(chains),
    cores = as.integer(cores),
    seed = as.integer(seed),
    refresh = as.integer(refresh)
  )
  if (!is.null(file)) args$file <- file
  do.call(brms::brm, args)
}

standardized_marginal_probability <- function(fit, newdata, ndraws = NULL) {
  if (!inherits(fit, "brmsfit")) stop("`fit` must be a brms fit.")
  if (!is.data.frame(newdata) || nrow(newdata) == 0L) {
    stop("`newdata` must contain at least one row.")
  }
  draws <- marginaleffects::avg_predictions(
    fit, newdata = newdata, type = "response", re_formula = NA
  ) |>
    marginaleffects::get_draws()
  values <- draws$draw
  if (!is.null(ndraws)) {
    ndraws <- as.integer(ndraws)
    if (is.na(ndraws) || ndraws < 1L) stop("`ndraws` must be positive.")
    keep <- seq_len(min(ndraws, length(values)))
    values <- values[keep]
  }
  data.frame(draw = as.numeric(values), stringsAsFactors = FALSE)
}

summarize_binary_draws <- function(draws, outcome = "probability") {
  values <- as_numeric_draws(draws, value_col = "draw")
  values <- values[is.finite(values)]
  if (length(values) == 0L) stop("Posterior probability draws contain no finite values.")
  quantiles <- stats::quantile(values, probs = c(0.025, 0.5, 0.975), names = FALSE,
                               type = 8)
  data.frame(
    outcome = outcome,
    mean = mean(values),
    median = stats::median(values),
    q025 = quantiles[[1L]],
    q975 = quantiles[[3L]],
    stringsAsFactors = FALSE
  )
}
