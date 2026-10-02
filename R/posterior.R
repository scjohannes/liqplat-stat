# Posterior summaries shared by the analysis notebooks.
#
# Packages the calling notebook must attach: none (base R and stats only).


# Mean, median, SD and quantiles of a vector of posterior draws. Accepts a
# numeric vector, a one-column matrix, or a data frame with a `value_col`
# column. Non-finite draws are dropped. Quantiles use type 8 (approximately
# median-unbiased), so they differ slightly from the quantile() default.
summarize_posterior_draws <- function(draws,
                                      value_col = "estimate",
                                      probs = c(0.025, 0.5, 0.975)) {
  values <- if (is.numeric(draws)) {
    as.numeric(draws)
  } else if (is.matrix(draws) && ncol(draws) == 1L) {
    as.numeric(draws[, 1L])
  } else if (is.data.frame(draws) && value_col %in% names(draws)) {
    as.numeric(draws[[value_col]])
  } else {
    stop("Posterior draws must be numeric or contain `", value_col, "`.")
  }

  values <- values[is.finite(values)]
  if (length(values) == 0L) stop("Posterior draws contain no finite values.")

  quantiles <- quantile(values, probs = probs, names = FALSE, type = 8)
  names(quantiles) <- paste0("q", formatC(probs * 100, format = "fg", digits = 3))

  out <- data.frame(
    mean = mean(values),
    median = median(values),
    sd = if (length(values) > 1L) sd(values) else NA_real_,
    stringsAsFactors = FALSE
  )
  cbind(out, as.data.frame(as.list(quantiles), check.names = FALSE))
}


# Posterior probability that the estimand is above (or below) `threshold`,
# computed over the finite draws.
posterior_probability <- function(draws,
                                  threshold = 0,
                                  direction = c("greater", "less"),
                                  value_col = "estimate") {
  direction <- match.arg(direction)

  values <- if (is.numeric(draws)) {
    as.numeric(draws)
  } else if (is.matrix(draws) && ncol(draws) == 1L) {
    as.numeric(draws[, 1L])
  } else if (is.data.frame(draws) && value_col %in% names(draws)) {
    as.numeric(draws[[value_col]])
  } else {
    stop("Posterior draws must be numeric or contain `", value_col, "`.")
  }

  values <- values[is.finite(values)]
  if (length(values) == 0L) stop("Posterior draws contain no finite values.")

  if (direction == "greater") {
    mean(values > threshold)
  } else {
    mean(values < threshold)
  }
}
