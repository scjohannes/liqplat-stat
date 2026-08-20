# Bayesian negative-binomial count/rate helpers for secondary analyses.
#
# Count outcomes are not silently completed: an NA count is not a zero, and
# every model requires a finite, strictly positive observed exposure.

count_offset_column <- function(formula) {
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.")
  text <- paste(deparse(formula), collapse = " ")
  patterns <- c(
    "offset\\s*\\(\\s*log\\s*\\(\\s*([[:alnum:]_.]+)",
    "offset\\s*\\(\\s*([[:alnum:]_.]+)"
  )
  for (pattern in patterns) {
    match <- regexec(pattern, text, perl = TRUE)
    values <- regmatches(text, match)[[1L]]
    if (length(values) >= 2L) return(values[[2L]])
  }
  NULL
}

validate_count_inputs <- function(data, formula, exposure_col = NULL) {
  if (!is.data.frame(data) || !inherits(formula, "formula")) {
    stop("Count-model data and formula are invalid.")
  }
  response <- all.vars(formula)[1L]
  if (is.null(response) || !response %in% names(data)) {
    stop("Count response is missing from the model data.")
  }
  if (is.null(exposure_col)) exposure_col <- count_offset_column(formula)
  if (is.null(exposure_col) || length(exposure_col) != 1L ||
      !exposure_col %in% names(data)) {
    stop("A positive observed exposure column and offset are required for count models.")
  }
  counts <- suppressWarnings(as.numeric(data[[response]]))
  exposure <- suppressWarnings(as.numeric(data[[exposure_col]]))
  if (anyNA(counts)) {
    stop(response, " contains missing values; missing is not equivalent to zero.")
  }
  if (any(!is.finite(counts) | counts < 0 | counts != floor(counts))) {
    stop(response, " must contain non-negative integer counts.")
  }
  if (anyNA(exposure) || any(!is.finite(exposure) | exposure <= 0)) {
    stop(exposure_col, " must contain finite, strictly positive exposure.")
  }
  list(response = response, exposure_col = exposure_col)
}

is_negative_binomial_family <- function(family) {
  if (is.null(family)) return(FALSE)
  family_name <- tryCatch(as.character(family$family), error = function(e) "")
  length(family_name) == 1L && grepl("negative|neg_binomial", family_name,
                                     ignore.case = TRUE)
}

fit_count_model <- function(data, formula, exposure_col = NULL, family = NULL,
                            fit_args = list()) {
  inputs <- validate_count_inputs(data, formula, exposure_col)
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package 'rstanarm' is required for Bayesian count models.")
  }
  if (is.null(family)) family <- rstanarm::neg_binomial_2()
  if (!is_negative_binomial_family(family)) {
    stop("Count models must use a Bayesian negative-binomial family.")
  }
  if (!is.list(fit_args)) stop("`fit_args` must be a list.")
  fit_args <- fit_args[setdiff(names(fit_args), c("formula", "data", "family"))]
  args <- c(
    list(formula = formula, data = data, family = family),
    fit_args
  )
  # Keep the explicit exposure contract visible to callers even though the
  # offset is part of the formula consumed by stan_glm().
  attr(args, "exposure_col") <- inputs$exposure_col
  do.call(rstanarm::stan_glm, args)
}

summarize_count_rates <- function(data, count_col, denominator_col = NULL,
                                  group_cols = character()) {
  required <- c(count_col, denominator_col, group_cols)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Count summary columns are missing: ", paste(missing, collapse = ", "))
  }
  count <- suppressWarnings(as.numeric(data[[count_col]]))
  if (anyNA(count)) stop(count_col, " contains missing values; missing is not zero.")
  if (any(!is.finite(count) | count < 0 | count != floor(count))) {
    stop(count_col, " must contain non-negative integer counts.")
  }
  if (is.null(denominator_col)) {
    denominator <- rep(1, nrow(data))
  } else {
    denominator <- suppressWarnings(as.numeric(data[[denominator_col]]))
    if (anyNA(denominator) || any(!is.finite(denominator) | denominator <= 0)) {
      stop(denominator_col, " must contain finite, strictly positive exposure.")
    }
  }
  if (length(group_cols) == 0L) {
    return(data.frame(
      events = sum(count), exposure = sum(denominator),
      rate = sum(count) / sum(denominator), stringsAsFactors = FALSE
    ))
  }
  if (anyNA(data[group_cols])) {
    stop("Grouping columns contain missing values; missing groups are not zero.")
  }
  key <- do.call(paste, c(data[group_cols], sep = "\r"))
  groups <- split(seq_len(nrow(data)), key, drop = TRUE)
  result <- lapply(groups, function(index) {
    row <- data[index[1L], group_cols, drop = FALSE]
    row$events <- sum(count[index])
    row$exposure <- sum(denominator[index])
    row$rate <- row$events / row$exposure
    row
  })
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}

standardize_count_draws <- function(draws, draw_col = "draw", treatment_col = "tx",
                                    rate_col = "rate", treatment_levels = c(0, 1)) {
  required <- c(draw_col, treatment_col, rate_col)
  missing <- setdiff(required, names(draws))
  if (length(missing) > 0L) {
    stop("Posterior count draws are missing: ", paste(missing, collapse = ", "))
  }
  rate <- suppressWarnings(as.numeric(draws[[rate_col]]))
  if (anyNA(rate) || any(!is.finite(rate) | rate < 0)) {
    stop("Posterior rates must be finite and non-negative.")
  }
  draw_values <- unique(draws[[draw_col]])
  pieces <- lapply(draw_values, function(draw_value) {
    piece <- draws[draws[[draw_col]] == draw_value, , drop = FALSE]
    if (!all(treatment_levels %in% piece[[treatment_col]])) {
      stop("A posterior draw is missing a treatment level.")
    }
    values <- rate[draws[[draw_col]] == draw_value]
    treatment <- piece[[treatment_col]]
    rate_0 <- values[match(treatment_levels[[1L]], treatment)]
    rate_1 <- values[match(treatment_levels[[2L]], treatment)]
    if (anyNA(c(rate_0, rate_1))) stop("Posterior count rates are incomplete.")
    data.frame(
      draw = draw_value,
      rate_0 = rate_0,
      rate_1 = rate_1,
      rate_ratio = if (rate_0 == 0) NA_real_ else rate_1 / rate_0,
      rate_difference = rate_1 - rate_0,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

summarize_count_posterior <- function(draws) {
  required <- c("rate_0", "rate_1", "rate_ratio", "rate_difference")
  missing <- setdiff(required, names(draws))
  if (length(missing) > 0L) {
    stop("Standardized count draws are missing: ", paste(missing, collapse = ", "))
  }
  summarize_metric <- function(values, lower_is_better = FALSE, threshold = 0) {
    values <- as.numeric(values)
    finite <- is.finite(values)
    if (!any(finite)) {
      return(c(median = NA_real_, lower_95 = NA_real_, upper_95 = NA_real_,
               posterior_probability_benefit = NA_real_))
    }
    values <- values[finite]
    quantiles <- stats::quantile(values, c(0.025, 0.5, 0.975),
                                 names = FALSE, type = 8)
    probability <- if (isTRUE(lower_is_better)) mean(values < threshold) else mean(values > threshold)
    c(median = quantiles[[2L]], lower_95 = quantiles[[1L]],
      upper_95 = quantiles[[3L]], posterior_probability_benefit = probability)
  }
  rows <- list(
    rate_external_comparator = summarize_metric(draws$rate_0),
    rate_sat = summarize_metric(draws$rate_1),
    rate_ratio_sat_vs_external = summarize_metric(draws$rate_ratio,
                                                   lower_is_better = TRUE,
                                                   threshold = 1),
    rate_difference_sat_minus_external = summarize_metric(draws$rate_difference,
                                                          lower_is_better = TRUE)
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  out$metric <- rownames(out)
  rownames(out) <- NULL
  out[, c("metric", "median", "lower_95", "upper_95",
          "posterior_probability_benefit"), drop = FALSE]
}
