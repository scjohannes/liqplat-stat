# Deterministic one-imputation MICE branches and equal-weight draw pooling.

one_imputation_seed <- function(seed, imputation) {
  seed <- as.numeric(seed)
  imputation <- as.integer(imputation)
  if (length(seed) != 1L || !is.finite(seed) || is.na(imputation) || imputation < 1L) {
    stop("`seed` and `imputation` must be valid scalar values.")
  }
  value <- seed + (imputation - 1) * 1000
  if (value < 1 || value > .Machine$integer.max) stop("Imputation seed is out of range.")
  as.integer(value)
}

validate_mice_spec <- function(data, method, predictor_matrix) {
  if (!is.data.frame(data)) stop("MICE input must be a data frame.")
  if (length(method) > 0L && !is.null(names(method))) {
    unknown <- setdiff(names(method), names(data))
    if (length(unknown) > 0L) stop("MICE methods name unknown columns: ", paste(unknown, collapse = ", "))
  }
  if (!is.null(predictor_matrix)) {
    predictor_matrix <- as.matrix(predictor_matrix)
    if (!identical(dim(predictor_matrix), c(ncol(data), ncol(data))) ||
        !identical(colnames(predictor_matrix), names(data)) ||
        !identical(rownames(predictor_matrix), names(data))) {
      stop("MICE predictorMatrix must be a square matrix named by input columns.")
    }
  }
  invisible(TRUE)
}

mice_one_imputation <- function(data, method = NULL, predictor_matrix = NULL,
                                maxit = 50L, donors = 5L, seed,
                                imputation = 1L, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' is required for imputation.")
  }
  maxit <- as.integer(maxit)
  donors <- as.integer(donors)
  if (is.na(maxit) || maxit < 1L || is.na(donors) || donors < 1L) {
    stop("`maxit` and `donors` must be positive integers.")
  }
  validate_mice_spec(data, method, predictor_matrix)
  branch_seed <- one_imputation_seed(seed, imputation)
  mice_args <- list(
    data = data,
    m = 1L,
    maxit = maxit,
    seed = branch_seed,
    donors = donors
  )
  if (!is.null(method)) mice_args$method <- method
  if (!is.null(predictor_matrix)) mice_args$predictorMatrix <- predictor_matrix
  mice_args <- c(mice_args, list(...))
  do.call(mice::mice, mice_args)
}

complete_one_imputation <- function(mids_object, imputation = 1L) {
  if (!inherits(mids_object, "mids")) stop("Expected a `mids` object.")
  if (as.integer(imputation) != 1L) {
    stop("One-imputation branches contain exactly one completed dataset.")
  }
  mice::complete(mids_object, action = 1L)
}

run_mice_branches <- function(data, imputations, method = NULL,
                              predictor_matrix = NULL, maxit = 50L,
                              donors = 5L, seed, ...) {
  imputations <- as.integer(imputations)
  if (length(imputations) == 0L || anyNA(imputations) || any(imputations < 1L)) {
    stop("`imputations` must contain positive integers.")
  }
  lapply(imputations, function(index) {
    fit <- mice_one_imputation(
      data = data,
      method = method,
      predictor_matrix = predictor_matrix,
      maxit = maxit,
      donors = donors,
      seed = seed,
      imputation = index,
      ...
    )
    list(index = index, seed = one_imputation_seed(seed, index), mids = fit,
         data = complete_one_imputation(fit))
  })
}

pool_equal_weight_draws <- function(draws, n_draws = NULL, seed = NULL,
                                    imputation_col = ".imputation") {
  if (!is.list(draws) || length(draws) == 0L) stop("`draws` must be a non-empty list.")
  frames <- lapply(draws, function(x) {
    if (is.matrix(x)) x <- as.data.frame(x)
    if (is.vector(x) && !is.list(x)) x <- data.frame(estimate = x)
    if (!is.data.frame(x)) stop("Each draw set must be a data frame, matrix, or vector.")
    x
  })
  sizes <- vapply(frames, nrow, integer(1))
  if (any(sizes < 1L)) stop("Every imputation must contain at least one draw.")
  n_draws <- if (is.null(n_draws)) min(sizes) else as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 1L || any(sizes < n_draws)) {
    stop("Each imputation must contain at least `n_draws` posterior draws.")
  }
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (is.na(seed)) stop("`seed` must be an integer.")
  }
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)
  old_seed <- if (old_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (old_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  pooled <- lapply(seq_along(frames), function(index) {
    set.seed(one_imputation_seed(seed, index))
    selected <- sample.int(sizes[index], n_draws, replace = FALSE)
    frame <- frames[[index]][selected, , drop = FALSE]
    frame[[imputation_col]] <- index
    frame$.pooled_draw <- seq_len(n_draws)
    frame
  })
  out <- do.call(rbind, pooled)
  rownames(out) <- NULL
  out
}

pool_posterior_draws <- pool_equal_weight_draws
