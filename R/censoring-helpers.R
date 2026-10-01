# Deterministic covariate completion for the adjusted censoring diagnostic.
prepare_censoring_data <- function(cohort) {
  required <- c("id", "tx", "survival_time_days_unrestricted", "status_death_unrestricted",
                "ecog_fstcnt", "stage_binary", "diagnosis", "albumin", "c_reactive_protein")
  stopifnot(all(required %in% names(cohort)), !anyDuplicated(cohort$id),
            !anyNA(cohort[c("id", "tx", "survival_time_days_unrestricted",
                           "status_death_unrestricted", "stage_binary", "diagnosis")]),
            all(cohort$tx %in% 0:1), all(cohort$status_death_unrestricted %in% 0:1),
            all(cohort$stage_binary %in% 0:1),
            all(is.finite(cohort$survival_time_days_unrestricted)),
            all(cohort$survival_time_days_unrestricted >= 0),
            all(na.omit(cohort$ecog_fstcnt) %in% 0:4))
  data <- data.frame(id = cohort$id, tx = cohort$tx,
    follow_up_days = cohort$survival_time_days_unrestricted,
    censoring_event = as.integer(1L - cohort$status_death_unrestricted),
    ecog_binary = derive_ecog_binary(cohort$ecog_fstcnt),
    stage_binary = cohort$stage_binary, diagnosis = factor(cohort$diagnosis),
    mgps = derive_mgps(cohort$albumin, cohort$c_reactive_protein))
  replacements <- list()
  for (variable in c("ecog_binary", "mgps")) {
    x <- data[[variable]]
    frequencies <- table(x)
    if (!length(frequencies)) stop("No observed values for modal replacement: ", variable)
    # table orders the numeric categories: ties select the lowest category.
    mode <- as.numeric(names(frequencies)[which.max(frequencies)])
    data[[variable]][is.na(x)] <- mode
    replacements[[variable]] <- data.frame(variable = variable, missing = sum(is.na(x)), replacement = mode)
  }
  data$mgps <- factor(data$mgps, levels = 0:2)
  stopifnot(!anyNA(data))
  list(data = data, replacements = do.call(rbind, replacements))
}

# The observed and administrative-only comparisons use identical adjustment and priors.
analyze_adjusted_censoring <- function(observed, artifact_dir, result_dir,
                                      max_follow_up = max(observed$follow_up_days)) {
  figure_dir <- file.path(artifact_dir, "figures")
  model_dir <- file.path(artifact_dir, "models")
  for (directory in c(result_dir, figure_dir, model_dir)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  fitting_data <- observed |> filter(follow_up_days > 0)

  stopifnot(all(levels(observed$diagnosis) %in% as.character(fitting_data$diagnosis)))

  model_path <- file.path(model_dir, "model-01.rds")
  input_hash <- digest::digest(list(fitting_data, "adjusted-censoring-v1-normal-tx-5-no-autoscale"))
  model <- if (file.exists(model_path)) readRDS(model_path) else NULL

  if (is.null(model) || !identical(attr(model, "input_hash"), input_hash) || identical(Sys.getenv("LIQPLAT_FORCE"), "1")) {
   
   model <- rstanarm::stan_surv(
      survival::Surv(follow_up_days, censoring_event) ~
        tx + ecog_binary + stage_binary + mgps + (1 | diagnosis),
      data = fitting_data, basehaz = "ms", basehaz_ops = list(df = 5),
      prior = rstanarm::normal(location = rep(0, 5), scale = c(5, rep(2.5, 4)), autoscale = FALSE),
      prior_intercept = rstanarm::normal(0, 20, autoscale = FALSE),
      prior_aux = rstanarm::dirichlet(1), prior_covariance = rstanarm::decov(),
      chains = 4L, cores = 4L, iter = 2000L, warmup = 1000L,
      adapt_delta = 0.99, seed = 4401L, refresh = 100L)
    
    attr(model, "input_hash") <- input_hash
    saveRDS(model, model_path, compress = "xz")
  }

  parameters <- posterior::summarise_draws(posterior::as_draws_array(model)) |> as.data.frame()
  sampler <- rstan::get_sampler_params(model$stanfit, inc_warmup = FALSE)

  diagnostics <- tibble(
    fits = 1L, multiple_imputations = 0L, mice_maxit = NA_integer_,
    randomized = nrow(observed), fitted = nrow(fitting_data),
    max_rhat = max(parameters$rhat, na.rm = TRUE),
    min_ess_bulk = min(parameters$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(parameters$ess_tail, na.rm = TRUE),
    divergences = sum(vapply(sampler, function(x) sum(x[, "divergent__"]), numeric(1))),
    treedepth_hits = sum(vapply(sampler, function(x) sum(x[, "treedepth__"] >= 10), numeric(1))),
    min_bfmi = min(vapply(sampler, function(x) mean(diff(x[, "energy__"])^2) / var(x[, "energy__"]), numeric(1))),
    chains = model$stanfit@sim$chains, iterations = model$stanfit@sim$iter,
    warmup = model$stanfit@sim$warmup)

  arrow::write_parquet(diagnostics, file.path(result_dir, "adjusted-diagnostics.parquet"))

  stopifnot(diagnostics$max_rhat <= 1.01, diagnostics$min_ess_bulk >= 400,
    diagnostics$min_ess_tail >= 400, diagnostics$divergences == 0,
    diagnostics$treedepth_hits == 0, diagnostics$min_bfmi >= .3)

  hr <- exp(as.matrix(model, pars = "tx"))[, 1]

  adjusted_summary <- tibble(median = median(hr), lower_95 = quantile(hr, .025),
    upper_95 = quantile(hr, .975), probability_higher_censoring = mean(hr > 1))

  arrow::write_parquet(adjusted_summary, file.path(result_dir, "adjusted-hazard-ratio-summary.parquet"))
  arrow::write_parquet(parameters, file.path(result_dir, "adjusted-parameter-summary.parquet"))

  cohort_summary <- observed |> summarise(participants = n(), censoring_events = sum(censoring_event),
    deaths = sum(1L - censoring_event), zero_follow_up = sum(follow_up_days == 0), .by = tx)

  arrow::write_parquet(cohort_summary, file.path(result_dir, "adjusted-cohort-summary.parquet"))
  curves <- posterior_standardized_survival_draws(model, observed, horizon = max_follow_up,
    n_points = 200L, n_draws = 1000L, seed = 4402L)

  curve_summary <- curves |> summarise(median = median(survival),
    lower = quantile(survival, .025), upper = quantile(survival, .975), .by = c(tx, time))

  stopifnot(all(curves$survival >= 0 & curves$survival <= 1),
    all(curves |> group_by(tx, draw) |> summarise(ok = all(diff(survival) <= 1e-10), .groups = "drop") |> pull(ok)))

  newdata <- observed |> mutate(tx = 1)

  individual <- posterior_survfit_compatible(model, newdata = newdata, times = 182,
    extrapolate = FALSE, standardise = FALSE, return_matrix = TRUE, draws = 100L, seed = 4403L)[[1]]

  standardized <- posterior_survfit_compatible(model, newdata = newdata, times = 182,
    extrapolate = FALSE, standardise = TRUE, return_matrix = TRUE, draws = 100L, seed = 4403L)[[1]]

  contrast_data <- newdata[rep(1L, nlevels(newdata$diagnosis)), ]

  contrast_data$diagnosis <- factor(levels(newdata$diagnosis), levels = levels(newdata$diagnosis))

  diagnosis_predictions <- posterior_survfit_compatible(model, newdata = contrast_data, times = 182,
    extrapolate = FALSE, standardise = FALSE, return_matrix = TRUE, draws = 100L, seed = 4403L)[[1]]

  checks <- tibble(maximum_averaging_error = max(abs(rowMeans(individual) - as.numeric(standardized))),
    maximum_diagnosis_contrast = max(apply(diagnosis_predictions, 1, function(x) diff(range(x)))))

  stopifnot(checks$maximum_averaging_error < 1e-10, checks$maximum_diagnosis_contrast > 1e-8)

  arrow::write_parquet(curve_summary, file.path(result_dir, "adjusted-censoring-curves.parquet"))
  arrow::write_parquet(checks, file.path(result_dir, "adjusted-prediction-checks.parquet"))

  observed$tx_plot <- factor(observed$tx, levels = 0:1, labels = c("Usual care", "Selected for invitation"))

  curve_summary$tx_plot <- factor(curve_summary$tx, levels = 0:1, labels = levels(observed$tx_plot))

  km <- ggsurvfit::survfit2(survival::Surv(follow_up_days, censoring_event) ~ tx_plot,
    data = observed, conf.type = "log-log")

  reverse_plot <- ggsurvfit::ggsurvfit(km, linetype_aes = FALSE, linetype = "dashed") +
    ggsurvfit::add_censor_mark() +
    geom_ribbon(data = curve_summary, aes(x = time, ymin = lower, ymax = upper, fill = tx_plot),
      inherit.aes = FALSE, alpha = .15) +
    geom_line(data = curve_summary, aes(x = time, y = median, colour = tx_plot), inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(0, max_follow_up, by = 100)) +
    coord_cartesian(xlim = c(0, max_follow_up), ylim = c(0, 1)) +
    labs(x = "Days since randomization", y = "Probability of remaining uncensored", colour = NULL, fill = NULL) +
    theme_bw() + ggsurvfit::add_risktable(times = seq(0, max_follow_up, by = 100),
      risktable_stats = c("n.risk", "cum.event"),
      stats_label = list(n.risk = "At risk", cum.event = "Censoring"))

  ggsave(file.path(figure_dir, "adjusted-reverse-kaplan-meier.png"),
    ggsurvfit::ggsurvfit_build(reverse_plot), width = 9, height = 7, dpi = 300)

  trace_plot <- bayesplot::mcmc_trace(posterior::as_draws_array(model), pars = c("tx", "ecog_binary", "stage_binary", "mgps1", "mgps2"))

  ggsave(file.path(figure_dir, "adjusted-censoring-traces.png"), trace_plot, width = 10, height = 7, dpi = 180)
  list(diagnostics = diagnostics, parameters = parameters, checks = checks,
       adjusted_summary = adjusted_summary, cohort_summary = cohort_summary,
       curve_summary = curve_summary, reverse_plot = reverse_plot, trace_plot = trace_plot)
}
