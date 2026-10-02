# One plot style for every analysis notebook and the report.
#
# Packages the calling notebook must attach: tidyverse (ggplot2, dplyr, purrr,
# tibble); ggdist for plot_posterior(); ggsurvfit for plot_km_model();
# rstanarm for plot_time_varying_hr().
#
# - Theme: theme_bw(11), legend at the bottom, no minor grid (set globally).
# - Randomised groups: ctDNA blue (#76a7c8), Usual care red (#cf6c57); ctDNA
#   solid, Usual care dashed where groups are distinguished by line type.
# - States (QoL, TAOOH): ColorBrewer Dark2 (scale_colour_state(),
#   scale_fill_state(); replaces the viridis default of mostr::plot_sops()).
# - MCMC chains: four fixed colours; imputed values: pink, observed: black.
# - Posteriors: grey half-eye with 66% and 95% intervals above a dot plot.
# - Saved figures: 8 inches wide, PNG at 300 dpi and SVG (save_figure()).

theme_liqplat <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", colour = "grey70"),
      plot.title = element_text(size = rel(1), face = "bold")
    )
}

theme_set(theme_liqplat())

group_colours <- c("ctDNA" = "#76a7c8", "Usual care" = "#cf6c57")
group_linetypes <- c("ctDNA" = "solid", "Usual care" = "22")
uptake_colours <- c(
  "Usual care" = "#cf6c57",
  "ctDNA, accepted" = "#76a7c8",
  "ctDNA, declined" = "#2f5d7c",
  "ctDNA, not offered" = "#a9a9a9"
)
chain_colours <- c("#009E73", "#CC79A7", "#E69F00", "#56B4E9")
imputation_colours <- c("Observed" = "black", "Imputed" = "#CC79A7")
reference_line_colour <- "grey40"

scale_colour_group <- function(...) scale_colour_manual(values = group_colours, ...)
scale_fill_group <- function(...) scale_fill_manual(values = group_colours, ...)
scale_linetype_group <- function(...) scale_linetype_manual(values = group_linetypes, ...)
scale_colour_uptake <- function(...) scale_colour_manual(values = uptake_colours, ...)
scale_fill_uptake <- function(...) scale_fill_manual(values = uptake_colours, ...)

# Outcome states (QoL states 1-8, TAOOH states 1-5): Dark2, in state order.
state_colours <- function(n) RColorBrewer::brewer.pal(8, "Dark2")[seq_len(n)]
scale_colour_state <- function(...) scale_colour_brewer(palette = "Dark2", ...)
scale_fill_state <- function(...) scale_fill_brewer(palette = "Dark2", ...)

# Treatment indicator (0/1) as the group factor used in plots and tables.
group_factor <- function(tx) {
  factor(tx, levels = c(1, 0), labels = c("ctDNA", "Usual care"))
}

# Save a figure as PNG (300 dpi, for the PDF report) and SVG (for the HTML
# report) under the same name; `path` may end in either extension.
save_figure <- function(plot, path, height = 5, width = 8) {
  base <- tools::file_path_sans_ext(path)
  ggsave(paste0(base, ".png"), plot, width = width, height = height, dpi = 300)
  ggsave(paste0(base, ".svg"), plot, width = width, height = height)
}


# Posterior distribution of one quantity: half-eye above a dot plot of all
# draws (the ggdist default). `draws` is a
# numeric vector or a data frame with a `draw` column and optionally a `panel`
# column (one facet each, with `scales` passed to facet_wrap()). `reference =
# NULL` omits the reference line. With free scales, or when facets are added
# afterwards, `normalize = "panels"` scales each half-eye to its own panel.
plot_posterior <- function(draws, x_label, reference = 0, log_scale = FALSE,
                           scales = "fixed",
                           normalize = if (scales == "fixed") "all" else "panels") {
  if (is.numeric(draws)) draws <- tibble(draw = draws)
  plot <- ggplot(draws, aes(x = draw)) +
    ggdist::stat_dots(side = "bottom") +
    ggdist::stat_halfeye(
      .width = c(0.66, 0.95),
      point_interval = ggdist::median_qi,
      normalize = normalize
    ) +
    labs(x = x_label, y = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  if (!is.null(reference)) {
    plot <- plot +
      geom_vline(xintercept = reference, linetype = "dashed", colour = reference_line_colour)
  }
  if (log_scale) plot <- plot + scale_x_log10()
  if ("panel" %in% names(draws)) plot <- plot + facet_wrap(~panel, ncol = 1, scales = scales)
  plot
}


# Kaplan–Meier estimates (dashed steps, censoring marks) and model curves
# (solid median, 95% band) by randomised group, with a risk table every
# `risk_every` days up to `x_max`. `km_fit`: survfit2() fit stratified by
# group_factor(tx); `curves`: tx (0/1), time, median, lower, upper. Returns a
# ggsurvfit object; save it with save_figure(ggsurvfit_build(plot), ...).
plot_km_model <- function(km_fit, curves, x_max, risk_every = 100,
                          y_label = "Survival probability",
                          event_label = "Events") {
  risk_times <- seq(0, x_max, by = risk_every)
  curves <- mutate(curves, group = group_factor(tx))
  # theme = NULL: the report theme instead of ggsurvfit's own. linetype_aes
  # must be named, or `linetype` would partially match it.
  ggsurvfit::ggsurvfit(
    km_fit,
    linetype_aes = FALSE,
    linetype = "dashed",
    linewidth = 0.55,
    theme = NULL
  ) +
    geom_ribbon(
      data = curves,
      aes(x = time, ymin = lower, ymax = upper, fill = group),
      alpha = 0.16,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = curves,
      aes(x = time, y = median, colour = group),
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    ggsurvfit::add_censor_mark(size = 1.8) +
    scale_colour_group() +
    scale_fill_group() +
    scale_x_continuous(breaks = risk_times) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1)) +
    labs(x = "Days since random selection", y = y_label, colour = NULL, fill = NULL) +
    ggsurvfit::add_risktable(
      times = risk_times,
      risktable_stats = c("n.risk", "cum.event"),
      stats_label = list(n.risk = "At risk", cum.event = event_label)
    )
}

# Hazard ratio over time of the tve() treatment term of an rstanarm stan_surv
# fit: posterior median (line) and 95% credible interval (band), log scale.
plot_time_varying_hr <- function(fit) {
  plot(fit, plotfun = "tve")$data |>
    ggplot(aes(times, med)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed", colour = reference_line_colour) +
    scale_y_log10() +
    labs(x = "Days since random selection", y = "Hazard ratio, ctDNA versus usual care")
}


# Post-warm-up draws of `pars` in long format (iteration, chain, parameter,
# value) from an rstanarm, brms or rmsb fit.
trace_draws <- function(fit, pars) {
  draws <- if (inherits(fit, "brmsfit")) {
    as.array(fit, variable = pars)
  } else if (inherits(fit, "stanreg")) {
    as.array(fit, pars = pars)
  } else if (inherits(fit, "blrm")) {
    # rmsb stacks the chains (chain 1 first); the random-intercept SD is in omega.
    stacked <- cbind(fit$draws, fit$omega)[, pars, drop = FALSE]
    n_chains <- as.integer(fit$chains)
    array(
      stacked,
      dim = c(nrow(stacked) / n_chains, n_chains, length(pars)),
      dimnames = list(NULL, NULL, pars)
    )
  } else {
    stop("trace_draws() supports rstanarm, brms and rmsb fits.")
  }

  n_iterations <- dim(draws)[1]
  n_chains <- dim(draws)[2]
  tibble(
    iteration = rep(seq_len(n_iterations), times = n_chains * length(pars)),
    chain = rep(rep(seq_len(n_chains), each = n_iterations), times = length(pars)),
    parameter = rep(dimnames(draws)[[3]], each = n_iterations * n_chains),
    value = as.vector(draws)
  )
}

# Trace plot of draws in the format of trace_draws(), one panel per parameter.
plot_traces <- function(draws, ncol = NULL) {
  ggplot(draws, aes(iteration, value, colour = factor(chain))) +
    geom_line(linewidth = 0.2, alpha = 0.8) +
    facet_wrap(~parameter, scales = "free_y", ncol = ncol) +
    scale_colour_manual(values = chain_colours) +
    labs(x = "Iteration after warm-up", y = NULL, colour = "Chain")
}


# Mean and SD of the imputed values per MICE iteration, from one mids object
# (one line per imputation) or a list of mids objects with one imputation each
# (one line per list element). Passively derived variables are left out;
# factors are summarised by their integer codes.
mice_chain_stats <- function(imputations) {
  if (inherits(imputations, "mids")) {
    imputations <- list(imputations)
    per_run <- FALSE
  } else {
    per_run <- TRUE
  }
  imap(imputations, \(imp, run) {
    active <- imp$method != "" & !startsWith(imp$method, "~")
    imputed <- names(imp$method)[active & colSums(is.na(imp$data)) > 0]
    imputed <- intersect(imputed, dimnames(imp$chainMean)[[1]])
    map(c(Mean = "chainMean", SD = "chainVar"), \(component) {
      values <- imp[[component]][imputed, , , drop = FALSE]
      if (component == "chainVar") values <- sqrt(values)
      tibble(
        variable = rep(imputed, times = dim(values)[2] * dim(values)[3]),
        iteration = rep(rep(seq_len(dim(values)[2]), each = length(imputed)), times = dim(values)[3]),
        dataset = if (per_run) {
          as.integer(run)
        } else {
          rep(seq_len(dim(values)[3]), each = length(imputed) * dim(values)[2])
        },
        value = as.vector(values)
      )
    }) |>
      list_rbind(names_to = "statistic")
  }) |>
    list_rbind()
}

# Trace plot of mice_chain_stats() output: one row per variable, mean and SD.
plot_mice_traces <- function(stats) {
  ggplot(stats, aes(iteration, value, group = dataset, colour = factor(dataset))) +
    geom_line(linewidth = 0.3, alpha = 0.8) +
    facet_wrap(
      vars(variable, statistic),
      scales = "free_y",
      ncol = 2,
      labeller = label_wrap_gen(multi_line = FALSE)
    ) +
    scale_colour_viridis_d(guide = "none") +
    labs(x = "MICE iteration", y = NULL)
}

# Observed and imputed values of numeric `variables` (named vector: label =
# column) of a mids object with one column of imputed values per completed
# dataset (e.g. combined with ibind()), for plot_imputed_densities().
imputed_values <- function(imputation, variables) {
  observed <- imputation$data |>
    select(all_of(variables)) |>
    pivot_longer(everything(), names_to = "variable", values_drop_na = TRUE)
  # After ibind() all columns of imp are named "1": number them by position.
  imputed <- imputation$imp[variables] |>
    set_names(names(variables)) |>
    map(\(values) {
      values |>
        set_names(seq_along(values)) |>
        pivot_longer(everything(), names_to = "dataset")
    }) |>
    list_rbind(names_to = "variable")
  list(observed = observed, imputed = imputed)
}

# Category proportions of observed values and of the imputed values of every
# completed dataset, including categories with no imputed value (0%), for
# plot_imputed_categories(). `variables`: named vector, label = column.
imputed_category_proportions <- function(imputation, variables) {
  map(variables, \(variable) {
    observed <- imputation$data[[variable]]
    imputed <- imputation$imp[[variable]]
    categories <- if (is.factor(observed)) {
      levels(observed)
    } else {
      as.character(sort(unique(c(observed, unlist(imputed)))))
    }
    bind_rows(
      tibble(source = "Observed", dataset = "0", category = as.character(observed)),
      imputed |>
        set_names(seq_along(imputed)) |>
        mutate(across(everything(), as.character)) |>
        pivot_longer(everything(), names_to = "dataset", values_to = "category") |>
        mutate(source = "Imputed")
    ) |>
      drop_na(category) |>
      count(source, dataset, category) |>
      complete(nesting(source, dataset), category = categories, fill = list(n = 0)) |>
      mutate(
        proportion = n / sum(n),
        category = factor(category, levels = categories),
        .by = c(source, dataset)
      )
  }) |>
    list_rbind(names_to = "variable") |>
    # Combining variables orders the levels by appearance; sort numeric codes.
    mutate(category = if (anyNA(suppressWarnings(as.numeric(levels(category))))) {
      category
    } else {
      fct_inseq(category)
    })
}

# Densities of observed (black) and imputed values (pink, one line per completed
# dataset). `observed`: variable, value; `imputed`: variable, value, dataset.
plot_imputed_densities <- function(observed, imputed) {
  ggplot(mapping = aes(value)) +
    geom_density(
      data = imputed,
      aes(group = dataset, colour = "Imputed"),
      linewidth = 0.3
    ) +
    geom_density(data = observed, aes(colour = "Observed"), linewidth = 0.8) +
    facet_wrap(~variable, scales = "free") +
    scale_colour_manual(values = imputation_colours) +
    labs(x = NULL, y = "Density", colour = NULL)
}

# Category proportions of observed (black) and imputed values (pink, one point
# per completed dataset). `proportions`: variable, category, source
# ("Observed"/"Imputed"), dataset, proportion.
plot_imputed_categories <- function(proportions) {
  ggplot(proportions, aes(category, proportion, colour = source)) +
    geom_point(
      data = \(x) filter(x, source == "Imputed"),
      position = position_jitter(width = 0.12, height = 0, seed = 1),
      size = 1,
      alpha = 0.7
    ) +
    geom_point(data = \(x) filter(x, source == "Observed"), size = 2.5, shape = 18) +
    facet_wrap(~variable, scales = "free_x") +
    scale_colour_manual(values = imputation_colours) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = NULL, y = "Proportion", colour = NULL)
}
