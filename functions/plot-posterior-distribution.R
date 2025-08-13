plot_posterior_distribution <- function(
  data,
  x_variable,
  subtitle_type = "H0",
  hline_intercept = log(1)
) {
  if (!subtitle_type %in% c("H0", "HA")) {
    stop("subtitle_type must be either 'H0' or 'HA'")
  }

  subtitle_expression <- if (subtitle_type == "H0") {
    expression(Stacked ~ Posterior ~ Draws ~ under ~ H[0])
  } else {
    expression(Stacked ~ Posterior ~ Draws ~ under ~ H[A])
  }

  x_label_expression <- expression(beta[treatment])

  ggplot(data, aes(x = {{ x_variable }})) +
    ggdist::stat_dist_halfeye(fill = "orange") +
    geom_vline(
      xintercept = hline_intercept,
      linetype = "dashed",
      color = "blue"
    ) +
    labs(
      subtitle = subtitle_expression,
      x = x_label_expression,
      y = NULL
    )
}
