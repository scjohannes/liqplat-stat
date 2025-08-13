plot_p_benefit <- function(data, param = tx) {
  data |>
    group_by(iter) |>
    summarise(p_benefit = mean({{ param }} < 0)) |>
    ggplot() +
    aes(x = iter, y = p_benefit) +
    geom_point(alpha = 0.5) +
    theme_light() +
    labs(
      y = expression(P(beta[treatment] < 0)),
      x = "iteration"
    )
}
