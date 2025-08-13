plot_rhat <- function(data, y = tx) {
  data |>
    ggplot() +
    aes(x = iter, y = {{ y }}) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", color = "red") +
    labs(
      x = "Iteration",
      y = expression(beta["treatment"])
    )
}
