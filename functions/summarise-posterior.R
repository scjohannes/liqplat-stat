summarise_posterior <- function(
  data,
  param_name,
  true_param_value,
  threshold = 0.95
) {
  data |>
    summarise(
      # Point estimate (posterior mean) for the log OR
      point_estimate = median({{ param_name }}),
      bias = point_estimate - true_param_value,
      mod_SE = sd({{ param_name }}),

      # 95% Credible Interval for the log OR
      lower_ci = quantile({{ param_name }}, probs = 0.025),
      upper_ci = quantile({{ param_name }}, probs = 0.975),
      coverage = if_else(
        lower_ci <= true_param_value & upper_ci >= true_param_value,
        1,
        0
      ),

      # Posterior probability that the log(OR) is less than 0
      p_benefit = mean({{ param_name }} < 0),
      rejected = if_else(p_benefit > threshold, 1, 0)
    )
}
