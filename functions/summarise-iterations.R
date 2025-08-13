summarise_iterations <- function(data) {
  data |>
    summarise(
      mean_point_estimate = mean(point_estimate),
      mcse_point_estimate = jackknife_mcse(point_estimate),
      prob_rejection = mean(rejected),
      mcse_prob_rejection = jackknife_mcse(rejected),
      mean_bias = mean(bias),
      mcse_bias = jackknife_mcse(bias),
      mean_coverage = mean(coverage),
      mcse_coverage = jackknife_mcse(coverage),
      emp_SE = sd(point_estimate),
      mcse_emp_SE = jackknife_mcse(coverage, statistic = sd),
      mean_modSE = mean(mod_SE),
      mcse_modSE = jackknife_mcse(mod_SE)
    )
}
