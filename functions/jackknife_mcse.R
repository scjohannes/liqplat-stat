jackknife_mcse <- function(estimates, statistic = mean) {
  # Number of simulation repetitions
  nsim <- length(estimates)

  # Vector to store the "leave-one-out" estimates
  leave_one_out_estimates <- numeric(nsim)

  # Step 1: Create the "leave-one-out" estimates
  # Loop through each simulation run, remove it, and recalculate the statistic
  for (i in 1:nsim) {
    leave_one_out_estimates[i] <- statistic(estimates[-i])
  }

  # Step 2: Calculate the average of all the leave-one-out estimates
  mean_of_estimates <- mean(leave_one_out_estimates)

  # Step 3: Calculate the sum of squared differences
  sum_sq_diff <- sum((leave_one_out_estimates - mean_of_estimates)^2)

  # Step 4: Apply the final jackknife formula
  mcse <- sqrt(((nsim - 1) / nsim) * sum_sq_diff)

  return(mcse)
}
