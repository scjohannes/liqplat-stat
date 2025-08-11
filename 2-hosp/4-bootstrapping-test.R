# Load necessary libraries
library(tidyverse)
library(data.table)
library(arrow)
library(rms)
library(VGAM)
library(furrr)
library(Hmisc)

# --- 1. Load and Prepare Data ---
# Load the dataset from a Parquet file
df <- read_parquet("./data/hosp_long_weeks.parquet") |>
  mutate(
    tx = factor(tx),
    ecog_fstcnt = factor(ecog_fstcnt, ordered = FALSE)
  ) |>
  # Select only the columns needed for the analysis
  select(
    pat_id,
    tx,
    y,
    yprev,
    week,
    ecog_fstcnt,
    diagnosis,
    age,
    gender,
    albumin,
    c_reactive_protein
  )

# --- 2. Define Helper Function for SOP Calculation ---
# This function calculates State Occupancy Probabilities for a given model and baseline data.
# It is designed to be called for each patient.
calculate_sop_in_memory <- function(i, tx, baseline_df, model) {
  # Select the specific patient's baseline data by row index
  row <- baseline_df[i, ]

  # Calculate State Occupancy Probabilities using the provided Markov model function
  sops_array <- soprobMarkovOrdm(
    model,
    data = list(
      tx = tx,
      ecog_fstcnt = row$ecog_fstcnt,
      diagnosis = row$diagnosis,
      yprev = row$yprev
    ),
    times = 1:26,
    ylevels = 1:5,
    absorb = 5,
    tvarname = "week",
    pvarname = "yprev"
  )

  # Convert the array output to a tidy data.table
  sops_dt <- as.data.table(sops_array) |>
    rownames_to_column(var = "rowname") |>
    pivot_longer(
      names_to = "state",
      values_to = "sop",
      cols = c("1", "2", "3", "4", "5")
    ) |>
    mutate(week = as.numeric(rowname)) |>
    select(-rowname) |>
    mutate(
      tx = tx,
      id = row$pat_id # Use the actual patient ID
    )

  return(sops_dt)
}


# --- 3. Define the Main Bootstrap Iteration Function ---
# This function encapsulates one full bootstrap replicate:
# 1. Resamples patients
# 2. Refits the model
# 3. Calculates SOPs for the new model
run_bootstrap_iteration <- function(iter, full_df, unique_ids) {
  # Step 3.1: Resample patients with replacement
  # This creates a bootstrap sample of patient IDs.
  boot_pat_ids <- sample(unique_ids, length(unique_ids), replace = TRUE)

  # Create the bootstrap dataframe by taking all rows for the sampled patients.
  # We use a join to efficiently construct the new dataset.
  df_boot <- as.data.table(full_df)[
    .(pat_id = boot_pat_ids),
    on = 'pat_id'
  ]

  # Step 3.2: Fit the model on the bootstrap sample
  # The model is refit for each bootstrap replicate.
  model_boot <- vglm(
    ordered(y) ~ tx + rms::rcs(week, 4) + ecog_fstcnt + diagnosis,
    cumulative(reverse = TRUE, parallel = FALSE ~ rms::rcs(week, 4)),
    data = df_boot,
    # Use 'half.stepging' for more robust convergence with bootstrap samples
    control = vglm.control(maxit = 50, stepsize = 0.5)
  )

  # Step 3.3: Calculate SOPs for the bootstrapped model
  # Get the baseline data (week 1) from the current bootstrap sample
  baseline_df_boot <- df_boot[week == 1, ]

  # Calculate SOPs for the Standard of Care (SoC) arm ("0")
  sops_soc_boot <- map_dfr(
    1:nrow(baseline_df_boot),
    \(x) calculate_sop_in_memory(x, "0", baseline_df_boot, model_boot)
  )

  # Calculate SOPs for the Treatment (Tx) arm ("1")
  sops_tx_boot <- map_dfr(
    1:nrow(baseline_df_boot),
    \(x) calculate_sop_in_memory(x, "1", baseline_df_boot, model_boot)
  )

  # Step 3.4: Aggregate results for this iteration
  # Combine SoC and Tx results and calculate the mean SOP across patients
  sop_df_boot <-
    bind_rows(sops_soc_boot, sops_tx_boot) |>
    mutate(tx = as.factor(tx)) |>
    group_by(state, week, tx) |>
    # Calculate the mean SOP for this bootstrap replicate
    summarise(sop = mean(sop), .groups = 'drop') |>
    # Add the bootstrap replicate number for tracking
    mutate(replicate = iter)

  return(sop_df_boot)
}


# --- 4. Run the Parallelized Bootstrap ---
# Set the number of bootstrap replicates
B <- 100
# Get unique patient IDs for resampling
unique_patient_ids <- unique(df$pat_id)

# Set up the parallel processing plan using 'callr' workers
# 'callr' creates fresh, clean R sessions for each worker, which is more robust.
future::plan(future.callr::callr, workers = 8)

# Use furrr to run the bootstrap iterations in parallel
bootstrap_results <- furrr::future_map_dfr(
  .x = 1:B,
  .f = \(i) run_bootstrap_iteration(i, df, unique_patient_ids),
  # Provide progress bar and specify necessary packages for the workers
  .progress = TRUE,
  .options = furrr_options(
    packages = c("VGAM", "rms", "dplyr", "tidyr", "data.table", "tibble"),
    seed = TRUE # Ensures reproducibility
  )
)

# --- 5. Summarize and Analyze Bootstrap Results ---
# Calculate the mean, and lower/upper 95% confidence intervals from the bootstrap distribution
sop_summary <- bootstrap_results |>
  group_by(state, week, tx) |>
  summarise(
    sop_mean = mean(sop, na.rm = TRUE),
    sop_lower_ci = quantile(sop, 0.025, na.rm = TRUE),
    sop_upper_ci = quantile(sop, 0.975, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the first few rows of the final summary table
print(head(sop_summary))

# Example of how you could plot the results for one state
# ggplot(filter(sop_summary, state == 1), aes(x = week, y = sop_mean, color = tx)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = sop_lower_ci, ymax = sop_upper_ci, fill = tx), alpha = 0.2) +
#   labs(
#     title = "State 1 Occupancy Probability with 95% Confidence Intervals",
#     x = "Week",
#     y = "State Occupancy Probability"
#   ) +
#   theme_minimal()
