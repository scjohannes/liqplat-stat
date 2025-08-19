run_simulation_iteration <- function(
  iter_num,
  base_path,
  full_data,
  model_formula,
  prior_spec = NULL,
  sample_size = NULL
) {
  # This function runs one full simulation iteration.

  # --- 1. Setup for the current iteration ---
  set.seed(123 + iter_num)
  iter_path <- file.path(base_path, glue("run_{iter_num}"))
  if (!dir.exists(iter_path)) {
    dir.create(iter_path, recursive = TRUE)
  }

  # for simulation from large sim data
  if (!is.null(sample_size)) {
    # Resample the full_data to the specified sample size
    ids <- full_data |>
      distinct(id) |>
      sample_n(sample_size, replace = FALSE) |>
      pull(id)

    full_data <- full_data |>
      filter(id %in% ids)
  }

  # Resample from data with no effect
  tx_ids <- full_data |>
    distinct(id) |>
    sample_frac(2 / 3) |>
    pull(id)

  data_for_model <- full_data |>
    mutate(
      tx = if_else(id %in% tx_ids, "1", "0"),
      tx = factor(tx, levels = c(0, 1))
    )

  rm(full_data)

  # --- 5. Save the data used for model fitting ---
  saveRDS(data_for_model, file.path(iter_path, "data_for_model.rds"))

  # --- 6. Fit the Model and Save ---

  # we'll stick to evaluating type I error rate via OR of tx for the time being, then we don't need to marginalize

  if (nrow(data_for_model) > 0) {
    options(mc.cores = 1)
    dd <- datadist(data_for_model)
    options(datadist = 'dd')

    # Forcefully place the 'dd' object into the global environment of the worker.
    assign("dd", dd, envir = .GlobalEnv)
    on.exit(rm(dd, envir = .GlobalEnv))

    blrm_args <- list(
      formula = model_formula,
      data = data_for_model,
      ppo = ~time,
      cppo = function(y) y,
      refresh = 0,
      iter = 2000,
      chains = 4,
      seed = 1234,
      loo = FALSE,
      method = "sampling"
    )

    if (!is.null(prior_spec)) {
      blrm_args$pcontrast <- prior_spec
    }

    model <- tryCatch(
      {
        do.call(blrm, blrm_args)
      },
      error = function(e) {
        message(glue("ERROR in iteration {iter_num}: {e$message}"))

        saveRDS(data_for_model, file.path(iter_path, "problematic_data.rds"))

        return(NULL)
      }
    )

    # --- 7. Save the results ONLY if the model ran successfully ---
    if (!is.null(model)) {
      # Save the full model object
      saveRDS(model, file.path(iter_path, "model_object.rds"))

      # Save just the MCMC draws to a separate file
      saveRDS(
        as.data.table(model$draws),
        file.path(iter_path, "model_draws.rds")
      )

      rhat_values <- stanDx(model) |>
        data.frame() |>
        rownames_to_column() |>
        select(-n_eff) |>
        pivot_wider(names_from = rowname, values_from = Rhat)

      saveRDS(rhat_values, file.path(iter_path, "rhat_values.rds"))

      ess_ratios <- stanDx(model) |>
        data.frame() |>
        rownames_to_column() |>
        select(-Rhat) |>
        pivot_wider(names_from = rowname, values_from = n_eff) |>
        mutate_all(~ . / (model$iter * model$chains))

      saveRDS(ess_ratios, file.path(iter_path, "ess_ratios.rds"))
    }
  }
  # Return a status message for progress tracking
  return(glue("Iteration {iter_num} completed."))
}
