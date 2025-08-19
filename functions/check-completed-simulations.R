check_completed_simulations <- function(output_path, total_sims) {
  completed_files <- fs::dir_ls(
    path = output_path,
    glob = "*/model_draws.rds",
    recurse = TRUE
  )

  completed_iters <- integer(0)
  if (length(completed_files) > 0) {
    completed_iters <- stringr::str_extract(completed_files, "(?<=run_)\\d+") |>
      as.integer() |>
      na.omit() |>
      unique()
  }

  all_possible_iters <- 1:total_sims
  iters_to_run <- setdiff(all_possible_iters, completed_iters)

  return(list(
    iters_to_run = iters_to_run,
    n_to_run = length(iters_to_run),
    n_completed = length(completed_iters)
  ))
}
