load_rhat_values <- function(path) {
  rhat_files <- fs::dir_ls(
    path = path,
    glob = "*/rhat_values.rds",
    recurse = TRUE
  )

  # 2. Read and stack all draw files into a single data frame
  rhat <- purrr::map_dfr(rhat_files, readRDS, .id = "source") |>
    mutate(iter = as.integer(str_extract(source, "(?<=run_)\\d+"))) |>
    select(-source)

  return(rhat)
}
