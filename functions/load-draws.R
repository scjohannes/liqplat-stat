# Read and stack all draw files into a single data frame
load_draws <- function(path, glob = "*/model_draws.rds") {
  draw_files <- fs::dir_ls(
    path = path,
    glob = glob,
    recurse = TRUE
  )

  draws <- purrr::map_dfr(draw_files, readRDS, .id = "source") |>
    mutate(iter = as.integer(str_extract(source, "(?<=run_)\\d+"))) |>
    select(-source)

  return(draws)
}
