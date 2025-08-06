write_SOP <- function(i, tx, baseline_df, path) {
  row <- baseline_df[i]

  sops <-
    soprobMarkovOrdm(
      model,
      data = list(
        tx = tx,
        ecog_fstcnt = row$ecog_fstcnt,
        diagnosis = row$diagnosis,
        yprev = row$yprev,
        gap = 1
      ),
      times = 1:26,
      ylevels = 1:8,
      absorb = 8,
      tvarname = "time",
      pvarname = "yprev"
    )

  # Because of the size of the data, only use the first 500 MCMC draws
  sops <- sops[1:100, , ]

  sops <- as.data.table(sops)

  # Rename columns
  setnames(
    sops,
    old = c("V1", "V2", "V3", "value"),
    new = c("draw", "time", "state", "sop")
  )

  sops$time <- as.integer(sops$time)
  sops$state <- as.factor(sops$state)
  sops$tx <- tx
  sops$i <- i

  folder <- file.path(
    path,
    glue::glue("/marginalized_sop_{tx}"),
    glue::glue("/msop_{i}.parquet")
  )

  # Create the directory if it doesn't exist
  dir_to_create <- dirname(folder)
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }

  arrow::write_parquet(
    x = sops,
    sink = folder
  )
}
