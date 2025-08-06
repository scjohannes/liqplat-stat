library(dplyr)
library(tidyr)

censor_qol_data <- function(df, percentage_to_keep = 0.15) {
  # Separate first row and rows where y == 8
  first_rows <- df |>
    group_by(id) |>
    arrange(id, time) |>
    slice_head(n = 1)

  death_rows <- df |>
    filter(y == '8')

  # Take random sample of other rows
  sample_rows <- df |>
    # anti_join(first_rows) |> # Specify 'by' argument to avoid "joining by conflicting types" warning
    anti_join(death_rows) |>
    ungroup() |>
    slice_sample(prop = percentage_to_keep) |>
    arrange(id, time)

  # Recompute yprev and add gap
  baseline <- first_rows |>
    mutate(
      y = yprev,
      time = 0
    ) |>
    select(-yprev)

  follow_up <- sample_rows |>
    bind_rows(death_rows) |>
    select(-yprev) |>
    bind_rows(baseline) |>
    arrange(id, time)

  follow_up <- follow_up |>
    group_by(id) |>
    mutate(
      yprev = case_when(
        TRUE ~ lag(y, n = 1, order_by = time)
      ),
      yprev = factor(yprev, levels = 1:7, ordered = FALSE), # death = 8 doesn't exist in yprev
      timeprev = case_when(
        TRUE ~ lag(time, n = 1, order_by = time)
      ),
      gap = time - timeprev
    ) |>
    slice(-1) |> # remove baseline without prior state
    ungroup()

  # Create censored dataset
  df_censored <- follow_up |>
    select(-y, -yprev, -time, -timeprev, -gap) |>
    distinct() |>
    mutate(time = list(1:26)) |>
    unnest(time) |>
    left_join(
      follow_up,
      by = c(
        "id",
        "tx",
        "gender",
        "pat_age",
        "ecog_fstcnt",
        "diagnosis",
        "plan_fstcnt_coded",
        "status",
        "time"
      )
    ) |>
    mutate(
      yprev = as.numeric(as.character(yprev)),
      y = as.numeric(as.character(y))
    ) |>
    group_by(id) |>
    fill(yprev, .direction = "up") |>
    mutate(
      yprev = case_when(
        is.na(yprev) ~ lag(y, n = 1, order_by = time),
        TRUE ~ yprev
      ),
      y.a = case_when(
        !is.na(y) ~ y,
        TRUE ~ 1
      ),
      y.b = case_when(
        !is.na(y) ~ y,
        TRUE ~ 7
      )
    ) |>
    fill(yprev, .direction = "down") |>
    mutate(gap_group = cumsum(lag(!is.na(y), default = FALSE))) |>
    group_by(id, gap_group) |>
    mutate(gap = row_number()) |>
    ungroup() |>
    filter(yprev != 8) |>
    select(-gap_group, -timeprev) |>
    mutate(
      ecog_fstcnt = factor(ecog_fstcnt, ordered = FALSE), # rmsb can't handle ordered factor for predictions later on
      yprev = factor(yprev) # absolutely needs to be a factor
    )

  return(df_censored)
}
