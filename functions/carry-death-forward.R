library(dplyr)
library(tidyr)

carry_death_forward <- function(df) {
  df_state_8 <-
    df |>
    group_by(id) |>
    mutate(
      y = as.integer(y),
      yprev = as.integer(yprev),
      death_time = ifelse(any(y == 8), time[y == 8], NA_real_)
    ) |>
    filter(y == 8) |>
    complete(time = death_time:26, fill = list(y = 8L, yprev = 8L)) |>
    arrange(id, time) |>
    fill(everything())

  df_state_1_to_7 <-
    df |>
    mutate(y = as.integer(y), yprev = as.integer(yprev)) |>
    filter(y != 8) |>
    arrange(id, time)

  df_forward <-
    bind_rows(df_state_1_to_7, df_state_8) |>
    mutate(y = as.ordered(y), yprev = as.factor(yprev)) |>
    arrange(id, time)

  return(df_forward)
}
