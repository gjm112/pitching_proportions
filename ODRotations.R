library(tidyverse)
library(Lahman)

game_logs <- data.frame()
for (yr in 21:24) {
  filename <- paste0("gl20", yr, ".txt")
  temp <- read.csv(filename, header = FALSE, stringsAsFactors = FALSE)
  game_logs <- bind_rows(game_logs, temp)
}

game_logs |>
  select(1, 4, 7, 102:105) |>
  rename(
    date = V1,
    visiting_team = V4,
    home_team = V7,
    visiting_id = V102,
    visiting_name = V103,
    home_id = V104,
    home_name = V105
  ) |>
  pivot_longer(
    cols = c(visiting_team, home_team,
             visiting_name, home_name,
             visiting_id, home_id),
    names_to = c("side", ".value"),
    names_pattern = "^(visiting|home)_(.*)$"
  ) |>
  mutate(
    year = substr(date, 1, 4),
    month = substr(date, 5, 6),
    day = substr(date, 7, 8)
  ) |>
  filter(
    !date %in% c(20240320, 20240321)
  ) |>
  group_by(year, team) |>
  arrange(date, .by_group = T) |>
  filter(!duplicated(name)) |>
  slice_head(n = 5) |>
  mutate(rotation_spot = row_number()) |>
  ungroup() |>
  pivot_wider(
    id_cols = c(year, team),
    names_from = rotation_spot,
    values_from = c(name, id),
    names_glue = "SP{rotation_spot}_{.value}"
  ) |> view()

name_dict <- People |>
  filter(finalGame > 2021-03-01) |>
  select(playerID, retroID) 
