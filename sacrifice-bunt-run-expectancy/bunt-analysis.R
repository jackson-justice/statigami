# Project: Sacrifice Bunt Run Expectancy
# Date: 2026-05-06

library(tidyverse)
library(baseballr)

game <- mlb_game_pks(date = "2026-05-02", level_ids = c(11))

game |>
  select(game_pk,
         officialDate,
         teams.away.team.name,
         teams.home.team.name,
         venue.name)

bunt_analysis_pk <- 814938

game_pbp <- mlb_pbp(bunt_analysis_pk)

glimpse(game_pbp)
names(game_pbp)

game_pbp |>
  distinct(result.eventType) |>
  arrange(result.eventType)

game_pbp |> 
  count(last.pitch.of.ab)

game_pbp_clean <- game_pbp |> 
  filter(last.pitch.of.ab == "true")

game_pbp_clean |>
  filter(result.eventType == "sac_bunt") |>
  select(
    about.inning,
    about.halfInning,
    count.outs.start,
    result.description,
    matchup.postOnFirst.fullName,
    matchup.postOnSecond.fullName,
    matchup.postOnThird.fullName
  )
