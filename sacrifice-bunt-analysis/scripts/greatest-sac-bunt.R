library(tidyverse)

sac_bunts_2023_2026 |>
  arrange(desc(about.captivatingIndex)) |>
  select(
    game_date,
    batting_team,
    matchup.batter.fullName,
    result.description,
    about.inning,
    about.halfInning,
    result.awayScore,
    result.homeScore,
    about.captivatingIndex
  ) |> 
  filter(about.captivatingIndex == 41) |> 
  write.csv(
    file = "sacrifice-bunt-analysis/data/exciting_bunts")

exciting_bunts <- read.csv("sacrifice-bunt-analysis/data/exciting_bunts")


exciting_bunts |> 
  filter(about.inning >= 8) |> 
  filter(abs(result.awayScore - result.homeScore) <= 1)

exciting_bunts

# GOAT sac bunt candidate
sac_bunts_2023_2026 |>
  filter(
    matchup.batter.fullName == "Brenton Doyle",
    game_date == "2025-07-28"
  ) |>
  select(
    game_date,
    final_away_score,
    final_home_score
  )


# Brenton Doyle's bunts
sac_bunts_2023_2026 |> 
  filter(matchup.batter.fullName == "Brenton Doyle") |> 
  select(
    game_date,
    matchup.batter.fullName,
    about.inning,
    about.halfInning,
    about.captivatingIndex
  )


