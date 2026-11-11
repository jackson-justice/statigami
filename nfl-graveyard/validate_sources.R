# =============================================================================
# NFL Graveyard: source cross-checks
#
# analysis.R checks that the combined dataset is internally consistent. This
# script checks the 1966-1998 source itself (FiveThirtyEight) against two
# others:
#   1. spreadspoke, an independent game file, for 1966-1998
#   2. nflverse, for the seasons both cover (1999-2020)
#
# It is not part of the pipeline and writes nothing but downloads. Run it
# after analysis.R, which downloads the FiveThirtyEight and nflverse files.
#
# Run from the statigami repo root (where statigami.Rproj lives):
#   source("nfl-graveyard/validate_sources.R")
# =============================================================================

library(tidyverse)

raw_dir <- file.path("nfl-graveyard", "data", "raw")

# spreadspoke (https://github.com/tobycrabtree/spreadspoke) has no license,
# so it is only read here for checking and never saved into the project data.
spreadspoke_url <- "https://raw.githubusercontent.com/tobycrabtree/spreadspoke/master/"

for (file in c("spreadspoke_scores.csv", "nfl_teams.csv")) {
  path <- file.path(raw_dir, file)
  if (!file.exists(path)) {
    download.file(paste0(spreadspoke_url, file), path, mode = "wb")
  }
}

five38 <- read_csv(file.path(raw_dir, "nfl_games_538.csv"), show_col_types = FALSE)
nflverse <- read_csv(file.path(raw_dir, "games.csv"), show_col_types = FALSE)

# Games are matched on date and the pair of teams, in either order, because
# the sources disagree on who is "home" at neutral sites.
with_pair <- function(games, team_a, team_b) {
  games |>
    mutate(
      pair = paste(pmin({{ team_a }}, {{ team_b }}),
                   pmax({{ team_a }}, {{ team_b }}))
    )
}


# =============================================================================
# 1. FiveThirtyEight vs spreadspoke, 1966-1998
# =============================================================================

# spreadspoke names teams as they were known that season; its team table maps
# every name to a franchise code, which matches FiveThirtyEight's but for WAS.
franchise_codes <- read_csv(
  file.path(raw_dir, "nfl_teams.csv"),
  show_col_types = FALSE
) |>
  transmute(team_name, code = if_else(team_id == "WAS", "WSH", team_id)) |>
  bind_rows(tibble(team_name = "Boston Patriots", code = "NE")) |>
  distinct()

spreadspoke <- read_csv(
  file.path(raw_dir, "spreadspoke_scores.csv"),
  col_types = cols(.default = "c")
) |>
  transmute(
    date = mdy(schedule_date),
    season = as.integer(schedule_season),
    home = franchise_codes$code[match(team_home, franchise_codes$team_name)],
    away = franchise_codes$code[match(team_away, franchise_codes$team_name)],
    home_score = as.integer(score_home),
    away_score = as.integer(score_away),
    playoff = schedule_playoff == "TRUE"
  ) |>
  filter(season >= 1966, season <= 1998) |>
  with_pair(home, away)

historical <- five38 |>
  filter(season >= 1966, season <= 1998) |>
  with_pair(team1, team2)

vs_spreadspoke <- historical |>
  full_join(spreadspoke, by = c("season", "date", "pair")) |>
  mutate(
    same_score = if_else(
      team1 == home,
      score1 == home_score & score2 == away_score,
      score1 == away_score & score2 == home_score
    ),
    same_playoff_flag = (playoff.x == 1) == playoff.y
  )

# The only games missing from spreadspoke should be the 42 played by
# replacement players during the 1987 strike.
only_538 <- vs_spreadspoke |>
  filter(is.na(home)) |>
  count(season, name = "games_only_in_538")


# =============================================================================
# 2. FiveThirtyEight vs nflverse, 1999-2020
# =============================================================================
# If the two agree on every game they share, FiveThirtyEight can be trusted
# for the seasons nflverse does not cover.

to_nflverse_code <- function(code, season) {
  case_when(
    code == "WSH" ~ "WAS",
    code == "LAR" & season <= 2015 ~ "STL",
    code == "LAR" ~ "LA",
    code == "LAC" & season <= 2016 ~ "SD",
    code == "OAK" & season >= 2020 ~ "LV",
    TRUE ~ code
  )
}

overlap_538 <- five38 |>
  filter(season >= 1999) |>
  mutate(
    team1 = to_nflverse_code(team1, season),
    team2 = to_nflverse_code(team2, season)
  ) |>
  with_pair(team1, team2)

overlap_nflverse <- nflverse |>
  filter(season >= 1999, season <= max(five38$season)) |>
  mutate(date = as.Date(gameday)) |>
  with_pair(home_team, away_team)

vs_nflverse <- overlap_538 |>
  full_join(overlap_nflverse, by = c("season", "date", "pair")) |>
  mutate(
    same_score = if_else(
      team1 == home_team,
      score1 == home_score & score2 == away_score,
      score1 == away_score & score2 == home_score
    ),
    same_playoff_flag = (playoff == 1) == (game_type != "REG")
  )


# =============================================================================
# 3. Results
# =============================================================================

source_checks <- tibble(
  check = c(
    "1966-1998: every spreadspoke game is in FiveThirtyEight",
    "1966-1998: only the 42 replacement games of 1987 are missing from spreadspoke",
    "1966-1998: every shared game has the same score",
    "1966-1998: every shared game has the same playoff flag",
    "1999-2020: FiveThirtyEight and nflverse list the same games",
    "1999-2020: every game has the same score",
    "1999-2020: every game has the same playoff flag"
  ),
  passed = c(
    !any(is.na(vs_spreadspoke$team1)),
    nrow(only_538) == 1 && only_538$season == 1987 &&
      only_538$games_only_in_538 == 42,
    all(vs_spreadspoke$same_score, na.rm = TRUE),
    all(vs_spreadspoke$same_playoff_flag, na.rm = TRUE),
    !any(is.na(vs_nflverse$team1)) && !any(is.na(vs_nflverse$game_id)),
    all(vs_nflverse$same_score, na.rm = TRUE),
    all(vs_nflverse$same_playoff_flag, na.rm = TRUE)
  )
)

print(source_checks, n = Inf)

message(
  nrow(filter(vs_spreadspoke, !is.na(home), !is.na(team1))),
  " games compared with spreadspoke, ",
  nrow(filter(vs_nflverse, !is.na(game_id), !is.na(team1))),
  " with nflverse."
)

if (!all(source_checks$passed)) {
  stop(
    "Source check failed:\n",
    paste("-", source_checks$check[!source_checks$passed], collapse = "\n")
  )
}
