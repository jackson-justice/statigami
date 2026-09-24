# =============================================================================
# NFL Championship Trajectories
#
# Question: How strongly does an NFL team's record at each point in the
# regular season relate to eventually winning the Super Bowl?
#
# This script:
#   1. Downloads historical game results from nflverse
#   2. Builds a team-season-week panel of cumulative W/L/T
#   3. Flags each season's eventual Super Bowl champion
#   4. Runs validation checks on the panel
#   5. Takes a first descriptive look at the "2-0 start" claim
#
# Run from the statigami repo root (where statigami.Rproj lives).
# =============================================================================

library(tidyverse)

project_dir <- "nfl-championship-trajectories"
raw_dir <- file.path(project_dir, "data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# Last season with a completed Super Bowl. Update after each Super Bowl.
last_complete_season <- 2025


# =============================================================================
# 1. Source data
# =============================================================================
# nflverse games file: one row per game, 1999-present, regular season and
# playoffs, maintained by Lee Sharpe / nflverse and cross-referenced to
# Pro-Football-Reference game IDs (pfr column).
# https://github.com/nflverse/nfldata

games_url <- "https://github.com/nflverse/nfldata/raw/master/data/games.csv"
games_raw_path <- file.path(raw_dir, "games.csv")

if (!file.exists(games_raw_path)) {
  download.file(games_url, games_raw_path, mode = "wb")
}

games_raw <- read_csv(games_raw_path, show_col_types = FALSE)

games <- games_raw |>
  filter(season <= last_complete_season) |>
  select(
    game_id, season, game_type, week, gameday,
    away_team, away_score, home_team, home_score, result
  )


# =============================================================================
# 2. Super Bowl champions
# =============================================================================

champions <- games |>
  filter(game_type == "SB") |>
  transmute(
    season,
    champion = if_else(home_score > away_score, home_team, away_team),
    runner_up = if_else(home_score > away_score, away_team, home_team)
  )

playoff_teams <- games |>
  filter(game_type != "REG") |>
  pivot_longer(c(home_team, away_team), values_to = "team") |>
  distinct(season, team)


# =============================================================================
# 3. Team-game results (regular season only)
# =============================================================================
# Each game becomes two rows, one per team. Ties count as a tie for both.

team_games <- games |>
  filter(game_type == "REG") |>
  mutate(home_opp = away_team, away_opp = home_team) |>
  pivot_longer(
    c(home_team, away_team),
    names_to = "home_away",
    values_to = "team"
  ) |>
  mutate(
    home_away = str_remove(home_away, "_team"),
    opponent = if_else(home_away == "home", home_opp, away_opp),
    points_for = if_else(home_away == "home", home_score, away_score),
    points_against = if_else(home_away == "home", away_score, home_score),
    outcome = case_when(
      points_for > points_against ~ "W",
      points_for < points_against ~ "L",
      TRUE ~ "T"
    )
  ) |>
  select(season, week, team, opponent, home_away, points_for, points_against, outcome)


# =============================================================================
# 4. Team-season-week panel
# =============================================================================
# One row per team per regular-season week, INCLUDING bye weeks. On a bye the
# record carries forward and games_played does not change. This lets us ask
# the question two ways:
#   - by calendar week   ("record after week 4")
#   - by games played    ("record after 2 games", i.e. the 2-0 claim)

season_weeks <- team_games |>
  group_by(season) |>
  summarise(n_weeks = max(week), .groups = "drop")

team_weeks <- team_games |>
  distinct(season, team) |>
  left_join(season_weeks, by = "season") |>
  mutate(week = map(n_weeks, seq_len)) |>
  unnest(week) |>
  select(-n_weeks)

panel <- team_weeks |>
  left_join(team_games, by = c("season", "week", "team")) |>
  arrange(season, team, week) |>
  group_by(season, team) |>
  mutate(
    played = !is.na(outcome),
    games_played = cumsum(played),
    wins = cumsum(coalesce(outcome == "W", FALSE)),
    losses = cumsum(coalesce(outcome == "L", FALSE)),
    ties = cumsum(coalesce(outcome == "T", FALSE)),
    win_pct = if_else(games_played > 0, (wins + 0.5 * ties) / games_played, NA_real_),
    point_diff = cumsum(coalesce(points_for - points_against, 0)),
    final_wins = last(wins),
    final_losses = last(losses),
    final_ties = last(ties)
  ) |>
  ungroup() |>
  left_join(champions |> select(season, champion), by = "season") |>
  mutate(
    is_bye = !played,
    record = if_else(ties > 0, paste(wins, losses, ties, sep = "-"), paste(wins, losses, sep = "-")),
    sb_champion = team == champion,
    made_playoffs = paste(season, team) %in% paste(playoff_teams$season, playoff_teams$team)
  ) |>
  select(
    season, week, team, is_bye, opponent, home_away, outcome,
    games_played, wins, losses, ties, record, win_pct, point_diff,
    final_wins, final_losses, final_ties, made_playoffs, sb_champion
  )


# =============================================================================
# 5. Validation checks
# =============================================================================
# Every check must pass before the panel is saved.

team_seasons <- panel |>
  group_by(season, team) |>
  summarise(
    games = max(games_played),
    byes = sum(is_bye),
    weeks = n(),
    final_w = max(wins),
    final_l = max(losses),
    final_t = max(ties),
    champion = any(sb_champion),
    .groups = "drop"
  )

# Scheduled regular-season length by era
expected_games <- function(season) if_else(season >= 2021, 17L, 16L)

# Known exception: 2022 BUF at CIN (Week 17) was suspended after Damar Hamlin's
# cardiac arrest and never completed. The league cancelled it, so both teams
# played 16 games. The game is not in the source file.
known_short_seasons <- tribble(
  ~season, ~team,
  2022, "BUF",
  2022, "CIN"
)

# Independent reference list of Super Bowl champions (by season, not game year)
reference_champions <- tribble(
  ~season, ~champion,
  1999, "STL", 2000, "BAL", 2001, "NE",  2002, "TB",  2003, "NE",
  2004, "NE",  2005, "PIT", 2006, "IND", 2007, "NYG", 2008, "PIT",
  2009, "NO",  2010, "GB",  2011, "NYG", 2012, "BAL", 2013, "SEA",
  2014, "NE",  2015, "DEN", 2016, "NE",  2017, "PHI", 2018, "NE",
  2019, "KC",  2020, "TB",  2021, "LA",  2022, "KC",  2023, "KC",
  2024, "PHI", 2025, "SEA"
)

short_seasons <- team_seasons |>
  filter(games != expected_games(season)) |>
  select(season, team)

league_balance <- panel |>
  filter(!is_bye) |>
  group_by(season) |>
  summarise(w = sum(outcome == "W"), l = sum(outcome == "L"), t = sum(outcome == "T"))

checks <- tibble(
  check = c(
    "Seasons 1999 through last_complete_season all present",
    "No regular-season game is missing a score",
    "Exactly one Super Bowl per season, none tied",
    "Champions match independent reference list",
    "Exactly one champion flagged per season in the panel",
    "Every champion appears in the panel and made the playoffs",
    "31 teams in 1999-2001, 32 teams from 2002",
    "No team plays twice in the same week",
    "Games + byes = weeks in season for every team",
    "Every team plays a full schedule (except known 2022 BUF/CIN)",
    "Final W + L + T equals games played",
    "League-wide wins = losses each season; ties are even",
    "Cumulative records never decrease week to week",
    "Spot check: 2007 NE finished 16-0",
    "Spot check: 2008 DET finished 0-16",
    "Spot check: 2017 CLE finished 0-16"
  ),
  passed = c(
    setequal(unique(panel$season), 1999:last_complete_season),
    !any(is.na(games$result[games$game_type == "REG"])),
    all(count(filter(games, game_type == "SB"), season)$n == 1) &&
      !any(games$result[games$game_type == "SB"] == 0),
    isTRUE(all.equal(
      arrange(champions |> select(season, champion), season),
      arrange(reference_champions, season),
      check.attributes = FALSE
    )),
    all(count(filter(team_seasons, champion), season)$n == 1) &&
      n_distinct(filter(team_seasons, champion)$season) == nrow(champions),
    all(paste(champions$season, champions$champion) %in%
          paste(playoff_teams$season, playoff_teams$team)),
    all(with(count(team_seasons, season), n == if_else(season <= 2001, 31L, 32L))),
    nrow(filter(count(filter(panel, !is_bye), season, team, week), n > 1)) == 0,
    all(team_seasons$games + team_seasons$byes == team_seasons$weeks),
    isTRUE(all.equal(arrange(short_seasons, season, team), arrange(known_short_seasons, season, team),
                     check.attributes = FALSE)),
    all(team_seasons$final_w + team_seasons$final_l + team_seasons$final_t == team_seasons$games),
    all(league_balance$w == league_balance$l) && all(league_balance$t %% 2 == 0),
    panel |>
      group_by(season, team) |>
      summarise(ok = all(diff(wins) >= 0 & diff(losses) >= 0 & diff(ties) >= 0), .groups = "drop") |>
      pull(ok) |>
      all(),
    with(filter(team_seasons, season == 2007, team == "NE"), final_w == 16 && final_l == 0),
    with(filter(team_seasons, season == 2008, team == "DET"), final_w == 0 && final_l == 16),
    with(filter(team_seasons, season == 2017, team == "CLE"), final_w == 0 && final_l == 16)
  )
)

print(checks, n = Inf)

if (!all(checks$passed)) {
  stop("Validation failed:\n", paste("-", checks$check[!checks$passed], collapse = "\n"))
}

message("All ", nrow(checks), " validation checks passed.")

write_csv(panel, file.path(project_dir, "data", "team_week_records.csv"))


# =============================================================================
# 6. First look: the "2-0 start" claim
# =============================================================================
# Claim: roughly 60% of Super Bowl winners started 2-0; only a few started 0-2.
# Indexed by games played, not calendar week, so an early bye does not matter.

start_records <- panel |>
  filter(!is_bye, games_played == 2) |>
  mutate(start = record)

# How did champions start?
champion_starts <- start_records |>
  filter(sb_champion) |>
  count(start, name = "champions") |>
  mutate(share_of_champions = champions / sum(champions))

# How did every team start, and how often did each start produce a champion?
all_starts <- start_records |>
  group_by(start) |>
  summarise(
    teams = n(),
    champions = sum(sb_champion),
    playoff_teams = sum(made_playoffs),
    .groups = "drop"
  ) |>
  mutate(
    share_of_all_teams = teams / sum(teams),
    champion_rate = champions / teams,
    playoff_rate = playoff_teams / teams
  )

champion_starts
all_starts

# Relative to the league baseline: how much more likely is a 2-0 team to win?
baseline_rate <- n_distinct(champions$season) / nrow(team_seasons)

all_starts |>
  mutate(lift_vs_baseline = champion_rate / baseline_rate) |>
  select(start, teams, champions, champion_rate, lift_vs_baseline)

# Which champions did NOT start 2-0?
start_records |>
  filter(sb_champion, start != "2-0") |>
  select(season, team, start, final_wins, final_losses, final_ties)
