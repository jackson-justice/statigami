# =============================================================================
# NFL Championship Trajectories
#
# Question: How strongly does an NFL team's record at each point in the
# regular season relate to eventually winning the Super Bowl?
#
# This script:
#   1. Downloads game results: FiveThirtyEight (1966-1998), nflverse (1999 on)
#   2. Builds a team-season-week panel of cumulative W/L/T
#   3. Flags each season's eventual Super Bowl champion
#   4. Runs validation checks on the panel
#   5. Takes a first descriptive look at the "2-0 start" claim
#   6. Tabulates outcomes and loss thresholds by games played, for two
#      baselines: the full Super Bowl era and the modern era (1999 on)
#
# Run from the statigami repo root (where statigami.Rproj lives).
# =============================================================================

library(tidyverse)

project_dir <- "nfl-championship-trajectories"
raw_dir <- file.path(project_dir, "data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# First season of the Super Bowl era, and the first season in nflverse (the
# start of the "modern" baseline).
first_season <- 1966
modern_start <- 1999

# Last season with a completed Super Bowl. Update after each Super Bowl.
last_complete_season <- 2025


# =============================================================================
# 1. Source data
# =============================================================================

# --- 1a. 1999 on: nflverse ---------------------------------------------------
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

modern_games <- games_raw |>
  filter(season >= modern_start, season <= last_complete_season) |>
  select(
    game_id, season, game_type, week, gameday,
    away_team, away_score, home_team, home_score, result
  )

# --- 1b. 1966-1998: FiveThirtyEight ------------------------------------------
# FiveThirtyEight's Elo game file: one row per NFL and AFL game, 1920-2020,
# with a 0/1 playoff flag (MIT license). It is reshaped here into the nflverse
# columns so everything downstream treats the two sources alike.
# https://github.com/fivethirtyeight/nfl-elo-game

historical_url <- paste0(
  "https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/",
  "master/data/nfl_games.csv"
)
historical_raw_path <- file.path(raw_dir, "nfl_games_538.csv")

if (!file.exists(historical_raw_path)) {
  download.file(historical_url, historical_raw_path, mode = "wb")
}

# FiveThirtyEight uses one code per franchise for all time (the 1970 Baltimore
# Colts are "IND"). nflverse uses the code of the era, so convert. Codes not
# listed here are the same in both.
era_codes <- tribble(
  ~code_538, ~from, ~to,   ~team,
  "ARI",     1966,  1987,  "STL",  # St. Louis Cardinals
  "ARI",     1988,  1993,  "PHO",  # Phoenix Cardinals
  "IND",     1966,  1983,  "BAL",  # Baltimore Colts
  "LAC",     1966,  1998,  "SD",   # San Diego Chargers
  "LAR",     1966,  1994,  "LA",   # Los Angeles Rams (nflverse's Rams code)
  "LAR",     1995,  1998,  "STL",  # St. Louis Rams
  "NE",      1966,  1970,  "BOS",  # Boston Patriots
  "OAK",     1982,  1994,  "RAI",  # Los Angeles Raiders ("LA" is the Rams)
  "TEN",     1966,  1996,  "HOU",  # Houston Oilers
  "WSH",     1966,  1998,  "WAS"
)

era_code <- function(code, season) {
  tibble(code_538 = code, season = season) |>
    left_join(era_codes, by = join_by(code_538, between(season, from, to))) |>
    mutate(team = coalesce(team, code_538)) |>
    pull(team)
}

# The file has no week numbers or playoff rounds, so:
#   - week is the calendar week of the season, in Tuesday-to-Monday windows
#     counted from the opening game. This reproduces the official week for
#     1970-1998, including the gaps left by the 1982 and 1987 strikes. For
#     1966-1969 it puts the AFL and NFL, which numbered their weeks
#     separately, on one calendar.
#   - the Super Bowl is the last game of the season; other playoff games are
#     "POST". The playoff flag, not the date, separates the two: in 1967 and
#     1969 the first playoff game came before the last regular-season game.
#   - team1 is the home team (arbitrary at neutral sites).
historical_games <- read_csv(historical_raw_path, show_col_types = FALSE) |>
  filter(season >= first_season, season < modern_start) |>
  group_by(season) |>
  mutate(
    opening_tuesday = min(date) - (wday(min(date), week_start = 2) - 1),
    week = as.integer(date - opening_tuesday) %/% 7 + 1,
    game_type = case_when(
      playoff == 0 ~ "REG",
      date == max(date) ~ "SB",
      TRUE ~ "POST"
    )
  ) |>
  ungroup() |>
  mutate(
    home_team = era_code(team1, season),
    away_team = era_code(team2, season)
  ) |>
  transmute(
    game_id = sprintf("%d_%02d_%s_%s", season, week, away_team, home_team),
    season, game_type, week,
    gameday = date,
    away_team,
    away_score = score2,
    home_team,
    home_score = score1,
    result = home_score - away_score
  )

games <- bind_rows(historical_games, modern_games)


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
#
# Only weeks in which games were played get rows, so the weeks lost to the
# 1982 and 1987 strikes are gaps in the week numbers, not byes.

season_weeks <- team_games |>
  distinct(season, week)

team_weeks <- team_games |>
  distinct(season, team) |>
  left_join(season_weeks, by = "season", relationship = "many-to-many")

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

# League structure by era: scheduled games, teams, and playoff teams.
# The 1982 and 1987 seasons were shortened by player strikes; 1987's 15 games
# include three played by replacement players, which count in the standings.
expected_games <- function(season) {
  case_when(
    season == 1982 ~ 9L,
    season == 1987 ~ 15L,
    season <= 1977 ~ 14L,
    season <= 2020 ~ 16L,
    TRUE ~ 17L
  )
}

# AFL and NFL combined before the 1970 merger
expected_teams <- function(season) {
  case_when(
    season == 1966 ~ 24L,
    season == 1967 ~ 25L,  # Saints
    season <= 1975 ~ 26L,  # Bengals
    season <= 1994 ~ 28L,  # Seahawks, Buccaneers
    season <= 1998 ~ 30L,  # Panthers, Jaguars
    season <= 2001 ~ 31L,  # Browns return
    TRUE ~ 32L             # Texans
  )
}

# Teams that played at least one playoff game. 1968 counts the Chiefs, who
# lost an AFL West tiebreaker playoff; 1982 was a one-off 16-team tournament.
expected_playoff_teams <- function(season) {
  case_when(
    season == 1966 ~ 4L,
    season == 1967 ~ 6L,
    season == 1968 ~ 7L,
    season <= 1977 ~ 8L,
    season == 1982 ~ 16L,
    season <= 1989 ~ 10L,
    season <= 2019 ~ 12L,
    TRUE ~ 14L
  )
}

# Known exception: 2022 BUF at CIN (Week 17) was suspended after Damar Hamlin's
# cardiac arrest and never completed. The league cancelled it, so both teams
# played 16 games. The game is not in the source file.
known_short_seasons <- tribble(
  ~season, ~team,
  2022, "BUF",
  2022, "CIN"
)

# Independent reference list of Super Bowl champions (by season, not game
# year), in era codes: BAL in 1970 is the Colts, RAI the Los Angeles Raiders.
reference_champions <- tribble(
  ~season, ~champion,
  1966, "GB",  1967, "GB",  1968, "NYJ", 1969, "KC",  1970, "BAL",
  1971, "DAL", 1972, "MIA", 1973, "MIA", 1974, "PIT", 1975, "PIT",
  1976, "OAK", 1977, "DAL", 1978, "PIT", 1979, "PIT", 1980, "OAK",
  1981, "SF",  1982, "WAS", 1983, "RAI", 1984, "SF",  1985, "CHI",
  1986, "NYG", 1987, "WAS", 1988, "SF",  1989, "SF",  1990, "NYG",
  1991, "WAS", 1992, "DAL", 1993, "DAL", 1994, "SF",  1995, "DAL",
  1996, "GB",  1997, "DEN", 1998, "DEN",
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

# The panel as first published (1999-2025, nflverse only), kept so the
# extension back to 1966 can be shown not to have changed those seasons.
# Both sides go through CSV so column types match.
reference_dir <- file.path(project_dir, "data", "reference_1999_2025")
reference_panel <- read_csv(
  file.path(reference_dir, "team_week_records.csv"),
  show_col_types = FALSE
)
panel_reference_seasons <- panel |>
  filter(season %in% reference_panel$season) |>
  format_csv() |>
  I() |>
  read_csv(show_col_types = FALSE)

final_record <- function(s, t) {
  with(filter(team_seasons, season == s, team == t),
       paste(final_w, final_l, final_t, sep = "-"))
}

checks <- tibble(
  check = c(
    "Seasons first_season through last_complete_season all present",
    "No game is missing a score",
    "Exactly one Super Bowl per season, none tied",
    "Champions match independent reference list",
    "Exactly one champion flagged per season in the panel",
    "Every champion appears in the panel and made the playoffs",
    "Team count matches the league's size in every season",
    "Playoff-team count matches the format in every season",
    "No team plays twice in the same week",
    "Games + byes = weeks in season for every team",
    "Every team plays a full schedule (except known 2022 BUF/CIN)",
    "Final W + L + T equals games played",
    "League-wide wins = losses each season; ties are even",
    "Cumulative records never decrease week to week",
    "1999-2025 rows are identical to the original nflverse-only panel",
    "Spot check: 1972 MIA finished 14-0",
    "Spot check: 1976 TB finished 0-14",
    "Spot check: 1982 WAS finished 8-1 (strike season)",
    "Spot check: 1985 CHI finished 15-1",
    "Spot check: 1987 SF finished 13-2 (strike season)",
    "Spot check: 2007 NE finished 16-0",
    "Spot check: 2008 DET finished 0-16",
    "Spot check: 2017 CLE finished 0-16"
  ),
  passed = c(
    setequal(unique(panel$season), first_season:last_complete_season),
    !any(is.na(games$result)),
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
    all(with(count(team_seasons, season), n == expected_teams(season))),
    all(with(count(playoff_teams, season), n == expected_playoff_teams(season))),
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
    isTRUE(all.equal(panel_reference_seasons, reference_panel, check.attributes = FALSE)),
    final_record(1972, "MIA") == "14-0-0",
    final_record(1976, "TB") == "0-14-0",
    final_record(1982, "WAS") == "8-1-0",
    final_record(1985, "CHI") == "15-1-0",
    final_record(1987, "SF") == "13-2-0",
    final_record(2007, "NE") == "16-0-0",
    final_record(2008, "DET") == "0-16-0",
    final_record(2017, "CLE") == "0-16-0"
  )
)

print(checks, n = Inf)

if (!all(checks$passed)) {
  stop("Validation failed:\n", paste("-", checks$check[!checks$passed], collapse = "\n"))
}

message("All ", nrow(checks), " validation checks passed.")

write_csv(panel, file.path(project_dir, "data", "team_week_records.csv"))


# =============================================================================
# 6. Two historical baselines
# =============================================================================
# Everything below is calculated twice:
#   - "full":   the whole Super Bowl era (1966 on)
#   - "modern": 1999 on, the original scope of this project
# More history is not automatically a better comparison. Before 1978 seasons
# were 14 games, before 1990 at most 10 teams made the playoffs (16 in the
# 1982 strike season), and before 1974 there was no overtime, so ties were
# far more common. The modern baseline is kept to show what the older seasons
# change.

panel_baselines <- bind_rows(
  full = panel,
  modern = filter(panel, season >= modern_start),
  .id = "baseline"
)

# League-wide title rate: champions per team-season
baseline_rates <- panel_baselines |>
  group_by(baseline) |>
  summarise(
    seasons = n_distinct(season),
    team_seasons = n_distinct(paste(season, team)),
    .groups = "drop"
  ) |>
  mutate(baseline_rate = seasons / team_seasons)

baseline_rates


# =============================================================================
# 7. First look: the "2-0 start" claim
# =============================================================================
# Claim: roughly 60% of Super Bowl winners started 2-0; only a few started 0-2.
# Indexed by games played, not calendar week, so an early bye does not matter.

start_records <- panel_baselines |>
  filter(!is_bye, games_played == 2) |>
  mutate(start = record)

# How did every team start, and how often did each start produce a champion?
all_starts <- start_records |>
  group_by(baseline, start) |>
  summarise(
    teams = n(),
    champions = sum(sb_champion),
    playoff_teams = sum(made_playoffs),
    .groups = "drop_last"
  ) |>
  mutate(
    share_of_all_teams = teams / sum(teams),
    share_of_champions = champions / sum(champions)
  ) |>
  ungroup() |>
  left_join(baseline_rates |> select(baseline, baseline_rate), by = "baseline") |>
  mutate(
    champion_rate = champions / teams,
    playoff_rate = playoff_teams / teams,
    # How much more likely than the average team is this start to win it all?
    lift_vs_baseline = champion_rate / baseline_rate
  ) |>
  select(-baseline_rate) |>
  arrange(baseline, desc(teams))

print(all_starts, n = Inf, width = Inf)

# Which champions did NOT start 2-0?
start_records |>
  filter(baseline == "full", sb_champion, start != "2-0") |>
  select(season, team, start, final_wins, final_losses, final_ties) |>
  print(n = Inf)


# =============================================================================
# 8. Championship outcomes by record after N games
# =============================================================================
# For every games-played count and every record reached at that point: how
# many teams got there, and how many went on to win the Super Bowl or make
# the playoffs.
#
# Notes on the denominators:
#   - Not every season reaches every games-played count. Seasons were 14
#     games through 1977, 16 from 1978 and 17 from 2021, with 9 in 1982 and
#     15 in 1987. So games 15-16 have no pre-1978 teams and game 17 has only
#     2021 on.
#   - The same count is a different point in different seasons: game 14 was
#     the finale through 1977 and has three games left today.

record_outcomes <- panel_baselines |>
  filter(!is_bye) |>
  group_by(baseline, games_played, record, wins, losses, ties) |>
  summarise(
    teams = n(),
    champions = sum(sb_champion),
    playoff_teams = sum(made_playoffs),
    champion_seasons = paste(
      season[sb_champion], team[sb_champion],
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  group_by(baseline, games_played) |>
  mutate(
    share_of_teams = teams / sum(teams),
    share_of_champions = champions / sum(champions)
  ) |>
  ungroup() |>
  left_join(baseline_rates |> select(baseline, baseline_rate), by = "baseline") |>
  mutate(
    champion_rate = champions / teams,
    lift_vs_baseline = champion_rate / baseline_rate,
    playoff_rate = playoff_teams / teams
  ) |>
  select(-baseline_rate) |>
  arrange(baseline, games_played, desc(wins), losses) |>
  relocate(champion_seasons, .after = last_col())

write_csv(
  record_outcomes,
  file.path(project_dir, "data", "record_outcomes_by_games.csv")
)

# Common records (15+ teams) that have never produced a champion, keeping
# only the best such record at each point in the season
record_outcomes |>
  filter(champions == 0, teams >= 15) |>
  group_by(baseline, games_played) |>
  slice_max(wins, n = 1) |>
  ungroup() |>
  select(baseline, games_played, record, teams, playoff_teams, playoff_rate) |>
  print(n = Inf)

# Unbeaten teams: title odds by length of the streak
record_outcomes |>
  filter(losses == 0, ties == 0) |>
  select(baseline, games_played, record, teams, champions, champion_rate,
         lift_vs_baseline) |>
  print(n = Inf)


# =============================================================================
# 9. Loss thresholds by games played
# =============================================================================
# The most losses any eventual champion, and any eventual playoff team, had
# after N games. These split every team into three tiers:
#   - championship path: no more losses than the worst-placed champion
#   - playoff path:      more than that, but no more than the worst-placed
#                        playoff team
#   - graveyard:         more losses than any playoff team
#
# Losses only: a tie is not a loss here. A playoff team is one that played
# at least one playoff game.

loss_thresholds <- panel_baselines |>
  filter(!is_bye) |>
  mutate(label = paste(season, team, record)) |>
  group_by(baseline, games_played) |>
  summarise(
    seasons = n_distinct(season),
    teams = n(),
    champions = sum(sb_champion),
    playoff_teams = sum(made_playoffs),
    champion_max_losses = max(losses[sb_champion]),
    playoff_max_losses = max(losses[made_playoffs]),
    # Teams in each tier, and how many of them went on to the playoffs
    championship_path = sum(losses <= champion_max_losses),
    championship_path_playoffs = sum(
      made_playoffs[losses <= champion_max_losses]
    ),
    playoff_path = sum(losses > champion_max_losses &
                         losses <= playoff_max_losses),
    playoff_path_playoffs = sum(made_playoffs[losses > champion_max_losses]),
    graveyard = sum(losses > playoff_max_losses),
    # The team-seasons that set each threshold
    champion_boundary = paste(
      label[sb_champion & losses == champion_max_losses], collapse = "; "
    ),
    playoff_boundary = paste(
      label[made_playoffs & losses == playoff_max_losses], collapse = "; "
    ),
    .groups = "drop"
  ) |>
  # The thresholds the tiers use (graveyard_tiers.R): the raw maxima, carried
  # forward so they never decrease in games played. Without this a team could
  # drop a tier by winning: the champion maximum falls from 7 losses at game
  # 16 to 6 at game 17 because the 2011 Giants (7-7 after 14) never played a
  # 17th game. That is the only cell the carry-forward changes, on either
  # baseline. The raw columns and the tier counts above keep the raw values.
  group_by(baseline) |>
  arrange(games_played, .by_group = TRUE) |>
  mutate(
    champion_threshold = cummax(champion_max_losses),
    playoff_threshold = cummax(playoff_max_losses)
  ) |>
  ungroup() |>
  relocate(champion_threshold, playoff_threshold, .after = playoff_max_losses)

write_csv(
  loss_thresholds,
  file.path(project_dir, "data", "loss_thresholds_by_games.csv")
)

# Side by side: where does the full era move a threshold?
loss_thresholds |>
  select(baseline, games_played, teams, champion_max_losses,
         playoff_max_losses) |>
  pivot_wider(
    names_from = baseline,
    values_from = c(teams, champion_max_losses, playoff_max_losses)
  ) |>
  mutate(
    champion_change = champion_max_losses_full - champion_max_losses_modern,
    playoff_change = playoff_max_losses_full - playoff_max_losses_modern
  ) |>
  print(n = Inf, width = Inf)

# Share of teams in each tier, and how often each tier paid off
loss_thresholds |>
  transmute(
    baseline, games_played, teams,
    championship_path_share = championship_path / teams,
    playoff_path_share = playoff_path / teams,
    graveyard_share = graveyard / teams,
    title_rate_on_championship_path = champions / championship_path,
    playoff_rate_on_championship_path =
      championship_path_playoffs / championship_path,
    playoff_rate_on_playoff_path = playoff_path_playoffs / playoff_path
  ) |>
  print(n = Inf, width = Inf)
