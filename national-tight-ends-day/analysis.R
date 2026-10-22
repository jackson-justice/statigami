# =============================================================================
# National Tight Ends Day
#
# Question: Do NFL tight ends perform better on National Tight Ends Day
# (fourth Sunday of October, celebrated league-wide since 2019) than they
# normally do?
#
# This script:
#   1. Downloads 2019-2025 weekly player stats and game schedules (nflreadr)
#   2. Validates the schema and the player-stats -> schedule join
#   3. Flags each season's NTED Sunday and classifies every game against it
#   4. Builds a receiver participation panel with season positions, so that
#      zero-target games are separated from games a player did not play
#   5. Aggregates receiving by team-game and position group, with shares of
#      team targets and receiving yards (incl. the full team TE unit)
#   6. First views: NTED vs other Sundays, TEs vs their own baseline, and
#      team TE target-share changes (saved to figures/)
#
# See NTED_PLAN.md for the full analysis plan.
# Run from the statigami repo root (where statigami.Rproj lives).
# =============================================================================

library(tidyverse)
library(nflreadr)

project_dir <- "national-tight-ends-day"
raw_dir <- file.path(project_dir, "data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# Historical window. 2026 is reserved for the prospective prediction.
seasons <- 2019:2025

# Set TRUE to re-download instead of using the cached raw files.
refresh_raw <- FALSE


# =============================================================================
# 1. Source data
# =============================================================================
# nflverse weekly player stats (one row per player-game) and schedules (one
# row per game). Raw downloads are cached in data/raw/, which is git-ignored.
# https://nflreadr.nflverse.com/reference/load_player_stats.html
# https://nflreadr.nflverse.com/reference/load_schedules.html

load_cached <- function(path, loader) {
  if (refresh_raw || !file.exists(path)) {
    data <- loader()
    write_rds(data, path, compress = "gz")
  }
  read_rds(path)
}

player_stats_raw <- load_cached(
  file.path(raw_dir, "player_stats_week_2019_2025.rds"),
  \() load_player_stats(seasons, summary_level = "week")
)

schedules_raw <- load_cached(
  file.path(raw_dir, "schedules_2019_2025.rds"),
  \() load_schedules(seasons)
)

# Participation and position sources (used in section 4). Weekly stats only
# have a row when a player records a stat, so snap counts are needed to tell
# "played, zero targets" from "did not play".
# https://nflreadr.nflverse.com/reference/load_snap_counts.html
# https://nflreadr.nflverse.com/reference/load_rosters.html
snap_counts_raw <- load_cached(
  file.path(raw_dir, "snap_counts_2019_2025.rds"),
  \() load_snap_counts(seasons)
)

rosters_raw <- load_cached(
  file.path(raw_dir, "rosters_2019_2025.rds"),
  \() load_rosters(seasons)
)

# Player ID crosswalk: snap counts use PFR IDs, player stats use GSIS IDs
players_raw <- load_cached(
  file.path(raw_dir, "players.rds"),
  load_players
)


# =============================================================================
# 2. Schema and join validation
# =============================================================================
# nflverse has renamed player-stats columns before (e.g. recent_team -> team),
# so fail loudly if anything the analysis depends on is missing.

required_player_cols <- c(
  "player_id", "player_display_name", "position", "position_group",
  "season", "week", "season_type", "game_id", "team", "opponent_team",
  "targets", "receptions", "receiving_yards", "receiving_tds"
)
required_schedule_cols <- c(
  "game_id", "season", "game_type", "week", "gameday", "weekday",
  "away_team", "home_team", "away_score", "home_score"
)

missing_player_cols <- setdiff(required_player_cols, names(player_stats_raw))
missing_schedule_cols <- setdiff(required_schedule_cols, names(schedules_raw))
stopifnot(
  "player stats missing columns" = length(missing_player_cols) == 0,
  "schedules missing columns" = length(missing_schedule_cols) == 0
)

# Every season present, and one row per player-game
stopifnot(
  setequal(unique(player_stats_raw$season), seasons),
  setequal(unique(schedules_raw$season), seasons),
  !anyDuplicated(player_stats_raw[c("player_id", "game_id")]),
  !anyDuplicated(schedules_raw$game_id)
)

# Games per season. Expect 256 REG games through 2020 and 272 from 2021 on
# (17-game season), except 2022 = 271 (BUF-CIN was cancelled, never played).
schedules_raw |>
  count(season, game_type) |>
  pivot_wider(names_from = game_type, values_from = n) |>
  print()

# Schedules keep historical abbreviations (2019 Raiders = OAK) while player
# stats use current ones (LV). Normalize team columns, but leave game_id as-is
# since it is the join key.
schedules <- schedules_raw |>
  mutate(across(c(away_team, home_team), \(x) clean_team_abbrs(x, current_location = TRUE)))

# Join player-games to schedules on game_id. Every player row should find its
# game, and each player's team should be one of the two teams in that game.
player_games <- player_stats_raw |>
  left_join(
    schedules |>
      select(game_id, sched_season = season, sched_week = week, game_type,
             gameday, weekday, away_team, home_team),
    by = "game_id"
  )

join_check <- player_games |>
  summarise(
    rows = n(),
    no_schedule_match = sum(is.na(gameday)),
    season_mismatch = sum(season != sched_season, na.rm = TRUE),
    week_mismatch = sum(week != sched_week, na.rm = TRUE),
    team_not_in_game = sum(team != away_team & team != home_team, na.rm = TRUE),
    opponent_mismatch = sum(
      opponent_team != if_else(team == home_team, away_team, home_team),
      na.rm = TRUE
    )
  )
print(join_check)

stopifnot(
  join_check$no_schedule_match == 0,
  join_check$season_mismatch == 0,
  join_check$week_mismatch == 0,
  join_check$team_not_in_game == 0,
  join_check$opponent_mismatch == 0
)

# Every completed regular-season game should have player stats for both teams
completed_reg_games <- schedules |>
  filter(game_type == "REG", !is.na(home_score))

games_without_stats <- completed_reg_games |>
  anti_join(distinct(player_stats_raw, game_id), by = "game_id")
stopifnot(nrow(games_without_stats) == 0)

# Quick look at what positions are available for the TE/WR/RB comparison
player_stats_raw |>
  filter(season_type == "REG", targets > 0) |>
  count(position_group, sort = TRUE) |>
  print()

# Raw-data vintage, for the article's data attribution
message(
  "Player stats: ", nrow(player_stats_raw), " rows, nflverse timestamp ",
  attr(player_stats_raw, "nflverse_timestamp")
)
message(
  "Schedules: ", nrow(schedules_raw), " games, nflverse timestamp ",
  attr(schedules_raw, "nflverse_timestamp")
)


# =============================================================================
# 3. Flag National Tight Ends Day
# =============================================================================
# NTED is the fourth Sunday of October. Flag games by actual calendar date,
# not week number: only games played on that Sunday count as NTED games.
# Thursday/Monday games in the same NFL week are kept as a separate category
# for an optional, clearly labeled full-week sensitivity analysis.

fourth_sunday_of_october <- function(year) {
  oct_days <- as.Date(paste0(year, "-10-01")) + 0:30
  oct_days[wday(oct_days, week_start = 1) == 7][4]
}

nted_dates <- tibble(
  season = seasons,
  nted_date = map_vec(seasons, fourth_sunday_of_october)
)

# Fourth Sunday always falls on Oct 22-28
stopifnot(
  all(wday(nted_dates$nted_date, week_start = 1) == 7),
  all(day(nted_dates$nted_date) %in% 22:28)
)

# Schedule weekday should agree with the calendar date
schedules <- schedules |>
  mutate(gameday = as.Date(gameday))
stopifnot(all(schedules$weekday == as.character(wday(schedules$gameday, label = TRUE, abbr = FALSE))))

# The NFL week that contains each NTED Sunday
nted_weeks <- schedules |>
  inner_join(nted_dates, by = "season") |>
  filter(gameday == nted_date) |>
  distinct(season, nted_date, nted_week = week)

# Every NTED Sunday has regular-season games, all in a single NFL week
stopifnot(
  nrow(nted_weeks) == length(seasons),
  setequal(nted_weeks$season, seasons)
)

# Classify every regular-season game relative to NTED
reg_games <- schedules |>
  filter(game_type == "REG") |>
  left_join(nted_weeks, by = "season") |>
  mutate(
    nted_category = case_when(
      gameday == nted_date ~ "NTED Sunday",
      week == nted_week ~ "NTED week, other day",
      weekday == "Sunday" ~ "Other Sunday",
      .default = "Other day"
    ),
    is_nted = nted_category == "NTED Sunday"
  )

# NTED counts by season: games, teams playing, teams on bye, kickoff slots
nted_summary <- reg_games |>
  filter(week == nted_week) |>
  group_by(season, nted_date, nted_week) |>
  summarise(
    nted_sunday_games = sum(is_nted),
    nted_week_other_day_games = sum(!is_nted),
    other_days = paste(sort(unique(weekday[!is_nted])), collapse = ", "),
    teams_on_nted = n_distinct(c(home_team[is_nted], away_team[is_nted])),
    .groups = "drop"
  ) |>
  mutate(teams_on_bye = 32 - teams_on_nted - 2 * nted_week_other_day_games)
print(nted_summary)

stopifnot(
  all(nted_summary$teams_on_nted == 2 * nted_summary$nted_sunday_games),
  all(nted_summary$teams_on_bye >= 0)
)

# Comparison pool: Sunday games across the regular season
reg_games |>
  count(season, nted_category) |>
  pivot_wider(names_from = nted_category, values_from = n) |>
  print()

# Attach the NTED flag to player-games. Every regular-season player row must
# get a category; postseason rows are out of scope and dropped here.
player_games <- player_stats_raw |>
  filter(season_type == "REG") |>
  inner_join(
    reg_games |>
      select(game_id, gameday, weekday, nted_date, nted_week, nted_category, is_nted),
    by = "game_id"
  )

stopifnot(
  nrow(player_games) == sum(player_stats_raw$season_type == "REG"),
  !anyNA(player_games$nted_category)
)

# NTED player rows by season and position group (receivers only)
player_games |>
  filter(is_nted, position_group %in% c("TE", "WR", "RB")) |>
  count(season, position_group) |>
  pivot_wider(names_from = position_group, values_from = n) |>
  print()

# Small, auditable list of NTED games for the article and later steps
nted_games <- reg_games |>
  filter(is_nted) |>
  select(season, week, game_id, gameday, gametime, away_team, home_team,
         away_score, home_score, location) |>
  arrange(season, gametime, game_id)
write_csv(nted_games, file.path(project_dir, "data", "nted_games.csv"))


# =============================================================================
# 4. Participation and position definitions
# =============================================================================

# 4a. Season positions --------------------------------------------------------
# The weekly stats `position` is one static (current) label per player across
# every season, e.g. N'Keal Harry is "TE" in 2019-2022 when he was a WR.
stopifnot(
  player_stats_raw |>
    distinct(player_id, position) |>
    count(player_id) |>
    pull(n) |>
    max() == 1
)

# Use the team's season roster position instead (already grouped: FBs are
# listed as RB). One player-season appears twice; keep the latest listing.
season_positions <- rosters_raw |>
  filter(!is.na(gsis_id)) |>
  arrange(season, gsis_id, desc(week)) |>
  distinct(season, gsis_id, .keep_all = TRUE) |>
  select(season, player_id = gsis_id, roster_position = position)

# Deliberate exceptions to the roster rule. Taysom Hill is rostered as a QB
# but used as a TE/move player, so he counts in the Saints' TE group.
position_overrides <- tibble(
  player_id = "00-0033357",
  override_position = "TE"
)

to_position_group <- function(position) {
  case_when(
    position == "TE" ~ "TE",
    position == "WR" ~ "WR",
    position %in% c("RB", "FB", "HB") ~ "RB",
    position == "QB" ~ "QB",
    is.na(position) ~ NA_character_,
    .default = "Other"
  )
}

# 4b. Snap counts with GSIS IDs -----------------------------------------------
# Crosswalk PFR -> GSIS. The players table is authoritative; season rosters
# only fill PFR IDs it lacks. Rosters carry a few bad IDs (e.g. Tyler Conklin
# listed with Ryan Izzo's PFR ID; two different Byron Youngs and Jonah
# Williamses merged), so roster rows never override or duplicate a mapping.
players_ids <- players_raw |>
  filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct(pfr_id, gsis_id)

roster_only_ids <- rosters_raw |>
  filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct(pfr_id, gsis_id) |>
  anti_join(players_ids, by = "pfr_id") |>
  anti_join(players_ids, by = "gsis_id") |>
  group_by(pfr_id) |>
  filter(n() == 1) |>
  ungroup()

pfr_to_gsis <- bind_rows(players_ids, roster_only_ids)
stopifnot(
  !anyDuplicated(pfr_to_gsis$pfr_id),
  !anyDuplicated(pfr_to_gsis$gsis_id)
)

snaps <- snap_counts_raw |>
  filter(game_type == "REG") |>
  mutate(team = clean_team_abbrs(team, current_location = TRUE)) |>
  left_join(pfr_to_gsis, by = c("pfr_player_id" = "pfr_id")) |>
  select(game_id, team, gsis_id, pfr_player_id, snap_player = player,
         snap_position = position, offense_snaps, offense_pct, st_snaps)

# Snap data covers every regular-season game played
stopifnot(nrow(anti_join(filter(reg_games, !is.na(home_score)), snaps, by = "game_id")) == 0)

unmatched_snaps <- snaps |> filter(is.na(gsis_id), offense_snaps > 0)
message(
  "Snap rows with offensive snaps but no GSIS ID: ", nrow(unmatched_snaps),
  " (", sum(unmatched_snaps$snap_position == "TE"), " TE)"
)

# 4c. Receiver participation panel --------------------------------------------
# One row per player-team-game, from the union of stat rows and snap rows.
#   - Played with no stat row  -> receiving stats are a true zero
#   - On roster, no offensive snaps and no offensive stats -> did not play
#     (not imputed as zero; excluded from per-player baselines)
# Stat rows with no player_id are team-level entries (no targets); drop them.
stopifnot(sum(player_games$targets[is.na(player_games$player_id)]) == 0)

receiving_stats <- player_games |>
  filter(!is.na(player_id)) |>
  select(game_id, team, player_id, player_display_name, stats_position = position,
         targets, receptions, receiving_yards, receiving_tds,
         carries, attempts) |>
  mutate(has_stat_row = TRUE)

participation <- receiving_stats |>
  full_join(
    snaps |> filter(!is.na(gsis_id)) |> rename(player_id = gsis_id),
    by = c("game_id", "team", "player_id")
  ) |>
  # Players with snaps but no GSIS ID keep their PFR ID so they still count
  bind_rows(unmatched_snaps |> select(-gsis_id) |> mutate(player_id = pfr_player_id)) |>
  inner_join(
    reg_games |> select(game_id, season, week, gameday, nted_category, is_nted),
    by = "game_id"
  ) |>
  left_join(season_positions, by = c("season", "player_id")) |>
  left_join(position_overrides, by = "player_id") |>
  mutate(
    has_stat_row = replace_na(has_stat_row, FALSE),
    has_snap_row = !is.na(offense_snaps),
    player_name = coalesce(player_display_name, snap_player),
    # Season roster position, then PFR game position, then static stats label
    position = coalesce(roster_position, snap_position, stats_position),
    # Two-way players rostered on defense (e.g. Travis Hunter, DB) keep their
    # receiving label from the stats file
    position = if_else(
      to_position_group(position) == "Other" &
        to_position_group(stats_position) %in% c("TE", "WR", "RB"),
      stats_position, position
    ),
    position = coalesce(override_position, position),
    position_group = to_position_group(position),
    across(c(targets, receptions, receiving_yards, receiving_tds, carries,
             attempts, offense_snaps), \(x) replace_na(x, 0)),
    played_offense = offense_snaps > 0 | targets > 0 | carries > 0 | attempts > 0
  ) |>
  select(season, week, game_id, gameday, nted_category, is_nted, team,
         player_id, player_name, position, position_group, roster_position,
         snap_position, stats_position, has_stat_row, has_snap_row,
         played_offense, offense_snaps, offense_pct,
         targets, receptions, receiving_yards, receiving_tds)

stopifnot(
  !anyDuplicated(participation[c("game_id", "team", "player_id")]),
  !anyNA(participation$position_group)
)

# 4d. Participation checks ----------------------------------------------------

# Answers plan question 1: how many TEs played offense with no stat row?
te_games <- participation |> filter(position_group == "TE", played_offense)

te_games |>
  group_by(season) |>
  summarise(
    te_player_games = n(),
    no_stat_row = sum(!has_stat_row),
    pct_no_stat_row = round(100 * mean(!has_stat_row), 1),
    zero_targets = sum(targets == 0),
    pct_zero_targets = round(100 * mean(targets == 0), 1),
    stats_but_no_snaps = sum(!has_snap_row)
  ) |>
  print()

# Team-games where no TE played an offensive snap
team_games <- reg_games |>
  filter(!is.na(home_score)) |>
  select(game_id, season, nted_category, home_team, away_team) |>
  pivot_longer(c(home_team, away_team), values_to = "team") |>
  select(-name)

team_te_usage <- team_games |>
  left_join(
    te_games |> count(game_id, team, name = "tes_played"),
    by = c("game_id", "team")
  ) |>
  mutate(tes_played = replace_na(tes_played, 0L))

team_te_usage |> count(tes_played) |> print()

# Position-label disagreements among receivers, by player-season. Season roster
# position wins; this is what changes relative to the static stats label.
position_changes <- participation |>
  filter(played_offense, !is.na(stats_position)) |>
  mutate(stats_group = to_position_group(stats_position)) |>
  filter(position_group != stats_group,
         position_group %in% c("TE", "WR", "RB") | stats_group %in% c("TE", "WR", "RB")) |>
  group_by(season, player_name, team, stats_group, position_group) |>
  summarise(games = n(), targets = sum(targets), .groups = "drop") |>
  arrange(desc(targets))
print(position_changes, n = 25)

# Who catches passes outside TE/WR/RB? These stay in team target totals
# (the share denominator) as "QB" or "Other".
participation |>
  filter(targets > 0) |>
  group_by(position_group) |>
  summarise(player_games = n(), targets = sum(targets)) |>
  mutate(pct_targets = round(100 * targets / sum(targets), 2)) |>
  print()

# TE participation on NTED itself
te_games |>
  filter(is_nted) |>
  group_by(season) |>
  summarise(
    teams = n_distinct(team),
    te_player_games = n(),
    zero_target_games = sum(targets == 0),
    te_targets = sum(targets)
  ) |>
  print()


# =============================================================================
# 5. Team-game position-group aggregates
# =============================================================================
# One row per team-game-position group (TE, WR, RB, QB, Other), zero-filled so
# every team-game has every group. Shares use the whole team as denominator,
# so QB/Other targets count toward team volume but are not a headline group.
# Bye teams have no team-game row at all: they are never counted as zeros.

position_groups <- c("TE", "WR", "RB", "QB", "Other")

team_game_groups <- participation |>
  group_by(season, week, game_id, gameday, nted_category, is_nted, team,
           position_group) |>
  summarise(
    players = sum(played_offense),
    targets = sum(targets),
    receptions = sum(receptions),
    receiving_yards = sum(receiving_yards),
    receiving_tds = sum(receiving_tds),
    .groups = "drop"
  ) |>
  complete(
    nesting(season, week, game_id, gameday, nted_category, is_nted, team),
    position_group = position_groups,
    fill = list(players = 0L, targets = 0L, receptions = 0L,
                receiving_yards = 0, receiving_tds = 0L)
  ) |>
  group_by(game_id, team) |>
  mutate(
    team_targets = sum(targets),
    team_receiving_yards = sum(receiving_yards),
    target_share = targets / team_targets,
    receiving_yard_share = receiving_yards / team_receiving_yards
  ) |>
  ungroup() |>
  mutate(position_group = factor(position_group, levels = position_groups))

# Team TE unit: every TE who played, including backups (research question 2)
team_te_units <- team_game_groups |>
  filter(position_group == "TE") |>
  select(-position_group) |>
  rename(tes_played = players)

# 5a. Checks ------------------------------------------------------------------

# One row per completed team-game per group, and nothing else
stopifnot(
  n_distinct(team_game_groups[c("game_id", "team")]) == nrow(team_games),
  nrow(team_game_groups) == nrow(team_games) * length(position_groups),
  nrow(anti_join(team_games, team_game_groups, by = c("game_id", "team"))) == 0
)

# Totals match the raw weekly stats exactly (no rows lost or double counted)
raw_team_totals <- player_games |>
  group_by(game_id, team) |>
  summarise(raw_targets = sum(targets), raw_rec_yards = sum(receiving_yards),
            raw_pass_att = sum(attempts), raw_pass_yards = sum(passing_yards),
            .groups = "drop")

team_totals_check <- team_game_groups |>
  distinct(game_id, team, team_targets, team_receiving_yards) |>
  left_join(raw_team_totals, by = c("game_id", "team"))

stopifnot(
  all(team_totals_check$team_targets == team_totals_check$raw_targets),
  all(team_totals_check$team_receiving_yards == team_totals_check$raw_rec_yards),
  # Targets can't exceed pass attempts (throwaways/spikes have no target)
  all(team_totals_check$team_targets <= team_totals_check$raw_pass_att),
  # Every team-game has pass targets, so shares are always defined
  all(team_totals_check$team_targets > 0)
)

# Receiving yards should equal gross passing yards; report any differences
# (laterals and stat corrections can cause small gaps)
team_totals_check |>
  mutate(diff = team_receiving_yards - raw_pass_yards) |>
  count(diff_abs = pmin(abs(diff), 10), name = "team_games") |>
  print()

# Group shares add up to 1 within every team-game
share_sums <- team_game_groups |>
  group_by(game_id, team) |>
  summarise(target_share = sum(target_share),
            yard_share = sum(receiving_yard_share), .groups = "drop")
stopifnot(all(abs(share_sums$target_share - 1) < 1e-9))
# Yard share can be undefined (0/0) or odd when team receiving yards <= 0
message("Team-games with team receiving yards <= 0: ",
        sum(team_totals_check$team_receiving_yards <= 0))

# 5b. Overview ----------------------------------------------------------------

# League-wide TE/WR/RB share of targets and receiving yards by season (pooled
# over all regular-season team-games)
team_game_groups |>
  filter(position_group %in% c("TE", "WR", "RB")) |>
  group_by(season, position_group) |>
  summarise(target_share = sum(targets) / sum(team_targets),
            yard_share = sum(receiving_yards) / sum(team_receiving_yards),
            .groups = "drop") |>
  pivot_wider(names_from = position_group,
              values_from = c(target_share, yard_share)) |>
  mutate(across(-season, \(x) round(100 * x, 1))) |>
  print()

# Distribution of team TE target share per game
summary(team_te_units$target_share) |> print()


# =============================================================================
# 6. First views
# =============================================================================
# Exploratory comparisons of NTED Sunday with other Sundays in the same season.
# Within-season comparisons matter: TE target share rose ~3 points 2019-2025.

figures_dir <- file.path(project_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

# Chart ink. No Statigami brand files exist yet, so this uses a neutral
# palette: one blue highlight for NTED, gray for everything else.
nted_blue <- "#2a78d6"
baseline_gray <- "#898781"
ink <- "#0b0b0b"
ink_secondary <- "#52514e"
gridline <- "#e1e0d9"
surface <- "#fcfcfb"

theme_nted <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = surface, color = NA),
      panel.grid.major = element_line(color = gridline, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(color = ink, face = "bold"),
      plot.subtitle = element_text(color = ink_secondary),
      plot.caption = element_text(color = ink_secondary, hjust = 0),
      axis.text = element_text(color = ink_secondary),
      axis.title = element_text(color = ink_secondary),
      strip.text = element_text(color = ink, face = "bold", hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(color = ink_secondary)
    )
}

data_caption <- "Data: nflverse via nflreadr (weekly player stats, schedules, snap counts, rosters). Regular season, 2019-2025."

# Cluster bootstrap for a mean: resample whole games, so players or teams
# from the same game are not treated as independent.
set.seed(2026)
boot_mean_ci <- function(values, clusters, reps = 2000) {
  by_cluster <- split(values, clusters)
  boot_means <- replicate(reps, {
    mean(unlist(by_cluster[sample(names(by_cluster), replace = TRUE)]))
  })
  tibble(
    mean = mean(values),
    ci_low = unname(quantile(boot_means, 0.025)),
    ci_high = unname(quantile(boot_means, 0.975))
  )
}

# 6a. League: NTED vs every other Sunday ---------------------------------------
# Pooled share on each Sunday date (group targets / team targets). Dates with
# few games (e.g. a lone rescheduled game) are too noisy to compare.
min_team_games_per_date <- 10

sunday_dates <- team_game_groups |>
  filter(nted_category %in% c("NTED Sunday", "Other Sunday"),
         position_group %in% c("TE", "WR", "RB")) |>
  group_by(season, gameday, is_nted, position_group) |>
  summarise(
    team_games = n(),
    target_share = sum(targets) / sum(team_targets),
    yard_share = sum(receiving_yards) / sum(team_receiving_yards),
    .groups = "drop"
  )

message("Sunday dates dropped (< ", min_team_games_per_date, " team-games): ",
        sunday_dates |> filter(team_games < min_team_games_per_date) |>
          distinct(season, gameday) |> nrow())
sunday_dates <- filter(sunday_dates, team_games >= min_team_games_per_date)
stopifnot(sum(sunday_dates$is_nted) == length(seasons) * 3)

# Where NTED ranks among that season's Sundays (1 = highest share)
nted_rank <- sunday_dates |>
  group_by(season, position_group) |>
  mutate(
    rank = min_rank(desc(target_share)),
    sundays = n(),
    other_sunday_median = median(target_share[!is_nted])
  ) |>
  ungroup() |>
  filter(is_nted) |>
  transmute(
    season, position_group,
    nted_share = round(100 * target_share, 1),
    other_sunday_median = round(100 * other_sunday_median, 1),
    diff_pp = nted_share - other_sunday_median,
    rank = paste(rank, "of", sundays)
  )
print(nted_rank, n = Inf)

# Average NTED difference across the seven seasons, by group
nted_rank |>
  group_by(position_group) |>
  summarise(mean_diff_pp = round(mean(diff_pp), 2),
            seasons_above_median = sum(diff_pp > 0)) |>
  print()

fig_league <- sunday_dates |>
  mutate(
    position_group = factor(position_group, levels = c("TE", "WR", "RB"),
                            labels = c("Tight ends", "Wide receivers", "Running backs")),
    sunday_type = if_else(is_nted, "National Tight Ends Day", "Other Sundays")
  ) |>
  # Ringed points (surface-colored outline) so overlapping dots stay distinct;
  # NTED is drawn last and larger so it sits on top
  ggplot(aes(factor(season), target_share, fill = sunday_type)) +
  geom_point(
    data = \(d) filter(d, !is_nted),
    position = position_jitter(width = 0.15, height = 0, seed = 1),
    shape = 21, color = surface, size = 2, stroke = 0.4
  ) +
  geom_point(data = \(d) filter(d, is_nted), shape = 21,
             color = surface, size = 3.8, stroke = 0.8) +
  facet_wrap(~position_group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("National Tight Ends Day" = nted_blue,
                               "Other Sundays" = baseline_gray)) +
  guides(fill = guide_legend(override.aes = list(size = 3.5))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title = "Share of team targets: NTED vs every other Sunday",
    subtitle = "League-wide share on each Sunday of the regular season",
    x = NULL, y = "Share of team targets",
    caption = data_caption
  ) +
  theme_nted()

# 6b. Individual TEs vs their own baseline -------------------------------------
# Baseline = the TE's average on other Sundays for the same team and season,
# counting only games he played offensive snaps (DNPs are not zeros).
min_baseline_games <- 3

te_sundays <- participation |>
  filter(position_group == "TE", played_offense,
         nted_category %in% c("NTED Sunday", "Other Sunday"))

te_baselines <- te_sundays |>
  filter(!is_nted) |>
  group_by(season, team, player_id) |>
  summarise(
    baseline_games = n(),
    across(c(targets, receptions, receiving_yards, receiving_tds),
           mean, .names = "base_{.col}"),
    .groups = "drop"
  )

te_vs_baseline <- te_sundays |>
  filter(is_nted) |>
  inner_join(te_baselines, by = c("season", "team", "player_id")) |>
  filter(baseline_games >= min_baseline_games) |>
  mutate(
    targets_diff = targets - base_targets,
    receptions_diff = receptions - base_receptions,
    yards_diff = receiving_yards - base_receiving_yards,
    tds_diff = receiving_tds - base_receiving_tds,
    # Percent change only where the baseline is large enough to be stable
    yards_pct_diff = if_else(base_receiving_yards >= 30,
                             yards_diff / base_receiving_yards, NA_real_)
  )

message("NTED TE games: ", sum(te_sundays$is_nted), "; with >= ",
        min_baseline_games, " baseline Sundays: ", nrow(te_vs_baseline))

summarise_te_diffs <- function(d, label) {
  bind_rows(
    boot_mean_ci(d$targets_diff, d$game_id) |> mutate(metric = "targets"),
    boot_mean_ci(d$receptions_diff, d$game_id) |> mutate(metric = "receptions"),
    boot_mean_ci(d$yards_diff, d$game_id) |> mutate(metric = "yards"),
    boot_mean_ci(d$tds_diff, d$game_id) |> mutate(metric = "tds"),
    boot_mean_ci(na.omit(d$yards_pct_diff),
                 d$game_id[!is.na(d$yards_pct_diff)]) |> mutate(metric = "yards % (base 30+)")
  ) |>
    mutate(tes = label, n = nrow(d))
}

te_diff_summary <- bind_rows(
  summarise_te_diffs(te_vs_baseline, "All TEs who played"),
  # Regular receiving TEs: baseline of 3+ targets per Sunday
  summarise_te_diffs(filter(te_vs_baseline, base_targets >= 3), "Baseline 3+ targets")
)
te_diff_summary |>
  mutate(across(c(mean, ci_low, ci_high), \(x) round(x, 2))) |>
  print()

# Biggest yardage games above baseline
te_vs_baseline |>
  arrange(desc(yards_diff)) |>
  transmute(season, team, player_name, targets, receiving_yards,
            base_targets = round(base_targets, 1),
            base_yards = round(base_receiving_yards, 1),
            yards_diff = round(yards_diff, 1)) |>
  head(10) |>
  print()

fig_te <- te_vs_baseline |>
  select(game_id, targets_diff, yards_diff) |>
  pivot_longer(-game_id, names_to = "metric", values_to = "diff") |>
  mutate(metric = factor(metric, levels = c("targets_diff", "yards_diff"),
                         labels = c("Targets vs own baseline", "Receiving yards vs own baseline"))) |>
  ggplot(aes(diff)) +
  geom_histogram(
    data = \(d) filter(d, metric == "Targets vs own baseline"),
    binwidth = 1, center = 0, fill = nted_blue, color = surface, linewidth = 0.5
  ) +
  geom_histogram(
    data = \(d) filter(d, metric == "Receiving yards vs own baseline"),
    binwidth = 10, center = 0, fill = nted_blue, color = surface, linewidth = 0.5
  ) +
  geom_vline(xintercept = 0, color = ink, linewidth = 0.4) +
  facet_wrap(~metric, ncol = 1, scales = "free") +
  labs(
    title = "Tight ends' NTED games vs their own season baseline",
    subtitle = paste0(
      "NTED value minus the same TE's average on other Sundays that season\n",
      nrow(te_vs_baseline), " TE games; each TE played ", min_baseline_games,
      "+ other Sundays for the same team"
    ),
    x = "Difference from own baseline", y = "TE games",
    caption = data_caption
  ) +
  theme_nted()

# 6c. Team TE target share vs the team's own baseline --------------------------
team_te_baselines <- team_te_units |>
  filter(nted_category == "Other Sunday") |>
  group_by(season, team) |>
  summarise(
    baseline_games = n(),
    base_te_target_share = sum(targets) / sum(team_targets),
    base_te_yard_share = sum(receiving_yards) / sum(team_receiving_yards),
    .groups = "drop"
  )

team_te_vs_baseline <- team_te_units |>
  filter(is_nted) |>
  inner_join(team_te_baselines, by = c("season", "team")) |>
  mutate(
    target_share_diff_pp = 100 * (target_share - base_te_target_share),
    yard_share_diff_pp = 100 * (receiving_yard_share - base_te_yard_share)
  )
stopifnot(nrow(team_te_vs_baseline) == sum(nted_summary$teams_on_nted))

team_share_summary <- bind_rows(
  boot_mean_ci(team_te_vs_baseline$target_share_diff_pp, team_te_vs_baseline$game_id) |>
    mutate(metric = "TE target share (pp)",
           share_above_baseline = mean(team_te_vs_baseline$target_share_diff_pp > 0)),
  boot_mean_ci(team_te_vs_baseline$yard_share_diff_pp, team_te_vs_baseline$game_id) |>
    mutate(metric = "TE receiving-yard share (pp)",
           share_above_baseline = mean(team_te_vs_baseline$yard_share_diff_pp > 0))
) |>
  mutate(team_games = nrow(team_te_vs_baseline))
team_share_summary |>
  mutate(across(where(is.double), \(x) round(x, 2))) |>
  print()

team_season_means <- team_te_vs_baseline |>
  group_by(season) |>
  summarise(mean_diff_pp = mean(target_share_diff_pp), .groups = "drop")
print(team_season_means)

fig_team <- team_te_vs_baseline |>
  ggplot(aes(factor(season), target_share_diff_pp)) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.4) +
  geom_point(
    position = position_jitter(width = 0.15, height = 0, seed = 1),
    color = baseline_gray, alpha = 0.7, size = 1.8
  ) +
  geom_point(data = team_season_means, aes(y = mean_diff_pp),
             shape = 21, fill = nted_blue, color = surface, size = 4, stroke = 0.8) +
  scale_y_continuous(labels = \(x) paste0(if_else(x > 0, "+", ""), x, " pts")) +
  labs(
    title = "Team TE target share on NTED vs each team's other Sundays",
    subtitle = "Each gray dot is one team's NTED game; blue is the season average",
    x = NULL, y = "Change in TE share of team targets",
    caption = data_caption
  ) +
  theme_nted()

ggsave(file.path(figures_dir, "01_nted_vs_other_sundays.png"), fig_league,
       width = 8, height = 9, dpi = 200, bg = surface)
ggsave(file.path(figures_dir, "02_te_vs_own_baseline.png"), fig_te,
       width = 8, height = 7, dpi = 200, bg = surface)
ggsave(file.path(figures_dir, "03_team_te_share_change.png"), fig_team,
       width = 8, height = 5.5, dpi = 200, bg = surface)
