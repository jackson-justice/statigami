# =============================================================================
# NFL Championship Trajectories: weekly update
#
# Run every Tuesday after Monday Night Football. For every team in the current
# season, this script compares its record with every team in the historical
# baseline that had the same record after the same number of games, then
# prints a ready-to-post summary.
#
# Requires the outputs of analysis.R (run that first, once per offseason):
#   data/team_week_records.csv
#   data/record_outcomes_by_games.csv
#   data/loss_thresholds_by_games.csv   (used by graveyard_tiers.R)
#
# Run from the statigami repo root (where statigami.Rproj lives):
#   source("nfl-championship-trajectories/weekly_update.R")
# =============================================================================

library(tidyverse)

project_dir <- "nfl-championship-trajectories"
data_dir <- file.path(project_dir, "data")
raw_dir <- file.path(data_dir, "raw")
weekly_dir <- file.path(data_dir, "weekly")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(weekly_dir, recursive = TRUE, showWarnings = FALSE)

# Which history to compare against (see analysis.R section 6):
#   "modern" = 1999 on, "full" = the whole Super Bowl era (1966 on)
# This picks the baseline for the odds and the floor watch. The Graveyard
# tiers always use both (full as primary, modern alongside).
history_baseline <- "modern"


# =============================================================================
# 1. Current-season results and the Graveyard tiers
# =============================================================================
# graveyard_tiers.R downloads today's scores, builds the season-to-date
# team-week panel, classifies every team-week into the three tiers on both
# baselines, validates, and saves data/graveyard/<season>_*.csv. It leaves
# these objects for the rest of this script:
#   games_current, current_season, season_games, completed_week,
#   completed_games, team_results (one row per team per game played),
#   tiers_wide (team x week, both tiers), movements, section()

source(file.path(project_dir, "graveyard_tiers.R"))


# =============================================================================
# 2. Current records, and last week's records for comparison
# =============================================================================

record_through <- function(results, through_week) {
  results |>
    filter(week <= through_week) |>
    group_by(team) |>
    summarise(
      games_played = n(),
      wins = sum(outcome == "W"),
      losses = sum(outcome == "L"),
      ties = sum(outcome == "T"),
      point_diff = sum(points_for - points_against),
      .groups = "drop"
    ) |>
    mutate(
      record = if_else(
        ties > 0,
        paste(wins, losses, ties, sep = "-"),
        paste(wins, losses, sep = "-")
      )
    )
}

standings <- record_through(team_results, completed_week)

last_week <- record_through(team_results, completed_week - 1) |>
  select(team, prev_games = games_played, prev_losses = losses,
         prev_record = record)


# =============================================================================
# 3. Historical reference (through the last completed season)
# =============================================================================

panel <- read_csv(
  file.path(data_dir, "team_week_records.csv"),
  show_col_types = FALSE
)
record_outcomes <- read_csv(
  file.path(data_dir, "record_outcomes_by_games.csv"),
  show_col_types = FALSE
) |>
  filter(baseline == history_baseline) |>
  select(-baseline)

history <- panel |>
  filter(!is_bye, history_baseline == "full" | season >= 1999) |>
  mutate(loss_equiv = losses + 0.5 * ties)

history_seasons <- paste(min(history$season), max(history$season), sep = "-")

# Smoothed odds. Exact-record champion rates are noisy (27 champions spread
# over ~270 records): raw rates rank 7-1 above 8-0, for example. For each
# games-played count, the rates by losses (ties count as half a loss) are
# smoothed with weighted isotonic regression: neighbouring records are pooled
# until odds never rise as losses rise. Unlike a logistic curve, this stays
# close to what actually happened at the extremes (it doesn't turn one 16-0
# team into a 50% title favorite, or give 0-3 teams 8% playoff odds).

# Pool-adjacent-violators: non-increasing fit of rates y with weights w
pava_decreasing <- function(y, w) {
  blocks <- tibble(y = y, w = w, n = 1L)
  i <- 1
  while (i < nrow(blocks)) {
    if (blocks$y[i] < blocks$y[i + 1]) {
      w_sum <- blocks$w[i] + blocks$w[i + 1]
      blocks$y[i] <- (blocks$y[i] * blocks$w[i] +
                        blocks$y[i + 1] * blocks$w[i + 1]) / w_sum
      blocks$w[i] <- w_sum
      blocks$n[i] <- blocks$n[i] + blocks$n[i + 1]
      blocks <- blocks[-(i + 1), ]
      i <- max(i - 1, 1)
    } else {
      i <- i + 1
    }
  }
  rep(blocks$y, blocks$n)
}

smooth_odds <- function(data, outcome) {
  data |>
    group_by(games_played, loss_equiv) |>
    summarise(n = n(), rate = mean(.data[[outcome]]), .groups = "drop") |>
    arrange(games_played, loss_equiv) |>
    group_by(games_played) |>
    mutate(odds = pava_decreasing(rate, n)) |>
    ungroup() |>
    select(games_played, loss_equiv, odds)
}

odds_table <- smooth_odds(history, "sb_champion") |>
  rename(title_odds = odds) |>
  left_join(
    smooth_odds(history, "made_playoffs") |> rename(playoff_odds = odds),
    by = c("games_played", "loss_equiv")
  )

# Worst record (most losses) any eventual champion had after N games
champion_floor <- history |>
  filter(sb_champion) |>
  group_by(games_played) |>
  summarise(floor_losses = max(losses), .groups = "drop")

# Champions that were at the floor, per games played (for floor-watch text)
floor_examples <- history |>
  filter(sb_champion) |>
  inner_join(champion_floor, by = "games_played") |>
  filter(losses == floor_losses) |>
  group_by(games_played) |>
  summarise(
    floor_examples = paste(unique(paste(season, team)), collapse = ", "),
    .groups = "drop"
  )


# =============================================================================
# 4. Where every team stands
# =============================================================================

team_report <- standings |>
  left_join(
    record_outcomes |>
      select(games_played, wins, losses, ties,
             hist_teams = teams, hist_champions = champions,
             hist_playoff_teams = playoff_teams, champion_seasons),
    by = c("games_played", "wins", "losses", "ties")
  ) |>
  mutate(loss_equiv = losses + 0.5 * ties) |>
  left_join(odds_table, by = c("games_played", "loss_equiv")) |>
  left_join(champion_floor, by = "games_played") |>
  left_join(floor_examples, by = "games_played") |>
  left_join(last_week, by = "team") |>
  left_join(
    champion_floor |> select(prev_games = games_played,
                             prev_floor_losses = floor_losses),
    by = "prev_games"
  ) |>
  mutate(
    prev_past_floor = prev_losses > prev_floor_losses,
    across(c(hist_teams, hist_champions, hist_playoff_teams), \(x)
      coalesce(x, 0L)),
    unbeaten = losses == 0 & ties == 0,
    # No champion in the baseline ever had this many losses at this point
    past_floor = losses > floor_losses,
    at_floor = losses == floor_losses,
    # This exact record has never produced a champion (and is common enough
    # for that to mean something)
    never_champion = hist_champions == 0 & hist_teams >= 10
  ) |>
  # The three-tier classification (graveyard_tiers.R), both baselines
  left_join(
    tiers_wide |>
      filter(week == completed_week) |>
      select(team, tier_full, tier_modern, prev_tier_full, prev_tier_modern,
             movement_full, movement_modern,
             losses_past_playoff_full, losses_past_playoff_modern,
             playoff_threshold_full, playoff_threshold_modern,
             weeks_in_tier_full),
    by = "team"
  ) |>
  arrange(desc(title_odds), desc(point_diff)) |>
  select(
    team, record, games_played, point_diff, prev_record,
    title_odds, playoff_odds,
    hist_teams, hist_champions, hist_playoff_teams, champion_seasons,
    unbeaten, at_floor, past_floor, never_champion,
    prev_past_floor, floor_losses, floor_examples,
    tier_full, tier_modern, prev_tier_full, prev_tier_modern,
    movement_full, movement_modern,
    losses_past_playoff_full, losses_past_playoff_modern,
    playoff_threshold_full, playoff_threshold_modern, weeks_in_tier_full
  )

output_path <- file.path(
  weekly_dir,
  sprintf("%d_week%02d.csv", current_season, completed_week)
)
write_csv(team_report, output_path)


# =============================================================================
# 5. Ready-to-post summary
# =============================================================================

pct <- function(x) {
  case_when(
    x == 0 ~ "0%",
    x < 0.001 ~ "<0.1%",
    x < 0.01 ~ sprintf("%.1f%%", 100 * x),
    TRUE ~ sprintf("%.0f%%", 100 * x)
  )
}

# One line per record, listing every team that has it
history_lines <- function(d, note = "") {
  d |>
    group_by(record, hist_teams, hist_champions, hist_playoff_teams) |>
    summarise(teams = paste(team, collapse = ", "), .groups = "drop") |>
    arrange(desc(hist_champions / hist_teams)) |>
    transmute(sprintf(
      "%s (%s): %d teams since %s, %d champion%s, %d made playoffs%s",
      record, teams, hist_teams, min(history$season), hist_champions,
      if_else(hist_champions == 1, "", "s"), hist_playoff_teams, note
    )) |>
    pull()
}

# section() comes from graveyard_tiers.R

cat(strrep("=", 70), "\n")
cat(sprintf("%d Week %d: what history says about every record\n",
            current_season, completed_week))
cat(sprintf("Reference: every team-season %s (%d Super Bowl champions)\n",
            history_seasons, n_distinct(history$season)))
cat(strrep("=", 70), "\n")

section(
  "Title odds by record (smoothed), best to worst",
  team_report |>
    group_by(record, title_odds, playoff_odds) |>
    summarise(teams = paste(team, collapse = ", "), .groups = "drop") |>
    arrange(desc(title_odds)) |>
    transmute(sprintf("%-6s title %6s | playoffs %4s | %s", record,
                      pct(title_odds), pct(playoff_odds), teams)) |>
    pull()
)

section(
  "Unbeaten tracker",
  team_report |> filter(unbeaten) |> history_lines()
)

# --- The three tiers (full baseline, 1966 on, is primary) -------------------
# Championship Path: no more losses than any eventual champion at this point
# Playoff Path:      more than that, but no more than any eventual playoff team
# Graveyard:         more losses than any eventual playoff team

tier_counts_now <- team_report |> count(tier_full, .drop = FALSE)
cat(sprintf(
  "\nTiers after week %d (full baseline, 1966 on): %d Championship Path, %d Playoff Path, %d Graveyard\n",
  completed_week,
  tier_counts_now$n[tier_counts_now$tier_full == "Championship Path"],
  tier_counts_now$n[tier_counts_now$tier_full == "Playoff Path"],
  tier_counts_now$n[tier_counts_now$tier_full == "Graveyard"]
))

section(
  "The Graveyard: more losses than any playoff team since 1966 had at this point",
  team_report |>
    filter(tier_full == "Graveyard") |>
    arrange(desc(losses_past_playoff_full), title_odds) |>
    transmute(sprintf(
      "%-4s %-6s %d past the playoff limit of %d after %d games, week %d in the Graveyard%s",
      team, record, losses_past_playoff_full, playoff_threshold_full,
      games_played, weeks_in_tier_full,
      if_else(movement_full == "entered Graveyard", "  << ENTERED THIS WEEK", "")
    )) |>
    pull()
)

section(
  "Left the Graveyard this week: the limit stepped up, so they are back on the Playoff Path (no team that entered the Graveyard has ever made the playoffs)",
  team_report |>
    filter(movement_full == "left Graveyard") |>
    transmute(sprintf("%-4s %-6s now %s (limit %d after %d games)",
                      team, record, tier_full, playoff_threshold_full,
                      games_played)) |>
    pull()
)

section(
  "Playoff Path: past the worst start of any champion, but not of every playoff team",
  team_report |>
    filter(tier_full == "Playoff Path") |>
    group_by(record, games_played, playoff_threshold_full,
             losses_past_playoff_full) |>
    summarise(
      teams = paste(team, collapse = ", "),
      new = paste(team[movement_full == "down to Playoff Path"],
                  collapse = ", "),
      .groups = "drop"
    ) |>
    arrange(desc(losses_past_playoff_full), record) |>
    transmute(sprintf("%-6s (%s): %s the playoff limit of %d after %d games%s",
                      record, teams,
                      if_else(losses_past_playoff_full == 0, "at",
                              sprintf("%d short of", -losses_past_playoff_full)),
                      playoff_threshold_full, games_played,
                      if_else(new == "", "", paste0("  << NEW THIS WEEK: ", new)))) |>
    pull()
)

section(
  "Where the modern baseline (1999 on) disagrees",
  team_report |>
    filter(tier_full != tier_modern) |>
    transmute(sprintf("%-4s %-6s full: %s | modern: %s", team, record,
                      tier_full, tier_modern)) |>
    pull()
)

section(
  "Floor watch: tied with the worst start of any eventual champion",
  team_report |>
    filter(at_floor, !unbeaten) |>
    group_by(record, floor_examples) |>
    summarise(teams = paste(team, collapse = ", "), .groups = "drop") |>
    transmute(sprintf("%s (%s): same as %s", record, teams,
                      floor_examples)) |>
    pull()
)

section(
  "Records that have never produced a champion (but aren't past the floor)",
  team_report |> filter(never_champion, !past_floor) |> history_lines()
)

cat("\nSaved team-by-team table to", output_path, "\n")
