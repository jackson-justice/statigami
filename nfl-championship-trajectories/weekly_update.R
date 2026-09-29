# =============================================================================
# NFL Championship Trajectories: weekly update
#
# Run every Tuesday after Monday Night Football. For every team in the current
# season, this script compares its record with every team since 1999 that had
# the same record after the same number of games, then prints a ready-to-post
# summary.
#
# Requires the outputs of analysis.R (run that first, once per offseason):
#   data/team_week_records.csv
#   data/record_outcomes_by_games.csv
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


# =============================================================================
# 1. Current-season results (always a fresh download)
# =============================================================================
# analysis.R caches its copy of games.csv; this script needs today's scores,
# so it downloads to a separate file and overwrites it every run.

games_url <- "https://github.com/nflverse/nfldata/raw/master/data/games.csv"
current_path <- file.path(raw_dir, "games_current.csv")
download.file(games_url, current_path, mode = "wb", quiet = TRUE)

games_current <- read_csv(current_path, show_col_types = FALSE)

current_season <- max(games_current$season)

season_games <- games_current |>
  filter(season == current_season, game_type == "REG")

# Latest week in which every scheduled game has a final score
completed_week <- season_games |>
  group_by(week) |>
  summarise(done = all(!is.na(result)), .groups = "drop") |>
  filter(done) |>
  pull(week) |>
  max()

completed_games <- season_games |>
  filter(week <= completed_week, !is.na(result))


# =============================================================================
# 2. Current records, and last week's records for comparison
# =============================================================================

team_results <- completed_games |>
  pivot_longer(
    c(home_team, away_team),
    names_to = "home_away",
    values_to = "team"
  ) |>
  mutate(
    home_away = str_remove(home_away, "_team"),
    points_for = if_else(home_away == "home", home_score, away_score),
    points_against = if_else(home_away == "home", away_score, home_score),
    outcome = case_when(
      points_for > points_against ~ "W",
      points_for < points_against ~ "L",
      TRUE ~ "T"
    )
  )

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
# 3. Historical reference (1999 through the last completed season)
# =============================================================================

panel <- read_csv(
  file.path(data_dir, "team_week_records.csv"),
  show_col_types = FALSE
)
record_outcomes <- read_csv(
  file.path(data_dir, "record_outcomes_by_games.csv"),
  show_col_types = FALSE
)

history <- panel |>
  filter(!is_bye) |>
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
    # No champion since 1999 ever had this many losses at this point
    past_floor = losses > floor_losses,
    at_floor = losses == floor_losses,
    # This exact record has never produced a champion (and is common enough
    # for that to mean something)
    never_champion = hist_champions == 0 & hist_teams >= 10
  ) |>
  arrange(desc(title_odds), desc(point_diff)) |>
  select(
    team, record, games_played, point_diff, prev_record,
    title_odds, playoff_odds,
    hist_teams, hist_champions, hist_playoff_teams, champion_seasons,
    unbeaten, at_floor, past_floor, never_champion,
    prev_past_floor, floor_losses, floor_examples
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

section <- function(title, lines) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  if (length(lines) == 0) {
    cat("  (none)\n")
  } else {
    cat(paste0("  ", lines, "\n"), sep = "")
  }
}

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

graveyard <- filter(team_report, past_floor)
section(
  "The Graveyard: no champion since 1999 was ever this far behind",
  c(
    graveyard |>
      filter(prev_past_floor %in% FALSE) |>
      history_lines(note = "  << NEW THIS WEEK"),
    graveyard |>
      filter(!(prev_past_floor %in% FALSE)) |>
      history_lines()
  )
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
