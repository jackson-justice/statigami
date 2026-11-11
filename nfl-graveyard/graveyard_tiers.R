# =============================================================================
# NFL Graveyard: three tiers for the current season
#
# After every completed week, every team goes in one of three tiers, by
# comparing its losses with the most losses any eventual Super Bowl champion
# and any eventual playoff team had after the same number of games:
#   Championship Path: no more losses than the champion threshold
#   Playoff Path:      more than that, but no more than the playoff threshold
#   Graveyard:         more losses than any eventual playoff team
#
# The thresholds come from analysis.R (data/loss_thresholds_by_games.csv) and
# are frozen at the end of the last completed season: the current season never
# feeds back into them. They count losses only (a tie is not a loss) and are
# carried forward so they never decrease in games played. Tiers are computed
# on both baselines: "full" (1966 on) is the primary tier and "modern" (1999
# on) is carried alongside for sensitivity.
#
# A team can leave the Graveyard when the playoff threshold steps up at the
# next game count. That is a return to the Playoff Path, not evidence of a
# comeback: no team in either baseline that entered the Graveyard went on to
# make the playoffs. A team still in the Graveyard would have to set a new
# historical record to make the playoffs (losses_past_playoff says by how
# much).
#
# Every run re-downloads the current season's scores and rebuilds every
# completed week (the backfill), so the outputs always cover the season to
# date. Indexing is by games played, not calendar week, so byes don't matter.
# weekly_update.R sources this script.
#
# Requires: data/loss_thresholds_by_games.csv (from analysis.R)
# Writes:   data/graveyard/<season>_tiers.csv        team x week, both tiers
#           data/graveyard/<season>_movements.csv    every tier change
#           data/graveyard/<season>_tier_counts.csv  tier counts by week
#
# Run from the statigami repo root (where statigami.Rproj lives):
#   source("nfl-graveyard/graveyard_tiers.R")
# =============================================================================

library(tidyverse)

project_dir <- "nfl-graveyard"
data_dir <- file.path(project_dir, "data")
raw_dir <- file.path(data_dir, "raw")
weekly_dir <- file.path(data_dir, "weekly")
graveyard_dir <- file.path(data_dir, "graveyard")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graveyard_dir, recursive = TRUE, showWarnings = FALSE)

tier_levels <- c("Championship Path", "Playoff Path", "Graveyard")
primary_baseline <- "full"
secondary_baseline <- "modern"


# =============================================================================
# 1. Frozen thresholds (from analysis.R)
# =============================================================================

thresholds <- read_csv(
  file.path(data_dir, "loss_thresholds_by_games.csv"),
  show_col_types = FALSE
) |>
  select(
    baseline, games_played, seasons,
    champion_max_losses, playoff_max_losses,
    champion_threshold, playoff_threshold
  ) |>
  arrange(baseline, games_played)

classify_tier <- function(losses, champion_threshold, playoff_threshold) {
  factor(
    case_when(
      losses <= champion_threshold ~ "Championship Path",
      losses <= playoff_threshold ~ "Playoff Path",
      TRUE ~ "Graveyard"
    ),
    levels = tier_levels
  )
}


# =============================================================================
# 2. Current-season results (always a fresh download)
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

# Once the playoffs start: who is in them, and who won the Super Bowl. Empty
# until then. A playoff team with a Graveyard week, or a champion with a
# Playoff Path week, would break a historical threshold outright.
playoff_games_current <- games_current |>
  filter(season == current_season, game_type != "REG", !is.na(result))

playoffs_started <- nrow(playoff_games_current) > 0

playoff_teams_current <- playoff_games_current |>
  pivot_longer(c(home_team, away_team), values_to = "team") |>
  distinct(team) |>
  pull(team)

champion_current <- playoff_games_current |>
  filter(game_type == "SB") |>
  transmute(champion = if_else(home_score > away_score, home_team, away_team)) |>
  pull(champion)

# One row per team per game played
team_results <- completed_games |>
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
  select(-home_opp, -away_opp)


# =============================================================================
# 3. Team-week panel for the current season
# =============================================================================
# Same shape as team_week_records.csv: one row per team per completed week,
# byes included, with the cumulative record after that week's game.

season_teams <- season_games |>
  pivot_longer(c(home_team, away_team), values_to = "team") |>
  distinct(team) |>
  arrange(team)

panel_current <- expand_grid(season_teams, week = seq_len(completed_week)) |>
  left_join(
    team_results |>
      select(week, team, opponent, home_away, outcome, points_for,
             points_against),
    by = c("team", "week")
  ) |>
  arrange(team, week) |>
  group_by(team) |>
  mutate(
    played = !is.na(outcome),
    games_played = cumsum(played),
    wins = cumsum(coalesce(outcome == "W", FALSE)),
    losses = cumsum(coalesce(outcome == "L", FALSE)),
    ties = cumsum(coalesce(outcome == "T", FALSE)),
    point_diff = cumsum(coalesce(points_for - points_against, 0))
  ) |>
  ungroup() |>
  mutate(
    season = current_season,
    is_bye = !played,
    record = if_else(
      ties > 0,
      paste(wins, losses, ties, sep = "-"),
      paste(wins, losses, sep = "-")
    )
  ) |>
  select(
    season, week, team, is_bye, opponent, home_away, outcome,
    games_played, wins, losses, ties, record, point_diff
  )


# =============================================================================
# 4. Tiers on both baselines, and week-to-week movements
# =============================================================================

tiers_long <- panel_current |>
  cross_join(tibble(baseline = c(primary_baseline, secondary_baseline))) |>
  left_join(
    thresholds |>
      select(baseline, games_played, champion_threshold, playoff_threshold),
    by = c("baseline", "games_played")
  ) |>
  mutate(
    tier = classify_tier(losses, champion_threshold, playoff_threshold),
    # Positive = this many losses past any playoff team at this point, so
    # making the playoffs would set a new historical record
    losses_past_playoff = losses - playoff_threshold
  ) |>
  arrange(baseline, team, week) |>
  group_by(baseline, team) |>
  mutate(
    prev_tier = lag(tier),
    prev_losses = lag(losses),
    prev_champion_threshold = lag(champion_threshold),
    prev_playoff_threshold = lag(playoff_threshold),
    movement = case_when(
      is.na(prev_tier) | tier == prev_tier ~ "none",
      tier == "Graveyard" ~ "entered Graveyard",
      prev_tier == "Graveyard" ~ "left Graveyard",
      as.integer(tier) > as.integer(prev_tier) ~ paste("down to", tier),
      TRUE ~ paste("up to", tier)
    ),
    # What moved the team: its own loss, or the threshold stepping up at the
    # new game count (the only way to move up)
    trigger = case_when(
      movement == "none" ~ NA_character_,
      losses > prev_losses ~ "loss",
      TRUE ~ "threshold stepped up"
    ),
    tier_run = cumsum(coalesce(tier != lag(tier), TRUE))
  ) |>
  group_by(baseline, team, tier_run) |>
  mutate(weeks_in_tier = row_number()) |>
  ungroup() |>
  select(-tier_run)

# One row per team-week, with the two baselines side by side
tiers_wide <- tiers_long |>
  select(
    season, week, team, is_bye, opponent, home_away, outcome,
    games_played, wins, losses, ties, record, point_diff,
    baseline, champion_threshold, playoff_threshold, tier, prev_tier,
    movement, trigger, losses_past_playoff, weeks_in_tier
  ) |>
  pivot_wider(
    names_from = baseline,
    values_from = c(champion_threshold, playoff_threshold, tier, prev_tier,
                    movement, trigger, losses_past_playoff, weeks_in_tier),
    names_glue = "{.value}_{baseline}",
    names_vary = "slowest"
  ) |>
  mutate(
    tiers_disagree = tier_full != tier_modern,
    # NA until the playoffs start; then whether the team played a playoff
    # game, and whether it won the Super Bowl (NA until it is played)
    made_playoffs = if (playoffs_started) team %in% playoff_teams_current else NA,
    sb_champion = if (length(champion_current) == 1) team == champion_current else NA,
    # TRUE when the season's outcome contradicts the tier: a playoff team in
    # the Graveyard, or a champion off the Championship Path
    breaks_threshold_full = (made_playoffs & tier_full == "Graveyard") |
      (sb_champion & tier_full != "Championship Path"),
    breaks_threshold_modern = (made_playoffs & tier_modern == "Graveyard") |
      (sb_champion & tier_modern != "Championship Path")
  ) |>
  arrange(week, team)

movements <- tiers_long |>
  filter(movement != "none") |>
  transmute(
    season, week, team, baseline, games_played, record, outcome, losses,
    from_tier = prev_tier, to_tier = tier, movement, trigger,
    champion_threshold, playoff_threshold
  ) |>
  arrange(week, team, baseline)

tier_counts <- tiers_long |>
  count(season, week, baseline, tier, .drop = FALSE) |>
  pivot_wider(names_from = tier, values_from = n, values_fill = 0) |>
  rename(
    championship_path = `Championship Path`,
    playoff_path = `Playoff Path`,
    graveyard = `Graveyard`
  ) |>
  arrange(baseline, week)


# =============================================================================
# 5. Validation checks
# =============================================================================
# Every check must pass before anything is saved.

n_teams <- nrow(season_teams)

threshold_runs <- thresholds |>
  group_by(baseline) |>
  summarise(
    monotone = all(diff(champion_threshold) >= 0) &&
      all(diff(playoff_threshold) >= 0),
    carried = all(champion_threshold == cummax(champion_max_losses)) &&
      all(playoff_threshold == cummax(playoff_max_losses)),
    .groups = "drop"
  )

team_weeks <- panel_current |>
  group_by(team) |>
  summarise(
    games = max(games_played), byes = sum(is_bye), weeks = n(),
    .groups = "drop"
  )

down_moves <- tiers_long |> filter(str_starts(movement, "down|entered"))
up_moves <- tiers_long |> filter(str_starts(movement, "up|left"))

# Any weekly tables already saved for this season (weekly_update.R) must show
# the same records the backfill produces for that week.
weekly_files <- list.files(
  weekly_dir,
  pattern = sprintf("^%d_week\\d{2}\\.csv$", current_season),
  full.names = TRUE
)
weekly_records_match <- map_lgl(weekly_files, function(path) {
  wk <- as.integer(str_extract(basename(path), "(?<=week)\\d{2}"))
  saved <- read_csv(path, show_col_types = FALSE) |>
    select(team, saved_record = record, saved_games = games_played)
  backfill <- panel_current |> filter(week == wk)
  nrow(saved) == nrow(backfill) &&
    backfill |>
      inner_join(saved, by = "team") |>
      summarise(ok = n() == nrow(saved) &&
                  all(record == saved_record & games_played == saved_games)) |>
      pull(ok)
})

checks <- tibble(
  check = c(
    "Thresholds never decrease in games played, on either baseline",
    "Threshold columns are the raw maxima carried forward",
    "Thresholds exist for every games-played count reached this season",
    "Every team has one row for every completed week, on both baselines",
    "Every team-week has a tier on both baselines",
    "Games + byes = completed weeks for every team",
    "Cumulative records never decrease week to week",
    "Tiers agree with the thresholds when re-evaluated directly",
    "Downward moves happen only when the team lost",
    "Upward moves happen only when the threshold stepped up and the team did not lose",
    "No team changes tier on a bye week",
    "The full-baseline tier is never worse than the modern one",
    "Saved weekly tables for this season match the backfill's records"
  ),
  passed = c(
    all(threshold_runs$monotone),
    all(threshold_runs$carried),
    !any(is.na(tiers_long$champion_threshold)) &&
      !any(is.na(tiers_long$playoff_threshold)),
    nrow(tiers_long) == n_teams * completed_week * 2 &&
      nrow(distinct(tiers_long, baseline, team, week)) == nrow(tiers_long),
    !any(is.na(tiers_long$tier)),
    all(team_weeks$games + team_weeks$byes == team_weeks$weeks) &&
      all(team_weeks$weeks == completed_week),
    panel_current |>
      group_by(team) |>
      summarise(ok = all(diff(wins) >= 0 & diff(losses) >= 0 &
                           diff(ties) >= 0), .groups = "drop") |>
      pull(ok) |>
      all(),
    with(tiers_long,
         all((tier == "Championship Path") == (losses <= champion_threshold)) &&
           all((tier == "Graveyard") == (losses > playoff_threshold))),
    all(down_moves$losses == down_moves$prev_losses + 1) &&
      all(down_moves$trigger == "loss"),
    all(up_moves$losses == up_moves$prev_losses) &&
      all(up_moves$trigger == "threshold stepped up") &&
      all(up_moves$champion_threshold > up_moves$prev_champion_threshold |
            up_moves$playoff_threshold > up_moves$prev_playoff_threshold),
    all(tiers_long$movement[tiers_long$is_bye] == "none"),
    all(as.integer(tiers_wide$tier_full) <= as.integer(tiers_wide$tier_modern)),
    all(weekly_records_match)
  )
)

print(checks, n = Inf)

if (!all(checks$passed)) {
  stop("Graveyard validation failed:\n",
       paste("-", checks$check[!checks$passed], collapse = "\n"))
}

message("All ", nrow(checks), " Graveyard checks passed (",
        current_season, " through week ", completed_week, ").")

write_csv(
  tiers_wide,
  file.path(graveyard_dir, sprintf("%d_tiers.csv", current_season))
)
write_csv(
  movements,
  file.path(graveyard_dir, sprintf("%d_movements.csv", current_season))
)
write_csv(
  tier_counts,
  file.path(graveyard_dir, sprintf("%d_tier_counts.csv", current_season))
)


# =============================================================================
# 6. Report
# =============================================================================

section <- function(title, lines) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  if (length(lines) == 0) {
    cat("  (none)\n")
  } else {
    cat(paste0("  ", lines, "\n"), sep = "")
  }
}

cat(strrep("=", 70), "\n")
cat(sprintf("%d NFL Graveyard tiers through week %d\n",
            current_season, completed_week))
cat(sprintf("Primary: %s baseline (%d on); alongside: %s baseline (%d on)\n",
            primary_baseline, 1966, secondary_baseline, 1999))
cat(strrep("=", 70), "\n")

section(
  "Tier counts by week: Championship Path / Playoff Path / Graveyard (modern in brackets where different)",
  tier_counts |>
    pivot_wider(
      names_from = baseline,
      values_from = c(championship_path, playoff_path, graveyard)
    ) |>
    transmute(sprintf(
      "Week %2d: %2d / %2d / %2d%s", week,
      championship_path_full, playoff_path_full, graveyard_full,
      if_else(
        championship_path_full == championship_path_modern &
          graveyard_full == graveyard_modern,
        "",
        sprintf("   [modern %2d / %2d / %2d]", championship_path_modern,
                playoff_path_modern, graveyard_modern)
      )
    )) |>
    pull()
)

movement_lines <- function(d) {
  d |>
    transmute(sprintf(
      "Week %2d: %-4s %-6s (%s, %d games) %s, %s; thresholds %d / %d",
      week, team, record, outcome, games_played, movement, trigger,
      champion_threshold, playoff_threshold
    )) |>
    pull()
}

section(
  "Graveyard entries and exits, full baseline",
  movements |>
    filter(baseline == primary_baseline,
           movement %in% c("entered Graveyard", "left Graveyard")) |>
    movement_lines()
)

section(
  "Graveyard entries and exits, modern baseline",
  movements |>
    filter(baseline == secondary_baseline,
           movement %in% c("entered Graveyard", "left Graveyard")) |>
    movement_lines()
)

section(
  "Team-weeks where the two baselines disagree",
  tiers_wide |>
    filter(tiers_disagree) |>
    transmute(sprintf("Week %2d: %-4s %-6s (%d games) full: %s | modern: %s",
                      week, team, record, games_played, tier_full,
                      tier_modern)) |>
    pull()
)

section(
  sprintf("In the Graveyard after week %d (full baseline): a playoff berth would set a record",
          completed_week),
  tiers_wide |>
    filter(week == completed_week, tier_full == "Graveyard") |>
    arrange(desc(losses_past_playoff_full), wins) |>
    transmute(sprintf(
      "%-4s %-6s %d past the playoff limit of %d after %d games, week %d in the Graveyard",
      team, record, losses_past_playoff_full, playoff_threshold_full,
      games_played, weeks_in_tier_full
    )) |>
    pull()
)

if (playoffs_started) {
  section(
    "Threshold breakers: playoff teams with a Graveyard week, or a champion with a week off the Championship Path",
    tiers_wide |>
      filter(breaks_threshold_full %in% TRUE | breaks_threshold_modern %in% TRUE) |>
      transmute(sprintf(
        "Week %2d: %-4s %-6s (%d games) full: %s | modern: %s%s",
        week, team, record, games_played, tier_full, tier_modern,
        if_else(sb_champion %in% TRUE, "  (Super Bowl champion)", "  (playoff team)")
      )) |>
      pull()
  )
}

cat("\nSaved", basename(graveyard_dir), "tables for", current_season,
    "to", graveyard_dir, "\n")


# =============================================================================
# 7. Figures (full baseline), refreshed every run
# =============================================================================

source(file.path(project_dir, "chart_setup.R"))

thresholds_primary <- thresholds |> filter(baseline == primary_baseline)
history_seasons <- read_csv(file.path(data_dir, "team_week_records.csv"),
                            show_col_types = FALSE, col_select = "season") |>
  pull(season) |>
  range()
figure_caption <- sprintf(
  "// SOURCE · nflverse · %d through week %d · floors from every team-season %d-%d",
  current_season, completed_week, history_seasons[1], history_seasons[2]
)
tier_fill <- scale_fill_manual(values = tier_colors, breaks = tier_levels,
                               drop = FALSE)

# --- 7a. Every team's tier after every week ----------------------------------
# One tile per team-week. Teams are sorted by their current tier and record,
# so the Graveyard sits at the bottom. The letter is that week's result; a
# faded tile is a bye (the tier carries over); a ring marks a team that left
# the Graveyard because the floor stepped up at its next game.

team_order <- tiers_wide |>
  filter(week == completed_week) |>
  arrange(tier_full, losses, desc(wins), team) |>
  pull(team)

tiles <- tiers_wide |>
  mutate(
    team = factor(team, levels = rev(team_order)),
    tier_full = factor(tier_full, levels = tier_levels),
    text_color = tier_text_color(tier_full)
  )

current_records <- tiles |>
  filter(week == completed_week) |>
  select(team, record, tier_full)

fig_tiles <- ggplot(tiles, aes(week, team)) +
  geom_tile(aes(fill = tier_full, alpha = !is_bye),
            color = chart_surface, linewidth = 0.8) +
  geom_tile(data = filter(tiles, movement_full == "left Graveyard"),
            fill = NA, color = chart_fg, linewidth = 0.9,
            width = 0.82, height = 0.82) +
  geom_text(aes(label = coalesce(outcome, ""), color = text_color),
            family = chart_mono, size = 2.6, fontface = "bold") +
  geom_text(data = current_records,
            aes(x = completed_week + 0.75, label = record),
            family = chart_mono, size = 2.6, hjust = 0, color = chart_fg_2) +
  tier_fill +
  scale_color_identity() +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  scale_x_continuous(breaks = seq_len(completed_week), position = "top",
                     expand = expansion(add = c(0.5, 1.6))) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  labs(
    title = "The Graveyard, week by week",
    subtitle = sprintf(paste(
      "Every team's tier after each week of %d, against floors set by 60",
      "seasons of history.\nLetter = that week's result. Faded = bye.",
      "Ring = left the Graveyard when the floor stepped up."
    ), current_season),
    x = NULL, y = NULL,
    caption = figure_caption
  ) +
  theme_chart(grid = "none") +
  theme(
    legend.position = "top",
    axis.text.y = element_text(family = chart_mono),
    axis.ticks = element_blank()
  )

save_figure(fig_tiles, sprintf("02_tier_tiles_%d.png", current_season),
            format = "article_tall", height = 9.5)

# --- 7b. Loss paths against the floors --------------------------------------
# Losses after each game for every team, on top of the three bands. Teams
# that have been in the Graveyard are drawn in full; the rest are the faint
# bundle. Paths sit on an integer lattice, so each team gets a small fixed
# offset to keep the strands apart.

y_max <- max(thresholds_primary$playoff_threshold) + 3

graveyard_teams <- tiers_long |>
  filter(baseline == primary_baseline, tier == "Graveyard") |>
  distinct(team) |>
  pull(team)

paths <- panel_current |>
  filter(!is_bye) |>
  mutate(
    team_index = as.integer(factor(team)),
    x = games_played + ((team_index - 1) %/% 8 - 1.5) * 0.09,
    y = losses + ((team_index - 1) %% 8 - 3.5) * 0.07,
    highlighted = team %in% graveyard_teams
  )

# One label per endpoint: teams with the same record share a label
path_ends <- paths |>
  filter(highlighted) |>
  slice_max(games_played, n = 1, by = team) |>
  summarise(
    x = max(x), y = mean(y),
    label = paste(paste(team, collapse = ", "), first(record)),
    .by = c(games_played, record)
  )

band_labels <- tibble(
  tier = factor(tier_levels, levels = tier_levels),
  y = c(
    0.5,
    with(thresholds_primary, mean(c(champion_threshold[17], playoff_threshold[17]))),
    thresholds_primary$playoff_threshold[17] + 1.5
  ),
  label = toupper(tier_levels)
)

fig_paths <- ggplot() +
  geom_rect(data = tier_bands(thresholds_primary, y_max),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = tier),
            alpha = 0.22) +
  geom_step(data = tier_steps(thresholds_primary, "champion_threshold"),
            aes(x, y), direction = "hv", color = tier_colors[[2]],
            linewidth = 0.6) +
  geom_step(data = tier_steps(thresholds_primary, "playoff_threshold"),
            aes(x, y), direction = "hv", color = tier_colors[[3]],
            linewidth = 0.6) +
  geom_text(data = band_labels, aes(x = 17.3, y = y, label = label, color = tier),
            family = chart_mono, size = 2.5, hjust = 1, fontface = "bold") +
  geom_line(data = filter(paths, !highlighted), aes(x, y, group = team),
            color = chart_fg_3, linewidth = 0.4) +
  geom_line(data = filter(paths, highlighted), aes(x, y, group = team),
            color = chart_fg, linewidth = 1) +
  geom_point(data = filter(paths, highlighted), aes(x, y),
             color = chart_fg, size = 1.4) +
  geom_text(data = path_ends, aes(x, y, label = label),
            family = chart_mono, size = 2.7, hjust = 0, nudge_x = 0.3,
            color = chart_fg) +
  tier_fill +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(breaks = 1:17) +
  scale_y_continuous(breaks = seq(0, y_max, 2)) +
  coord_cartesian(xlim = c(0.5, 17.5), ylim = c(-0.5, y_max + 0.5),
                  expand = FALSE) +
  labs(
    title = "Three floors, and who has fallen through them",
    subtitle = sprintf(paste(
      "Losses after each game, %d through week %d. Lines: the most losses",
      "any eventual\nchampion (lower) and any eventual playoff team (upper)",
      "had at that point, 1966-2025.\nNobody above the upper line has ever",
      "made the playoffs."
    ), current_season, completed_week),
    x = "Games played", y = "Losses",
    caption = figure_caption
  ) +
  theme_chart(grid = "none") +
  theme(legend.position = "top")

save_figure(fig_paths, sprintf("03_loss_paths_%d.png", current_season),
            format = "article", height = 5.5)

# --- 7c. How many teams are in each tier -------------------------------------

counts_plot <- tier_counts |>
  filter(baseline == primary_baseline) |>
  pivot_longer(c(championship_path, playoff_path, graveyard),
               names_to = "tier", values_to = "teams") |>
  mutate(
    tier = factor(tier_levels[match(tier, c("championship_path", "playoff_path",
                                             "graveyard"))], levels = tier_levels),
    text_color = tier_text_color(tier),
    # A one-team segment is too thin for a label
    label = if_else(teams >= 2, as.character(teams), "")
  ) |>
  # Stack with the Graveyard at the bottom; label each segment at its middle
  arrange(week, desc(tier)) |>
  mutate(label_y = cumsum(teams) - teams / 2, .by = week)

fig_counts <- ggplot(counts_plot, aes(factor(week), teams, fill = tier)) +
  geom_col(color = chart_surface, linewidth = 0.7, width = 0.78) +
  geom_text(aes(y = label_y, label = label, color = text_color),
            family = chart_mono, size = 3, fontface = "bold") +
  tier_fill +
  scale_color_identity() +
  scale_y_continuous(breaks = seq(0, 32, 8), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "The Graveyard fills slowly",
    subtitle = sprintf(paste(
      "Teams in each tier after every completed week of %d. The floors rise",
      "with games\nplayed, so a bad week can drop a team and a good week at",
      "the right moment can\nlift one back out."
    ), current_season),
    x = "Week", y = "Teams",
    caption = figure_caption
  ) +
  theme_chart(grid = "y") +
  theme(legend.position = "top")

save_figure(fig_counts, sprintf("04_tier_counts_%d.png", current_season),
            format = "article")

cat("Saved figures to", figures_dir, "\n")
