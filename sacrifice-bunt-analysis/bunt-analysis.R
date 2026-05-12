# Project: Sacrifice Bunt Analysis
# Goal: Identify the situations where sacrifice bunts are most effective

library(tidyverse)
library(baseballr)

# ============================================================================
# EXPLORATION / VALIDATION ####
# ============================================================================

# Bees Game ###################################################################
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

sac_bunts <- game_pbp_clean |>
  filter(result.eventType == "sac_bunt")

sac_bunts |> 
  select(contains("score"))

sac_bunts <- sac_bunts |>
  mutate(
    runner_on_1st_after = !is.na(matchup.postOnFirst.fullName),
    runner_on_2nd_after = !is.na(matchup.postOnSecond.fullName),
    runner_on_3rd_after = !is.na(matchup.postOnThird.fullName)
  )

sac_bunts <- sac_bunts |>
  mutate(
    base_state_after = case_when(
      runner_on_1st_after & !runner_on_2nd_after & !runner_on_3rd_after ~ "1st",
      !runner_on_1st_after & runner_on_2nd_after & !runner_on_3rd_after ~ "2nd",
      !runner_on_1st_after & !runner_on_2nd_after & runner_on_3rd_after ~ "3rd",
      runner_on_1st_after & runner_on_2nd_after & !runner_on_3rd_after ~ "1st & 2nd",
      runner_on_1st_after & !runner_on_2nd_after & runner_on_3rd_after ~ "1st & 3rd",
      !runner_on_1st_after & runner_on_2nd_after & runner_on_3rd_after ~ "2nd & 3rd",
      runner_on_1st_after & runner_on_2nd_after & runner_on_3rd_after ~ "Bases Loaded",
      TRUE ~ "Empty"
    )
  )

sac_bunts <- sac_bunts |> 
  mutate(
    away_score_after_bunt = result.awayScore,
    home_score_after_bunt = result.homeScore
  )

game_pbp_clean <- game_pbp_clean |>
  mutate(
    inning_id = paste(
      about.inning,
      about.halfInning
    )
  )

sac_bunts <- sac_bunts |>
  mutate(
    inning_id = paste(
      about.inning,
      about.halfInning
    )
  )

inning_final_scores <- game_pbp_clean |>
  group_by(inning_id) |>
  summarise(
    final_away_score = max(result.awayScore, na.rm = TRUE),
    final_home_score = max(result.homeScore, na.rm = TRUE)
  )

sac_bunts <- sac_bunts |> 
  left_join(inning_final_scores, by = "inning_id")

sac_bunts <- sac_bunts |>
  mutate(
    batting_team_runs_after_bunt = case_when(
      about.halfInning == "top" ~
        final_away_score - away_score_after_bunt,
      
      about.halfInning == "bottom" ~
        final_home_score - home_score_after_bunt
    )
  )

sac_bunts <- sac_bunts |>
  mutate(
    successful_bunt = batting_team_runs_after_bunt >= 1
  )

sac_bunts |> 
  summarize(
    success_rate = mean(successful_bunt)
  )

sac_bunts |>
  count(about.inning, successful_bunt)

sac_bunts |>
  count(base_state_after, successful_bunt)

sac_bunts |> 
  count(matchup.splits.menOnBase)

sac_bunts |> 
  select(result.description)


# MLB May 1st games ###########################################################
games <- mlb_game_pks(
  date = "2026-05-01",
  level_ids = c(1)
)

game_ids <- games$game_pk

all_pbp <- map_dfr(
  game_ids,
  mlb_pbp
)

all_pbp_clean <- all_pbp |> 
  filter(last.pitch.of.ab == "true")

sac_bunts_mlb_day <- all_pbp_clean |> 
  filter(result.eventType == "sac_bunt")

dates <- seq(
  as.Date("2026-03-25"),
  as.Date("2026-05-06"),
  by = "day"
)

all_games <- map_dfr(
  dates,
  ~ mlb_game_pks(
    date = .x,
    level_ids = c(1)
  )
)

all_game_ids <- all_games$game_pk

safe_mlb_pbp <- possibly(
  mlb_pbp,
  otherwise = tibble()
)

all_pbp <- map_dfr(
  all_game_ids,
  safe_mlb_pbp
)

all_pbp_clean <- all_pbp |> 
  filter(last.pitch.of.ab == "true")

sac_bunts <- all_pbp_clean |> 
  filter(result.eventType == "sac_bunt")

nrow(sac_bunts)
sac_bunts |> 
  count(about.inning)

all_pbp_clean <- all_pbp_clean |>
  mutate(
    inning_id = paste(
      game_pk,
      about.inning,
      about.halfInning
    )
  )

sac_bunts <- sac_bunts |>
  mutate(
    inning_id = paste(
      game_pk,
      about.inning,
      about.halfInning
    )
  )

inning_final_scores <- all_pbp_clean |>
  group_by(inning_id) |>
  summarise(
    final_away_score = max(result.awayScore, na.rm = TRUE),
    final_home_score = max(result.homeScore, na.rm = TRUE),
    .groups = "drop"
  )

sac_bunts <- sac_bunts |>
  left_join(inning_final_scores, by = "inning_id")

sac_bunts <- sac_bunts |>
  mutate(
    batting_team_runs_after_bunt = case_when(
      about.halfInning == "top" ~
        final_away_score - result.awayScore,
      
      about.halfInning == "bottom" ~
        final_home_score - result.homeScore
    )
  )

sac_bunts <- sac_bunts |>
  mutate(
    successful_bunt = batting_team_runs_after_bunt >= 1
  )

sac_bunts |>
  summarise(
    success_rate = mean(successful_bunt)
  )

sac_bunts |>
  group_by(about.inning) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

sac_bunts |>
  count(about.inning, sort = TRUE)

sac_bunts |>
  group_by(count.outs.start) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

sac_bunts |>
  group_by(
    count.outs.start,
    about.inning >= 7
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

sac_bunts_summary <- sac_bunts |>
  mutate(
    inning_bucket = if_else(
      about.inning >= 7,
      "Late (7+)",
      "Early/Mid"
    ),
    outs_bucket = paste0(
      count.outs.start,
      " Outs"
    )
  ) |>
  group_by(inning_bucket, outs_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

ggplot(
  sac_bunts_summary,
  aes(
    x = inning_bucket,
    y = success_rate,
    fill = outs_bucket
  )
) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "When Do Sacrifice Bunts Actually Work?",
    subtitle = "MLB 2026 season through May 6",
    x = NULL,
    y = "Run Scored Later in Inning"
  )



# ============================================================================
# FINAL DATA PIPELINE ####
# ============================================================================

# -----------------------------------------------------------------------------
# Create folders ====
# -----------------------------------------------------------------------------

dir.create(
  "sacrifice-bunt-analysis/data",
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------------------------------------------------------
# Season date ranges ====
# -----------------------------------------------------------------------------

season_dates <- list(
  
  "2023" = seq(
    as.Date("2023-03-30"),
    as.Date("2023-10-01"),
    by = "day"
  ),
  
  "2024" = seq(
    as.Date("2024-03-28"),
    as.Date("2024-09-29"),
    by = "day"
  ),
  
  "2025" = seq(
    as.Date("2025-03-27"),
    as.Date("2025-09-28"),
    by = "day"
  ),
  
  "2026" = seq(
    as.Date("2026-03-27"),
    as.Date("2026-05-07"),
    by = "day"
  )
)

# -----------------------------------------------------------------------------
# Safe PBP pull ====
# -----------------------------------------------------------------------------

safe_pull <- function(game_id) {
  
  tryCatch(
    
    mlb_pbp(game_id),
    
    error = function(e) {
      message(paste("FAILED:", game_id))
      return(NULL)
    }
  )
}

# -----------------------------------------------------------------------------
# Pull ONLY sacrifice bunt events ====
# -----------------------------------------------------------------------------

pull_sac_bunts <- function(game_id) {
  
  pbp <- safe_pull(game_id)
  
  if (is.null(pbp)) {
    return(NULL)
  }
  
  pbp |>
    filter(
      last.pitch.of.ab == "true",
      result.eventType == "sac_bunt"
    )
}

# -----------------------------------------------------------------------------
# Pull one full season of sacrifice bunts ====
# -----------------------------------------------------------------------------

get_season_sac_bunts <- function(season) {
  
  # Pull game IDs --------------------------------------------------------------
  
  games <- map_dfr(
    season_dates[[as.character(season)]],
    ~ mlb_game_pks(
      date = .x,
      level_ids = c(1)
    )
  )
  
  # Clean game IDs -------------------------------------------------------------
  
  game_ids <- games |>
    filter(!is.na(game_pk)) |>
    distinct(game_pk) |>
    pull(game_pk)
  
  # Pull sacrifice bunts -------------------------------------------------------
  
  sac_bunts <- map_dfr(
    game_ids,
    pull_sac_bunts
  ) |>
    mutate(
      season = season
    )
  
  # Save raw season bunt data --------------------------------------------------
  
  write_rds(
    sac_bunts,
    paste0(
      "sacrifice-bunt-analysis/data/sac_bunts_",
      season,
      ".rds"
    )
  )
  
  return(sac_bunts)
}

# -----------------------------------------------------------------------------
# Pull all seasons ====
# -----------------------------------------------------------------------------

sac_bunts_2023 <- get_season_sac_bunts(2023)
sac_bunts_2024 <- get_season_sac_bunts(2024)
sac_bunts_2025 <- get_season_sac_bunts(2025)
sac_bunts_2026 <- get_season_sac_bunts(2026)

# -----------------------------------------------------------------------------
# Combine all seasons ====
# -----------------------------------------------------------------------------

sac_bunts_all <- bind_rows(
  sac_bunts_2023,
  sac_bunts_2024,
  sac_bunts_2025,
  sac_bunts_2026
)

# -----------------------------------------------------------------------------
# Save combined dataset ====
# -----------------------------------------------------------------------------

write_rds(
  sac_bunts_all,
  "sacrifice-bunt-analysis/data/sac_bunts_all.rds"
)

# -----------------------------------------------------------------------------
# Build inning IDs ====
# -----------------------------------------------------------------------------

sac_bunts_all <- sac_bunts_all |>
  mutate(
    inning_id = paste(
      game_pk,
      about.inning,
      about.halfInning
    )
  )

# -----------------------------------------------------------------------------
# Calculate inning-ending scores ====
# -----------------------------------------------------------------------------

inning_final_scores <- sac_bunts_all |>
  group_by(inning_id) |>
  summarise(
    final_away_score = max(result.awayScore, na.rm = TRUE),
    final_home_score = max(result.homeScore, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Join inning-ending scores ====
# -----------------------------------------------------------------------------

sac_bunts_all <- sac_bunts_all |>
  left_join(
    inning_final_scores,
    by = "inning_id"
  )

# -----------------------------------------------------------------------------
# Runs scored after bunt ====
# -----------------------------------------------------------------------------

sac_bunts_all <- sac_bunts_all |>
  mutate(
    
    batting_team_runs_after_bunt = case_when(
      
      about.halfInning == "top" ~
        final_away_score - result.awayScore,
      
      about.halfInning == "bottom" ~
        final_home_score - result.homeScore
    ),
    
    successful_bunt = batting_team_runs_after_bunt >= 1
  )

# -----------------------------------------------------------------------------
# Create analysis variables ====
# -----------------------------------------------------------------------------

sac_bunts_all <- sac_bunts_all |>
  mutate(
    
    inning_bucket = if_else(
      about.inning >= 7,
      "Late (7+)",
      "Early/Mid"
    ),
    
    outs_bucket = paste0(
      count.outs.start,
      " Outs"
    ),
    
    score_diff = case_when(
      
      about.halfInning == "top" ~
        result.awayScore - result.homeScore,
      
      about.halfInning == "bottom" ~
        result.homeScore - result.awayScore
    ),
    
    score_bucket = case_when(
      score_diff == 0 ~ "Tied",
      abs(score_diff) == 1 ~ "1 Run Game",
      TRUE ~ "2+ Run Game"
    )
  )

# -----------------------------------------------------------------------------
# Example summaries ====
# -----------------------------------------------------------------------------

# Overall success rate --------------------------------------------------------

sac_bunts_all |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

# Success by outs -------------------------------------------------------------

sac_bunts_all |>
  group_by(outs_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Success by inning bucket ----------------------------------------------------

sac_bunts_all |>
  group_by(inning_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Success by score situation --------------------------------------------------

sac_bunts_all |>
  group_by(score_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Combined situational analysis -----------------------------------------------

sac_bunts_all |>
  group_by(
    inning_bucket,
    outs_bucket,
    score_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(success_rate))






# =============================================================================
# Sacrifice Bunt Historical Data Pull ####
# =============================================================================

library(tidyverse)
library(baseballr)
library(furrr)
library(progressr)

# -----------------------------------------------------------------------------
# Parallel processing setup
# -----------------------------------------------------------------------------

plan(multisession, workers = 4)

handlers(global = TRUE)

# -----------------------------------------------------------------------------
# Create folders
# -----------------------------------------------------------------------------

dir.create(
  "sacrifice-bunt-analysis/data",
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------------------------------------------------------
# Safe PBP pull
# -----------------------------------------------------------------------------

safe_pull <- function(game_id) {
  
  tryCatch(
    
    mlb_pbp(game_id),
    
    error = function(e) {
      message(paste("FAILED:", game_id))
      return(NULL)
    }
  )
}

# -----------------------------------------------------------------------------
# Pull ONLY innings containing sacrifice bunts
# -----------------------------------------------------------------------------

pull_bunt_innings <- function(game_id) {
  
  message(paste("Pulling game:", game_id))
  
  pbp <- safe_pull(game_id)
  
  if (is.null(pbp)) {
    return(NULL)
  }
  
  # Keep only completed AB events ---------------------------------------------
  
  pbp_clean <- pbp |>
    filter(last.pitch.of.ab == "true")
  
  # Identify innings containing sac bunts -------------------------------------
  
  bunt_innings <- pbp_clean |>
    filter(result.eventType == "sac_bunt") |>
    transmute(
      inning_id = paste(
        game_pk,
        about.inning,
        about.halfInning
      )
    ) |>
    distinct()
  
  # If no bunt innings exist, return NULL -------------------------------------
  
  if (nrow(bunt_innings) == 0) {
    return(NULL)
  }
  
  # Keep ALL plays from bunt innings ------------------------------------------
  
  pbp_clean |>
    mutate(
      inning_id = paste(
        game_pk,
        about.inning,
        about.halfInning
      )
    ) |>
    semi_join(
      bunt_innings,
      by = "inning_id"
    )
}

# -----------------------------------------------------------------------------
# Pull one month of data
# -----------------------------------------------------------------------------

get_month_bunt_data <- function(start_date, end_date, label) {
  
  # Pull games ----------------------------------------------------------------
  
  games <- map_dfr(
    seq(
      as.Date(start_date),
      as.Date(end_date),
      by = "day"
    ),
    
    ~ mlb_game_pks(
      date = .x,
      level_ids = c(1)
    )
  )
  
  # Clean game IDs ------------------------------------------------------------
  
  game_ids <- games |>
    filter(!is.na(game_pk)) |>
    distinct(game_pk) |>
    pull(game_pk)
  
  # Pull bunt innings with progress bar ---------------------------------------
  
  with_progress({
    
    month_data <- future_map_dfr(
      game_ids,
      pull_bunt_innings
    )
  })
  
  # Save monthly chunk --------------------------------------------------------
  
  write_rds(
    month_data,
    paste0(
      "sacrifice-bunt-analysis/data/",
      label,
      ".rds"
    )
  )
  
  return(month_data)
}

# =============================================================================
# EXAMPLE MONTH PULL
# =============================================================================

bunt_data_2023_04 <- get_month_bunt_data(
  start_date = "2023-04-01",
  end_date = "2023-04-30",
  label = "bunt_data_2023_04"
)

# =============================================================================
# COMBINE MONTHS LATER
# =============================================================================

# Example:
#
# bunt_data_all <- bind_rows(
#   bunt_data_2023_04,
#   bunt_data_2023_05,
#   bunt_data_2023_06
# )

# =============================================================================
# SAC BUNT ANALYSIS PIPELINE
# =============================================================================

# Keep only sac bunt events ---------------------------------------------------

sac_bunts <- bunt_data_2023_04 |>
  filter(result.eventType == "sac_bunt")

# Calculate inning-ending scores ----------------------------------------------

inning_final_scores <- bunt_data_2023_04 |>
  group_by(inning_id) |>
  summarise(
    final_away_score = max(result.awayScore, na.rm = TRUE),
    final_home_score = max(result.homeScore, na.rm = TRUE),
    .groups = "drop"
  )

# Join inning-ending scores ---------------------------------------------------

sac_bunts <- sac_bunts |>
  left_join(
    inning_final_scores,
    by = "inning_id"
  )

# Runs scored after bunt ------------------------------------------------------

sac_bunts <- sac_bunts |>
  mutate(
    
    batting_team_runs_after_bunt = case_when(
      
      about.halfInning == "top" ~
        final_away_score - result.awayScore,
      
      about.halfInning == "bottom" ~
        final_home_score - result.homeScore
    ),
    
    successful_bunt = batting_team_runs_after_bunt >= 1
  )

# Analysis variables ----------------------------------------------------------

sac_bunts <- sac_bunts |>
  mutate(
    
    inning_bucket = if_else(
      about.inning >= 7,
      "Late (7+)",
      "Early/Mid"
    ),
    
    outs_bucket = paste0(
      count.outs.start,
      " Outs"
    ),
    
    score_diff = case_when(
      
      about.halfInning == "top" ~
        result.awayScore - result.homeScore,
      
      about.halfInning == "bottom" ~
        result.homeScore - result.awayScore
    ),
    
    score_bucket = case_when(
      score_diff == 0 ~ "Tied",
      abs(score_diff) == 1 ~ "1 Run Game",
      TRUE ~ "2+ Run Game"
    )
  )

# =============================================================================
# EXAMPLE SUMMARIES
# =============================================================================

# Overall success rate --------------------------------------------------------

sac_bunts |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

# Outs before bunt ------------------------------------------------------------

sac_bunts |>
  group_by(outs_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Inning bucket ---------------------------------------------------------------

sac_bunts |>
  group_by(inning_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Score situation -------------------------------------------------------------

sac_bunts |>
  group_by(score_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Combined situational analysis -----------------------------------------------

sac_bunts |>
  group_by(
    inning_bucket,
    outs_bucket,
    score_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(success_rate))






# =============================================================================
# Historical MLB Sacrifice Bunt Data Pull (2023–2026)
# =============================================================================

library(tidyverse)
library(baseballr)
library(furrr)
library(progressr)

# -----------------------------------------------------------------------------
# Parallel processing setup
# -----------------------------------------------------------------------------

plan(multisession, workers = 3)

handlers(global = TRUE)

# -----------------------------------------------------------------------------
# Create folders
# -----------------------------------------------------------------------------

dir.create(
  "sacrifice-bunt-analysis/data",
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------------------------------------------------------
# Safe PBP pull
# -----------------------------------------------------------------------------

safe_pull <- function(game_id) {
  
  tryCatch(
    
    mlb_pbp(game_id),
    
    error = function(e) {
      message(paste("FAILED GAME:", game_id))
      return(NULL)
    }
  )
}

# -----------------------------------------------------------------------------
# Pull all completed AB events from innings containing sac bunts
# -----------------------------------------------------------------------------

pull_bunt_innings <- function(game_id) {
  
  message(paste("Pulling game:", game_id))
  
  pbp <- safe_pull(game_id)
  
  if (is.null(pbp)) {
    return(NULL)
  }
  
  # Keep completed AB events only ---------------------------------------------
  
  pbp_clean <- pbp |>
    filter(last.pitch.of.ab == "true")
  
  # Identify innings containing sac bunts -------------------------------------
  
  bunt_innings <- pbp_clean |>
    filter(result.eventType == "sac_bunt") |>
    transmute(
      inning_id = paste(
        game_pk,
        about.inning,
        about.halfInning
      )
    ) |>
    distinct()
  
  # Skip games with no sac bunts ----------------------------------------------
  
  if (nrow(bunt_innings) == 0) {
    return(NULL)
  }
  
  # Keep ALL plays from bunt innings ------------------------------------------
  
  pbp_clean |>
    mutate(
      inning_id = paste(
        game_pk,
        about.inning,
        about.halfInning
      )
    ) |>
    semi_join(
      bunt_innings,
      by = "inning_id"
    )
}

# -----------------------------------------------------------------------------
# Pull one month of data
# -----------------------------------------------------------------------------

get_month_bunt_data <- function(start_date, end_date, label) {
  
  message("===================================================")
  message(paste("STARTING:", label))
  message("===================================================")
  
  start_time <- Sys.time()
  
  # Pull game schedule --------------------------------------------------------
  
  games <- map_dfr(
    seq(
      as.Date(start_date),
      as.Date(end_date),
      by = "day"
    ),
    
    ~ mlb_game_pks(
      date = .x,
      level_ids = c(1)
    )
  )
  
  # Clean game IDs ------------------------------------------------------------
  
  game_ids <- games |>
    filter(!is.na(game_pk)) |>
    distinct(game_pk) |>
    pull(game_pk)
  
  message(paste("Games found:", length(game_ids)))
  
  # Pull data -----------------------------------------------------------------
  
  with_progress({
    
    month_data <- future_map_dfr(
      game_ids,
      pull_bunt_innings,
      .options = furrr_options(seed = TRUE)
    )
  })
  
  # Save monthly chunk --------------------------------------------------------
  
  save_path <- paste0(
    "sacrifice-bunt-analysis/data/",
    label,
    ".rds"
  )
  
  write_rds(
    month_data,
    save_path
  )
  
  message(paste("SAVED:", save_path))
  
  message(
    paste(
      "Minutes elapsed:",
      round(
        difftime(
          Sys.time(),
          start_time,
          units = "mins"
        ),
        2
      )
    )
  )
  
  return(month_data)
}

# =============================================================================
# Monthly pull schedule
# =============================================================================

months <- tribble(
  ~start_date, ~end_date, ~label,
  
  "2023-04-01", "2023-04-30", "bunt_data_2023_04",
  "2023-05-01", "2023-05-31", "bunt_data_2023_05",
  "2023-06-01", "2023-06-30", "bunt_data_2023_06",
  "2023-07-01", "2023-07-31", "bunt_data_2023_07",
  "2023-08-01", "2023-08-31", "bunt_data_2023_08",
  "2023-09-01", "2023-09-30", "bunt_data_2023_09",

  "2024-04-01", "2024-04-30", "bunt_data_2024_04",
  "2024-05-01", "2024-05-31", "bunt_data_2024_05",
  "2024-06-01", "2024-06-30", "bunt_data_2024_06",
  "2024-07-01", "2024-07-31", "bunt_data_2024_07",
  "2024-08-01", "2024-08-31", "bunt_data_2024_08",
  "2024-09-01", "2024-09-30", "bunt_data_2024_09",

  "2025-04-01", "2025-04-30", "bunt_data_2025_04",
  "2025-05-01", "2025-05-31", "bunt_data_2025_05",
  "2025-06-01", "2025-06-30", "bunt_data_2025_06",
  "2025-07-01", "2025-07-31", "bunt_data_2025_07",
  "2025-08-01", "2025-08-31", "bunt_data_2025_08",
  "2025-09-01", "2025-09-30", "bunt_data_2025_09",

  "2026-04-01", "2026-04-30", "bunt_data_2026_04",
  "2026-05-01", "2026-05-31", "bunt_data_2026_05"
)

# =============================================================================
# Run all monthly pulls automatically
# =============================================================================

for(i in 1:nrow(months)) {
  
  current_start <- months$start_date[i]
  current_end <- months$end_date[i]
  current_label <- months$label[i]
  
  tryCatch(
    
    {
      
      get_month_bunt_data(
        start_date = current_start,
        end_date = current_end,
        label = current_label
      )
      
      message(paste("COMPLETED:", current_label))
    },
    
    error = function(e) {
      
      message(paste("FAILED MONTH:", current_label))
      message(e)
    }
  )
}

# =============================================================================
# Combine all monthly files
# =============================================================================

all_files <- list.files(
  "sacrifice-bunt-analysis/data",
  full.names = TRUE
)

bunt_data_all <- map_dfr(
  all_files,
  read_rds
)

# =============================================================================
# Save final combined dataset
# =============================================================================

write_rds(
  bunt_data_all,
  "sacrifice-bunt-analysis/data/bunt_data_all.rds"
)

message("ALL DATA PULLS COMPLETE")











