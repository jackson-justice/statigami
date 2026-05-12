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



# =============================================================================
# FINAL DATA PIPELINE #########################################################
# =============================================================================

# =============================================================================
# Historical MLB Play-by-Play Data Pull (2023–2026) ###########################
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
  "baseball-pbp-data/data",
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
# Pull all completed AB events from a game
# -----------------------------------------------------------------------------

pull_game_events <- function(game_id) {
  
  message(paste("Pulling game:", game_id))
  
  pbp <- safe_pull(game_id)
  
  if (is.null(pbp)) {
    return(NULL)
  }
  
  # Keep completed plate appearance events only -------------------------------
  
  pbp |>
    filter(last.pitch.of.ab == "true")
}

# -----------------------------------------------------------------------------
# Pull one month of data
# -----------------------------------------------------------------------------

get_month_pbp_data <- function(start_date, end_date, label) {
  
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
      pull_game_events,
      .options = furrr_options(seed = TRUE)
    )
  })
  
  # Save monthly chunk --------------------------------------------------------
  
  save_path <- paste0(
    "baseball-pbp-data/data/",
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
  
  "2023-04-01", "2023-04-30", "pbp_2023_04",
  "2023-05-01", "2023-05-31", "pbp_2023_05",
  "2023-06-01", "2023-06-30", "pbp_2023_06",
  "2023-07-01", "2023-07-31", "pbp_2023_07",
  "2023-08-01", "2023-08-31", "pbp_2023_08",
  "2023-09-01", "2023-09-30", "pbp_2023_09",
  
  "2024-04-01", "2024-04-30", "pbp_2024_04",
  "2024-05-01", "2024-05-31", "pbp_2024_05",
  "2024-06-01", "2024-06-30", "pbp_2024_06",
  "2024-07-01", "2024-07-31", "pbp_2024_07",
  "2024-08-01", "2024-08-31", "pbp_2024_08",
  "2024-09-01", "2024-09-30", "pbp_2024_09",
  
  "2025-04-01", "2025-04-30", "pbp_2025_04",
  "2025-05-01", "2025-05-31", "pbp_2025_05",
  "2025-06-01", "2025-06-30", "pbp_2025_06",
  "2025-07-01", "2025-07-31", "pbp_2025_07",
  "2025-08-01", "2025-08-31", "pbp_2025_08",
  "2025-09-01", "2025-09-30", "pbp_2025_09",
  
  "2026-04-01", "2026-04-30", "pbp_2026_04",
  "2026-05-01", "2026-05-31", "pbp_2026_05"
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
      
      get_month_pbp_data(
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
  "baseball-pbp-data/data",
  pattern = "\\.rds$",
  full.names = TRUE
)

pbp_all <- map_dfr(
  all_files,
  read_rds
)

# =============================================================================
# Save final combined dataset
# =============================================================================

write_rds(
  pbp_all,
  "baseball-pbp-data/data/pbp_all.rds"
)

message("ALL DATA PULLS COMPLETE")






