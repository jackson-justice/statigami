# Project: Sacrifice Bunt Analysis
# Goal: Identify the situations where sacrifice bunts are most effective

library(tidyverse)
library(baseballr)
library(furrr)
library(progressr)
library(scales)

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




# =============================================================================
# Sacrifice bunt dataset
# =============================================================================

sac_bunts_all <- pbp_all |>
  filter(result.eventType == "sac_bunt")

# =============================================================================
# Create inning ID
# =============================================================================

pbp_all <- pbp_all |>
  mutate(
    inning_id = paste(
      game_pk,
      about.inning,
      about.halfInning
    )
  )

sac_bunts_all <- sac_bunts_all |>
  mutate(
    inning_id = paste(
      game_pk,
      about.inning,
      about.halfInning
    )
  )

# =============================================================================
# Final inning scores
# =============================================================================

inning_final_scores <- pbp_all |>
  group_by(inning_id) |>
  summarise(
    
    final_away_score = max(
      result.awayScore,
      na.rm = TRUE
    ),
    
    final_home_score = max(
      result.homeScore,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# =============================================================================
# Join inning-ending scores
# =============================================================================

sac_bunts_all <- sac_bunts_all |>
  left_join(
    inning_final_scores,
    by = "inning_id"
  )

# =============================================================================
# Runs scored after bunt
# =============================================================================

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

# =============================================================================
# Situation variables
# =============================================================================

sac_bunts_all <- sac_bunts_all |>
  mutate(
    
    inning_bucket = case_when(
      about.inning <= 3 ~ "Early",
      about.inning <= 6 ~ "Middle",
      TRUE ~ "Late"
    ),
    
    late_game = about.inning >= 7,
    
    outs_bucket = case_when(
      count.outs.start == 0 ~ "0 Outs",
      count.outs.start == 1 ~ "1 Out"
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
    ),
    
    runner_on_first =
      !is.na(matchup.postOnFirst.fullName),
    
    runner_on_second =
      !is.na(matchup.postOnSecond.fullName),
    
    runner_on_third =
      !is.na(matchup.postOnThird.fullName)
  )

# =============================================================================
# Initial summaries
# =============================================================================

# Overall bunt success --------------------------------------------------------

sac_bunts_all |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n()
  )

# Outs before bunt ------------------------------------------------------------

sac_bunts_all |>
  group_by(outs_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(success_rate))

# Early vs late innings -------------------------------------------------------

sac_bunts_all |>
  group_by(inning_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Score situation -------------------------------------------------------------

sac_bunts_all |>
  group_by(score_bucket) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  )

# Combined situations ---------------------------------------------------------

sac_bunts_all |>
  group_by(
    outs_bucket,
    inning_bucket,
    score_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 20) |>
  arrange(desc(success_rate))


# =============================================================================
# Baserunner configurations
# =============================================================================

sac_bunts_all <- sac_bunts_all |>
  mutate(
    
    base_state = case_when(
      
      runner_on_first &
        !runner_on_second &
        !runner_on_third ~
        "1st Only",
      
      !runner_on_first &
        runner_on_second &
        !runner_on_third ~
        "2nd Only",
      
      !runner_on_first &
        !runner_on_second &
        runner_on_third ~
        "3rd Only",
      
      runner_on_first &
        runner_on_second &
        !runner_on_third ~
        "1st + 2nd",
      
      runner_on_first &
        !runner_on_second &
        runner_on_third ~
        "1st + 3rd",
      
      !runner_on_first &
        runner_on_second &
        runner_on_third ~
        "2nd + 3rd",
      
      runner_on_first &
        runner_on_second &
        runner_on_third ~
        "Bases Loaded",
      
      TRUE ~
        "Other"
    )
  )

# =============================================================================
# Overall bunt success by baserunner situation
# =============================================================================

sac_bunts_all |>
  group_by(base_state) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(success_rate))

# =============================================================================
# Baserunner situation + outs
# =============================================================================

sac_bunts_all |>
  group_by(
    base_state,
    outs_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 20) |>
  arrange(desc(success_rate))

# =============================================================================
# Baserunner situation + inning
# =============================================================================

sac_bunts_all |>
  group_by(
    base_state,
    inning_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 20) |>
  arrange(desc(success_rate))

# =============================================================================
# BEST bunt situations
# =============================================================================

best_bunt_situations <- sac_bunts_all |>
  group_by(
    base_state,
    outs_bucket,
    inning_bucket,
    score_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 15) |>
  arrange(desc(success_rate))

best_bunt_situations

# =============================================================================
# Most common bunt situations
# =============================================================================

sac_bunts_all |>
  count(
    base_state,
    outs_bucket,
    inning_bucket,
    score_bucket,
    sort = TRUE
  )


# =============================================================================
# BEST BUNT SITUATIONS DATA
# =============================================================================

best_bunt_situations <- sac_bunts_all |>
  group_by(
    base_state,
    outs_bucket,
    inning_bucket,
    score_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 15) |>
  arrange(desc(success_rate)) |>
  mutate(
    
    situation_label = paste(
      base_state,
      "|",
      outs_bucket,
      "|",
      inning_bucket,
      "|",
      score_bucket
    ),
    
    success_pct = success_rate * 100
  )

# =============================================================================
# HORIZONTAL BAR CHART
# =============================================================================

ggplot(
  best_bunt_situations,
  
  aes(
    x = success_pct,
    y = reorder(
      situation_label,
      success_pct
    ),
    fill = success_pct
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = paste0(
        round(success_pct, 1),
        "%  (n=",
        n,
        ")"
      )
    ),
    
    hjust = -0.1,
    size = 3
  ) +
  
  scale_x_continuous(
    limits = c(0, 85),
    labels = label_percent(scale = 1)
  ) +
  
  labs(
    title = "Most Successful MLB Sacrifice Bunt Situations Since 2023",
    
    subtitle = "Success = team scored later in the inning",
    
    x = "Success Rate",
    y = NULL,
    
    caption = "Source: MLB play-by-play data via baseballr"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text.y = element_text(size = 9)
  )


# =============================================================================
# Simplified bunt situations
# =============================================================================

simple_bunt_situations <- sac_bunts_all |>
  group_by(
    base_state,
    outs_bucket
  ) |>
  summarise(
    success_rate = mean(successful_bunt),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 20) |>
  mutate(
    
    success_pct = success_rate * 100,
    
    situation_label = paste(
      toupper(base_state),
      "•",
      toupper(outs_bucket)
    )
  ) |>
  arrange(desc(success_rate))

# =============================================================================
# Plot
# =============================================================================

ggplot(
  simple_bunt_situations,
  
  aes(
    x = success_pct,
    
    y = reorder(
      situation_label,
      success_pct
    ),
    
    fill = success_pct
  )
) +
  
  geom_col(
    width = 0.72
  ) +
  
  # Success percentage --------------------------------------------------------

geom_text(
  aes(
    label = paste0(
      round(success_pct, 1),
      "%"
    )
  ),
  
  hjust = -0.15,
  size = 5,
  fontface = "bold"
) +
  
  # Sample size ---------------------------------------------------------------

geom_text(
  aes(
    label = paste0(
      "n = ",
      n
    )
  ),
  
  hjust = 1.15,
  color = "white",
  size = 3.2,
  fontface = "bold"
) +
  
  scale_x_continuous(
    limits = c(0, 80),
    labels = label_percent(scale = 1)
  ) +
  
  scale_fill_gradient(
    low = "#355CFF",
    high = "#FFC857"
  ) +
  
  labs(
    title = "WHEN MLB TEAMS STILL BUNT",
    
    subtitle = paste(
      "Modern MLB sacrifice bunt success rates since 2023",
      "\nSuccess = team scored later in the inning"
    ),
    
    x = NULL,
    y = NULL,
    
    caption = "Source: MLB play-by-play data via baseballr"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    legend.position = "none",
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 24
    ),
    
    plot.subtitle = element_text(
      size = 12,
      color = "gray40"
    ),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 10
    )
  )

