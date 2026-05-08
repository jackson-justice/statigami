# Project: Sacrifice Bunt Run Expectancy
# Date: 2026-05-06

library(tidyverse)
library(baseballr)

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


# Games since 2023 ############################################################
season_dates <- list(
  "2023" = seq(
    as.Date("2023-03-30"),
    as.Date("2023-10-01"),
    by = "day"
  ),
  
  "2024" = seq(
    as.Date("2024-03-30"),
    as.Date("2024-10-01"),
    by = "day"
  ),
  
  "2025" = seq(
    as.Date("2025-03-30"),
    as.Date("2025-10-01"),
    by = "day"
  ),
  
  "2026" = seq(
    as.Date("2026-03-30"),
    as.Date("2026-05-07"),
    by = "day"
  )
)

games_2023 <- map_dfr(
  season_dates[["2023"]],
  ~ mlb_game_pks(
    date = .x,
    level_ids = c(1)
  )
)

dir.create(
  "sacrifice-bunt-analysis/data",
  recursive = TRUE,
  showWarnings = FALSE
)

write_rds(
  games_2023,
  "sacrifice-bunt-analysis/data/games_2023.rds"
)

game_id_2023 <- games_2023 |> 
  filter(!is.na(game_pk)) |> 
  distinct(game_pk) |> 
  pull(game_pk)

safe_pull <- function(game_id) {
  
  tryCatch(
    mlb_pbp(game_id),
    error = function(e) {
      message(paste("FAILED:", game_id))
      return(NULL)
    }
  )
}

pull_and_filter_sac_bunts <- function(game_id) {
  
  pbp <- safe_pull(game_id)
  
  if (is.null(pbp)) return(NULL)
  
  pbp |>
    filter(
      last.pitch.of.ab == "true",
      result.eventType == "sac_bunt"
    )
  
}

sac_bunts_2023 <- map_dfr(
  game_id_2023,
  pull_and_filter_sac_bunts
)

write_rds(
  pbp_2023,
  "sacrifice-bunt-analysis/data/pbp_2023.rds"
)




























