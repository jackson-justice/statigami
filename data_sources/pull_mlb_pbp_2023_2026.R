# =============================================================================
# Pull MLB Play-by-Play Data: 2023-2026
# =============================================================================
#
# File:
#   data_sources/pull_mlb_pbp_2023_2026.R
#
# Purpose:
#   Pull MLB play-by-play data from baseballr and save monthly raw .rds files.
#
# Notes:
#   - Raw data is saved locally in data/raw/mlb_pbp/.
#   - Existing monthly files are skipped unless force = TRUE.
#   - data/raw/ should be ignored by Git.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(tidyverse)
library(baseballr)
library(furrr)
library(future)
library(progressr)

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

output_dir <- "data/raw/mlb_pbp"

workers <- 3
force <- FALSE

start_year <- 2023
end_year <- 2026
season_months <- 4:9

max_pull_date <- Sys.Date()

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

plan(multisession, workers = workers)
handlers(global = TRUE)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------

safe_pull_pbp <- function(game_id) {
  tryCatch(
    baseballr::mlb_pbp(game_id),
    error = function(e) {
      message("FAILED GAME: ", game_id)
      message("Reason: ", conditionMessage(e))
      return(NULL)
    }
  )
}

pull_game_events <- function(game_id) {
  message("Pulling game: ", game_id)
  
  pbp <- safe_pull_pbp(game_id)
  
  if (is.null(pbp) || nrow(pbp) == 0) {
    return(NULL)
  }
  
  pbp |>
    filter(last.pitch.of.ab == "true") |>
    mutate(game_pk = game_id)
}

get_game_ids_for_dates <- function(start_date, end_date) {
  dates <- seq(
    as.Date(start_date),
    as.Date(end_date),
    by = "day"
  )
  
  games <- map_dfr(
    dates,
    ~ baseballr::mlb_game_pks(
      date = .x,
      level_ids = c(1)
    )
  )
  
  games |>
    filter(!is.na(game_pk)) |>
    distinct(game_pk) |>
    pull(game_pk)
}

pull_month_pbp <- function(start_date, end_date, label, output_dir, force = FALSE) {
  save_path <- file.path(output_dir, paste0(label, ".rds"))
  
  if (file.exists(save_path) && !force) {
    message("SKIPPING existing file: ", save_path)
    return(readr::read_rds(save_path))
  }
  
  message("===================================================")
  message("STARTING: ", label)
  message("Date range: ", start_date, " to ", end_date)
  message("===================================================")
  
  start_time <- Sys.time()
  
  game_ids <- get_game_ids_for_dates(start_date, end_date)
  
  message("Games found: ", length(game_ids))
  
  if (length(game_ids) == 0) {
    message("No games found for ", label)
    return(NULL)
  }
  
  with_progress({
    month_data <- furrr::future_map_dfr(
      game_ids,
      pull_game_events,
      .options = furrr_options(seed = TRUE)
    )
  })
  
  if (nrow(month_data) == 0) {
    message("No rows returned for ", label)
    return(NULL)
  }
  
  readr::write_rds(month_data, save_path)
  
  message("SAVED: ", save_path)
  
  message(
    "Minutes elapsed: ",
    round(difftime(Sys.time(), start_time, units = "mins"), 2)
  )
  
  month_data
}

make_month_schedule <- function(start_year, end_year, season_months, max_pull_date = Sys.Date()) {
  expand_grid(
    year = start_year:end_year,
    month = season_months
  ) |>
    mutate(
      start_date = as.Date(sprintf("%s-%02d-01", year, month)),
      end_date = ceiling_date(start_date, unit = "month") - days(1),
      end_date = pmin(end_date, max_pull_date),
      label = sprintf("pbp_%s_%02d", year, month)
    ) |>
    filter(start_date <= max_pull_date) |>
    select(start_date, end_date, label)
}

combine_monthly_pbp <- function(output_dir, combined_name = "pbp_all.rds") {
  all_files <- list.files(
    output_dir,
    pattern = "^pbp_[0-9]{4}_[0-9]{2}\\.rds$",
    full.names = TRUE
  )
  
  if (length(all_files) == 0) {
    stop("No monthly PBP files found in: ", output_dir)
  }
  
  pbp_all <- map_dfr(all_files, readr::read_rds)
  
  combined_path <- file.path(output_dir, combined_name)
  
  readr::write_rds(pbp_all, combined_path)
  
  message("COMBINED DATA SAVED: ", combined_path)
  message("Rows: ", nrow(pbp_all))
  
  pbp_all
}

# -----------------------------------------------------------------------------
# Build monthly schedule
# -----------------------------------------------------------------------------

months <- make_month_schedule(
  start_year = start_year,
  end_year = end_year,
  season_months = season_months,
  max_pull_date = max_pull_date
)

print(months)

# -----------------------------------------------------------------------------
# Pull monthly files
# -----------------------------------------------------------------------------

for (i in seq_len(nrow(months))) {
  current_start <- months$start_date[i]
  current_end <- months$end_date[i]
  current_label <- months$label[i]
  
  tryCatch(
    {
      pull_month_pbp(
        start_date = current_start,
        end_date = current_end,
        label = current_label,
        output_dir = output_dir,
        force = force
      )
      
      message("COMPLETED: ", current_label)
    },
    error = function(e) {
      message("FAILED MONTH: ", current_label)
      message("Reason: ", conditionMessage(e))
    }
  )
}

# -----------------------------------------------------------------------------
# Combine monthly files
# -----------------------------------------------------------------------------

pbp_all <- combine_monthly_pbp(output_dir)

message("ALL DATA PULLS COMPLETE")

