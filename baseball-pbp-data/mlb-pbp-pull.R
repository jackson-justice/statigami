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

