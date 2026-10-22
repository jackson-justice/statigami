library(baseballr)
library(tidyverse)
library(gganimate)

game <- mlb_game_pks(date = "2026-04-03", level_ids = c(11))

game |>
  select(game_pk,
         officialDate,
         teams.away.team.name,
         teams.home.team.name,
         venue.name)

bees_game <- game |>
  filter(str_detect(teams.home.team.name, "Salt Lake"))

bees_game

game_pk <- bees_game$game_pk[1]

pbp <- mlb_pbp(game_pk)

names(pbp)

events <- pbp |>
  filter(!is.na(atBatIndex)) |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(
    about.inning,
    about.halfInning,
    atBatIndex,
    result.event,
    result.rbi,
    result.isOut,
    details.isScoringPlay
  )

nrow(events)
events |>
  count(result.event, sort = TRUE)

events_scored <- events |>
  mutate(
    runs = result.rbi,
    hit = result.event %in% c("Single", "Double", "Triple", "Home Run"),
    xbh = result.event %in% c("Double", "Triple", "Home Run"),
    hr = result.event == "Home Run",
    walk = result.event %in% c("Walk", "Intent Walk"),
    hbp = result.event == "Hit By Pitch",
    reached_error = result.event == "Field Error",
    sac_fly = result.event == "Sac Fly",
    strikeout = result.event == "Strikeout",
    double_play = result.event == "Grounded Into DP",
    pitching_change = FALSE,
    mound_visit = FALSE,
    event_score =
      4 * runs +
      1 * hit +
      2 * xbh +
      3 * hr +
      0.5 * (walk + hbp) +
      1 * reached_error +
      1 * sac_fly -
      0.5 * strikeout -
      2 * double_play -
      2 * pitching_change -
      1 * mound_visit
  )

events_scored |>
  select(about.inning, about.halfInning, result.event, runs, event_score)

inning_scores <- events_scored |>
  group_by(about.inning) |>
  summarise(
    excitement_score = sum(event_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(excitement_score))

inning_scores

inning_breakdown <- events_scored |>
  group_by(about.inning) |>
  summarise(
    runs = sum(runs, na.rm = TRUE),
    hits = sum(hit, na.rm = TRUE),
    xbh = sum(xbh, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    walks_hbp = sum(walk + hbp, na.rm = TRUE),
    errors = sum(reached_error, na.rm = TRUE),
    sac_flies = sum(sac_fly, na.rm = TRUE),
    strikeouts = sum(strikeout, na.rm = TRUE),
    double_plays = sum(double_play, na.rm = TRUE),
    excitement_score = sum(event_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(excitement_score))

inning_breakdown

inning_half_scores <- events_scored |>
  group_by(about.inning, about.halfInning) |>
  summarise(
    runs = sum(runs, na.rm = TRUE),
    hits = sum(hit, na.rm = TRUE),
    xbh = sum(xbh, na.rm = TRUE),
    hr = sum(hr, na.rm = TRUE),
    walks_hbp = sum(walk + hbp, na.rm = TRUE),
    errors = sum(reached_error, na.rm = TRUE),
    sac_flies = sum(sac_fly, na.rm = TRUE),
    strikeouts = sum(strikeout, na.rm = TRUE),
    double_plays = sum(double_play, na.rm = TRUE),
    excitement_score = sum(event_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(excitement_score))

view(inning_half_scores)


get_inning_scores <- function(game_pk) {
  
  pbp <- mlb_pbp(game_pk)
  
  events <- pbp |>
    filter(!is.na(atBatIndex)) |>
    group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
    slice_tail(n = 1) |>
    ungroup()
  
  events_scored <- events |>
    mutate(
      runs = result.rbi,
      hit = result.event %in% c("Single", "Double", "Triple", "Home Run"),
      xbh = result.event %in% c("Double", "Triple", "Home Run"),
      hr = result.event == "Home Run",
      walk = result.event %in% c("Walk", "Intent Walk"),
      hbp = result.event == "Hit By Pitch",
      reached_error = result.event == "Field Error",
      sac_fly = result.event == "Sac Fly",
      strikeout = result.event == "Strikeout",
      double_play = result.event == "Grounded Into DP",
      pitching_change = FALSE,
      mound_visit = FALSE,
      event_score =
        4 * runs +
        1 * hit +
        2 * xbh +
        3 * hr +
        0.5 * (walk + hbp) +
        1 * reached_error +
        1 * sac_fly -
        0.5 * strikeout -
        2 * double_play -
        2 * pitching_change -
        1 * mound_visit
    )
  
  events_scored |>
    group_by(about.inning, about.halfInning) |>
    summarise(
      excitement_score = sum(event_score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(excitement_score))
}

get_inning_scores(game_pk)


##################################################################

get_bees_home_games <- function(start_date, end_date) {
  all_dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  
  game_list <- map(all_dates, \(d) {
    out <- try(suppressMessages(
      mlb_game_pks(date = as.character(d), level_ids = c(11))
    ), silent = TRUE)
    
    if (inherits(out, "try-error") || is.null(out)) {
      return(NULL)
    }
    
    out
  })
  
  bind_rows(game_list) |>
    filter(str_detect(teams.home.team.name, regex("Salt Lake", ignore_case = TRUE))) |>
    distinct(game_pk, .keep_all = TRUE)
}

bees_home_games <- bind_rows(
  get_bees_home_games("2025-03-01", "2025-09-30"),
  get_bees_home_games("2026-03-01", Sys.Date())
) |>
  distinct(game_pk, .keep_all = TRUE)

nrow(bees_home_games)

bees_home_games |>
  select(
    officialDate,
    game_pk,
    teams.away.team.name,
    teams.home.team.name
  ) |>
  arrange(officialDate)

all_game_scores <- bees_home_games |>
  select(game_pk, officialDate) |>
  mutate(
    scores = map(game_pk, get_inning_scores)
  ) |>
  unnest(scores)

all_game_scores

historical_scores <- all_game_scores |>
  group_by(about.inning, about.halfInning) |>
  summarise(
    avg_excitement = mean(excitement_score, na.rm = TRUE),
    games = n(),
    .groups = "drop"
  ) |>
  arrange(avg_excitement)

historical_scores

historical_scores_plot <- historical_scores |>
  mutate(
    inning_half = paste0(
      ifelse(about.halfInning == "top", "Top ", "Bot "),
      about.inning
    ),
    snack_zone = case_when(
      avg_excitement <= quantile(avg_excitement, 0.33) ~ "Safe",
      avg_excitement >= quantile(avg_excitement, 0.67) ~ "Do Not Leave",
      TRUE ~ "Risky"
    )
  )

historical_plot <- ggplot(historical_scores_plot, aes(x = reorder(inning_half, avg_excitement),
                                   y = avg_excitement,
                                   fill = snack_zone)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Do Not Leave" = "red",
      "Risky" = "gold",
      "Safe" = "green4"
    )
  ) +
  labs(
    title = "Best and Worst Times to Leave for Snacks",
    x = NULL,
    y = "Average Excitement Score",
    fill = NULL
  ) +
  theme_minimal()

ggsave(
  filename = "game_2_plot_historical.png",
  plot = historical_plot,
  width = 8,
  height = 6,
  dpi = 300
)


##############################################################################
# Game 2

game_0417 <- mlb_game_pks(date = "2026-04-17", level_ids = c(11))

bees_game_0417 <- game_0417 |>
  filter(str_detect(teams.home.team.name, regex("Salt Lake", ignore_case = TRUE)))

bees_game_0417 |>
  select(officialDate, game_pk, teams.away.team.name, teams.home.team.name)

game_pk_0417 <- bees_game_0417$game_pk[1]

get_inning_scores(game_pk_0417)


pbp_0417 <- mlb_pbp(game_pk_0417)

events_0417 <- pbp_0417 |>
  filter(!is.na(atBatIndex)) |>
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) |>
  slice_tail(n = 1) |>
  ungroup()

events_0417 |>
  filter(about.inning == 1, about.halfInning == "top") |>
  select(result.event, result.rbi)


events_0417 |>
  filter(about.inning == 4, about.halfInning == "top") |>
  select(result.event, result.rbi)

game_scores_plot <- get_inning_scores(game_pk_0417) |>
  mutate(
    inning_half = paste0(
      ifelse(about.halfInning == "top", "Top ", "Bot "),
      about.inning
    ),
    snack_zone = case_when(
      excitement_score <= quantile(excitement_score, 0.33) ~ "Safe",
      excitement_score >= quantile(excitement_score, 0.67) ~ "Do Not Leave",
      TRUE ~ "Risky"
    )
  )

game_plot <- ggplot(game_scores_plot,
                    aes(x = reorder(inning_half, excitement_score),
                        y = excitement_score,
                        fill = snack_zone)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Do Not Leave" = "red",
      "Risky" = "gold",
      "Safe" = "green4"
    )
  ) +
  labs(
    title = "Best and Worst Times to Leave During This Game",
    x = NULL,
    y = "Excitement Score",
    fill = NULL
  ) +
  theme_minimal()

ggsave(
  filename = "game_2_plot.png",
  plot = game_plot,
  width = 8,
  height = 6,
  dpi = 300
)


historical_anim <- historical_scores_plot |>
  transmute(
    inning_half,
    score = avg_excitement,
    snack_zone,
    chart = "Historical Average"
  )

game_anim <- game_scores_plot |>
  transmute(
    inning_half,
    score = excitement_score,
    snack_zone,
    chart = "April 17 Game"
  )

anim_data <- bind_rows(historical_anim, game_anim)

p <- ggplot(anim_data,
            aes(x = reorder(inning_half, score),
                y = score,
                fill = snack_zone)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Do Not Leave" = "red",
      "Risky" = "gold",
      "Safe" = "green4"
    )
  ) +
  labs(
    title = "{closest_state}",
    x = NULL,
    y = "Excitement Score"
  ) +
  theme_minimal() +
  transition_states(chart, transition_length = 2, state_length = 2) +
  ease_aes("cubic-in-out")

animate(p, width = 800, height = 600, fps = 20, duration = 6)

anim <- animate(
  p,
  width = 1080,
  height = 1920,
  fps = 30,
  duration = 4,
  renderer = gifski_renderer("snack_model.gif")
)

anim
