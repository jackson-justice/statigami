# =============================================================================
# NFL Graveyard: the weekly tracker on the website
#
# Sourced at the end of weekly_update.R (it needs that script's objects). For
# every completed week of the current season it writes the generated parts of
# website/articles/nfl-graveyard.qmd:
#
#   website/articles/_nfl-graveyard/tracker.qmd
#       The week selector, then one hidden section per week: the three tier
#       boxes (team logos by tier), what moved, the Graveyard and the two
#       floors, the hand-written story for the week, what is at stake next
#       week, and the smoothed title and playoff odds by record.
#   website/articles/_nfl-graveyard/meta.yml
#       The article's date and facts strip (merged in via metadata-files).
#   website/articles/_nfl-graveyard/stories/week<NN>.qmd
#       One hand-written file per week. Created with a placeholder when
#       missing and never overwritten.
#   nfl-championship-trajectories/logos/<TEAM>.png
#       Team logos from nflverse, downloaded once. The site copies them next
#       to the figures.
#
# Everything here is on the full baseline (1966 on), like the tiers. Rerun
# weekly_update.R after editing a story file to rebuild the page.
# =============================================================================

library(tidyverse)

site_dir <- file.path("website", "articles", "_nfl-graveyard")
stories_dir <- file.path(site_dir, "stories")
logos_dir <- file.path(project_dir, "logos")
dir.create(stories_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logos_dir, showWarnings = FALSE)
logo_web_dir <- sprintf("figures/%s/logos", project_dir)


# =============================================================================
# 1. Team names and logos
# =============================================================================

teams_url <- "https://github.com/nflverse/nflverse-pbp/raw/master/teams_colors_logos.csv"
teams_path <- file.path(raw_dir, "teams_colors_logos.csv")
if (!file.exists(teams_path)) {
  download.file(teams_url, teams_path, mode = "wb", quiet = TRUE)
}

teams <- read_csv(teams_path, show_col_types = FALSE) |>
  filter(team_abbr %in% season_teams$team) |>
  transmute(team = team_abbr, team_name, nick = team_nick,
            logo_url = team_logo_espn)
stopifnot(nrow(teams) == nrow(season_teams))

for (i in seq_len(nrow(teams))) {
  logo_path <- file.path(logos_dir, paste0(teams$team[i], ".png"))
  if (!file.exists(logo_path)) {
    download.file(teams$logo_url[i], logo_path, mode = "wb", quiet = TRUE)
  }
}

nick_of <- function(team) teams$nick[match(team, teams$team)]
the <- function(team) paste("the", nick_of(team))

# "the Ravens, Panthers and Raiders" (with optional records in brackets)
the_list <- function(team, record = NULL) {
  names <- nick_of(team)
  if (!is.null(record)) names <- sprintf("%s (%s)", names, record)
  paste("the", join_and(names))
}

# "A", "A and B", "A, B and C"
join_and <- function(x) {
  n <- length(x)
  if (n <= 1) return(paste(x, collapse = ""))
  if (n == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = ", "), " and ", x[n])
}

num_word <- function(n) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine", "ten", "eleven", "twelve")
  if_else(n >= 1 & n <= 12, words[pmax(pmin(n, 12), 1)], as.character(n))
}

cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

fmt_record <- function(w, l, t) {
  if_else(t > 0, paste(w, l, t, sep = "-"), paste(w, l, sep = "-"))
}

pct <- function(x) {
  case_when(
    is.na(x) ~ "",
    x == 0 ~ "0%",
    x < 0.001 ~ "<0.1%",
    x < 0.01 ~ sprintf("%.1f%%", 100 * x),
    TRUE ~ sprintf("%.0f%%", 100 * x)
  )
}

fmt_day <- function(d) sub(" 0", " ", format(d, "%a, %b %d"))

plural <- function(n, one, many = paste0(one, "s")) if_else(n == 1, one, many)


# =============================================================================
# 2. History on the full baseline: floors, odds, and what records became
# =============================================================================

th_full <- read_csv(file.path(data_dir, "loss_thresholds_by_games.csv"),
                    show_col_types = FALSE) |>
  filter(baseline == primary_baseline) |>
  arrange(games_played)
stopifnot(all(th_full$games_played == seq_len(nrow(th_full))))

champ_floor <- th_full$champion_threshold   # indexed by games played
playoff_floor <- th_full$playoff_threshold
max_games <- nrow(th_full)

tier_of <- function(losses, games) {
  as.character(classify_tier(losses, champ_floor[games], playoff_floor[games]))
}

# The team-seasons that set a floor, e.g. "2000 BAL, 2001 NE"
floor_examples <- function(games, which = c("champion", "playoff"), n = 4) {
  which <- match.arg(which)
  raw <- th_full[[paste0(which, "_max_losses")]][games]
  limit <- th_full[[paste0(which, "_threshold")]][games]
  if (is.na(raw) || raw != limit) return("")
  ex <- str_split_1(th_full[[paste0(which, "_boundary")]][games], ";\\s*") |>
    str_remove("\\s\\S+$") |>       # drop the record, keep "year team"
    unique()
  more <- if (length(ex) > n) sprintf(" and %d more", length(ex) - n) else ""
  paste0(paste(head(ex, n), collapse = ", "), more)
}

history_full <- panel |>
  filter(!is_bye) |>
  mutate(loss_equiv = losses + 0.5 * ties)

# smooth_odds() and pava_decreasing() come from weekly_update.R
odds_full <- smooth_odds(history_full, "sb_champion") |>
  rename(title_odds = odds) |>
  left_join(
    smooth_odds(history_full, "made_playoffs") |> rename(playoff_odds = odds),
    by = c("games_played", "loss_equiv")
  )

# Every team-season that had this exact record after this many games
record_comps <- history_full |>
  mutate(final_pct = (final_wins + 0.5 * final_ties) /
           (final_wins + final_losses + final_ties)) |>
  group_by(games_played, wins, losses, ties) |>
  summarise(
    n_teams = n(),
    n_playoffs = sum(made_playoffs),
    n_champions = sum(sb_champion),
    best_finish = fmt_record(final_wins[which.max(final_pct)],
                             final_losses[which.max(final_pct)],
                             final_ties[which.max(final_pct)]),
    .groups = "drop"
  )

history_years <- range(history_full$season)


# =============================================================================
# 3. One section per completed week
# =============================================================================

tier_class <- c("Championship Path" = "sg-tier-1", "Playoff Path" = "sg-tier-2",
                "Graveyard" = "sg-tier-3")
tier_rule <- c(
  "Championship Path" = "no more losses than any eventual champion had at this point",
  "Playoff Path" = "more than any champion, but no more than some playoff team",
  "Graveyard" = "more losses than any playoff team has ever had at this point"
)

# The three boxes, as HTML. Teams sorted by losses, then wins. A team that
# arrived in its box this week is tagged.
tier_boxes_html <- function(tw) {
  box <- function(tier) {
    rows <- tw |>
      filter(tier_full == tier) |>
      arrange(losses, desc(wins), team)
    items <- if (nrow(rows) == 0) {
      '<li class="sg-tier-empty">Nobody yet</li>'
    } else {
      sprintf(
        '<li%s><img src="%s/%s.png" alt="%s" width="56" height="56" loading="lazy"><span class="sg-tier-rec">%s</span>%s</li>',
        if_else(rows$movement_full != "none", ' class="is-new"', ""),
        logo_web_dir, rows$team, rows$team_name, rows$record,
        if_else(rows$movement_full != "none", '<span class="sg-tier-tag">new</span>', "")
      )
    }
    c(
      sprintf('<section class="sg-tier %s">', tier_class[[tier]]),
      '<div class="sg-tier-head">',
      sprintf('<h3>%s</h3>', tier),
      sprintf('<span class="sg-tier-count">%d %s</span>', nrow(rows), plural(nrow(rows), "team")),
      sprintf('<span class="sg-tier-rule">%s</span>', tier_rule[[tier]]),
      '</div>',
      '<ul class="sg-tier-teams">',
      items,
      '</ul>',
      '</section>'
    )
  }
  c("```{=html}", '<div class="sg-tiers">', unlist(map(tier_levels, box)),
    "</div>", "```")
}

# A bullet with a bold lead
bullet <- function(lead, text) sprintf("- **%s** %s", lead, text)

comp_sentence <- function(games, wins, losses, ties, record) {
  rc <- record_comps |>
    filter(games_played == games, wins == !!wins, losses == !!losses,
           ties == !!ties)
  if (nrow(rc) == 0 || rc$n_teams == 0) {
    return(sprintf("No team since %d had started %s.", history_years[1], record))
  }
  made <- case_when(
    rc$n_playoffs == 0 ~ "none made the playoffs",
    rc$n_champions > 0 ~ sprintf("%d made the playoffs and %d won the Super Bowl",
                                 rc$n_playoffs, rc$n_champions),
    TRUE ~ sprintf("%d made the playoffs", rc$n_playoffs)
  )
  sprintf("%d %s since %d %s started %s; the best of them finished %s and %s.",
          rc$n_teams, plural(rc$n_teams, "team"), history_years[1],
          plural(rc$n_teams, "has", "have"), record, rc$best_finish, made)
}

# --- What moved this week ----------------------------------------------------

moved_lines <- function(w, tw) {
  mv <- movements |>
    filter(baseline == primary_baseline, week == w) |>
    inner_join(tw |> select(team, wins, ties, opponent, home_away), by = "team")
  out <- character()

  ent <- mv |> filter(movement == "entered Graveyard")
  if (nrow(ent)) {
    out <- c(out, bullet(
      "Into the Graveyard:",
      paste0(the_list(ent$team, sprintf("%s, after losing %s %s", ent$record,
                                        if_else(ent$home_away == "home", "to", "at"),
                                        the(ent$opponent))), ".")
    ))
  }

  left <- mv |> filter(movement == "left Graveyard")
  if (nrow(left)) {
    for (i in seq_len(nrow(left))) {
      g <- left$games_played[i]
      ex <- floor_examples(g, "playoff")
      out <- c(out, bullet(
        "Out of the Graveyard:",
        sprintf(paste(
          "%s at %s. The playoff floor steps up to %s losses at %s games%s,",
          "so this is a return to the Playoff Path, not a comeback: no team",
          "that has entered the Graveyard has ever made the playoffs."),
          the(left$team[i]), left$record[i], num_word(playoff_floor[g]), num_word(g),
          if (ex != "") sprintf(" (%s all made it from there)", ex) else "")
      ))
    }
  }

  down <- mv |> filter(movement == "down to Playoff Path")
  if (nrow(down)) {
    out <- c(out, bullet(
      "Off the Championship Path:",
      sprintf("%s. No eventual champion since %d had that many losses at the same point.",
              the_list(down$team, down$record), history_years[1])
    ))
  }

  up <- mv |> filter(movement == "up to Championship Path")
  if (nrow(up)) {
    for (i in seq_len(nrow(up))) {
      g <- up$games_played[i]
      ex <- floor_examples(g, "champion")
      out <- c(out, bullet(
        "Back on the Championship Path:",
        sprintf("%s at %s. The champion floor steps up to %s losses at %s games%s.",
                the(up$team[i]), up$record[i], num_word(champ_floor[g]), num_word(g),
                if (ex != "") sprintf(" (%s)", ex) else "")
      ))
    }
  }

  if (length(out) == 0) {
    n_cp <- sum(tw$tier_full == "Championship Path")
    if (n_cp == nrow(tw)) {
      g <- max(tw$games_played)
      ex <- floor_examples(g, "champion")
      out <- bullet(
        "Nobody has moved.",
        sprintf("All %d teams are on the Championship Path: eventual champions have %s%s.",
                nrow(tw),
                if (g == 1) "lost their opener" else
                  sprintf("lost %s of their first %s games", num_word(champ_floor[g]), num_word(g)),
                if (ex != "") sprintf(" (%s)", ex) else "")
      )
    } else {
      out <- bullet("Nobody moved this week.", "")
    }
  }
  out
}

# --- The Graveyard and the two floors ---------------------------------------

standing_lines <- function(tw) {
  out <- character()

  gy <- tw |>
    filter(tier_full == "Graveyard") |>
    arrange(desc(losses_past_playoff_full), wins)
  for (i in seq_len(nrow(gy))) {
    r <- gy[i, ]
    out <- c(out, bullet(
      sprintf("%s, %s:", cap1(the(r$team)), r$record),
      sprintf("%s %s past the playoff floor of %s after %s games, week %d in the Graveyard. %s",
              num_word(r$losses_past_playoff_full),
              plural(r$losses_past_playoff_full, "loss", "losses"),
              num_word(r$playoff_threshold_full), num_word(r$games_played),
              r$weeks_in_tier_full,
              comp_sentence(r$games_played, r$wins, r$losses, r$ties, r$record))
    ))
  }

  # Teams on a floor whose next loss would drop them a tier. (A team sitting
  # on a floor that steps up at its next game is not on the cusp, so it is
  # left out: early in the season that would be half the league.)
  on_cusp <- tw |>
    filter(games_played < max_games) |>
    mutate(after_loss = map2_chr(losses, games_played, \(l, g) tier_of(l + 1, g + 1))) |>
    filter(after_loss != tier_full)

  cf <- on_cusp |>
    filter(tier_full == "Championship Path", losses == champion_threshold_full) |>
    group_by(games_played, record, losses) |>
    summarise(teams = list(team), .groups = "drop")
  for (i in seq_len(nrow(cf))) {
    r <- cf[i, ]
    ex <- floor_examples(r$games_played, "champion")
    out <- c(out, bullet(
      "On the champion floor:",
      sprintf("%s at %s, the worst start any champion has had after %s games%s. A loss drops them to the Playoff Path.",
              the_list(r$teams[[1]]), r$record, num_word(r$games_played),
              if (ex != "") sprintf(" (%s)", ex) else "")
    ))
  }

  pf <- on_cusp |>
    filter(tier_full == "Playoff Path", losses == playoff_threshold_full) |>
    group_by(games_played, record, losses) |>
    summarise(teams = list(team), .groups = "drop")
  for (i in seq_len(nrow(pf))) {
    r <- pf[i, ]
    ex <- floor_examples(r$games_played, "playoff")
    out <- c(out, bullet(
      "On the playoff floor:",
      sprintf("%s at %s, the most losses any playoff team has had after %s games%s. One more loss is the Graveyard.",
              the_list(r$teams[[1]]), r$record, num_word(r$games_played),
              if (ex != "") sprintf(" (%s)", ex) else "")
    ))
  }
  out
}

# --- What is at stake next week ---------------------------------------------

# What one result would do to a team's tier at its next game: a named list of
# phrases, keyed by the kind of stake so the two teams in a game can share one
stake_phrases <- list(
  out_of_graveyard = "a win lifts %s out of the Graveyard",
  back_to_cp = "a win puts %s back on the Championship Path",
  into_graveyard = "a loss puts %s in the Graveyard",
  down_to_pp = "a loss drops %s to the Playoff Path"
)
shared_subject <- c(out_of_graveyard = "the winner", back_to_cp = "the winner",
                    into_graveyard = "the loser", down_to_pp = "the loser")

team_stake <- function(r) {
  g <- r$games_played + 1
  if (g > max_games) return(list())
  now <- r$tier_full
  after_win <- tier_of(r$losses, g)
  after_loss <- tier_of(r$losses + 1, g)
  stakes <- list()
  if (now == "Graveyard") {
    if (after_win != "Graveyard") {
      stakes$out_of_graveyard <- the(r$team)
    } else {
      exits <- which(playoff_floor >= r$losses)
      exits <- exits[exits > r$games_played]
      stakes$stuck <- sprintf(
        "nothing can get %s out this week%s", the(r$team),
        if (length(exits)) sprintf(" (the earliest exit is game %d, and only by winning every game until then)", exits[1]) else ""
      )
    }
  } else {
    if (after_win == "Championship Path" && now != "Championship Path") {
      stakes$back_to_cp <- the(r$team)
    }
    if (after_loss == "Graveyard") {
      stakes$into_graveyard <- the(r$team)
    } else if (after_loss == "Playoff Path" && now == "Championship Path") {
      stakes$down_to_pp <- the(r$team)
    }
  }
  stakes
}

# One sentence for a game: stakes both teams share are said once
game_stakes <- function(away, home) {
  a <- team_stake(away)
  h <- team_stake(home)
  parts <- character()
  for (kind in names(stake_phrases)) {
    if (!is.null(a[[kind]]) && !is.null(h[[kind]])) {
      parts <- c(parts, sprintf(stake_phrases[[kind]], shared_subject[[kind]]))
    } else if (!is.null(a[[kind]])) {
      parts <- c(parts, sprintf(stake_phrases[[kind]], a[[kind]]))
    } else if (!is.null(h[[kind]])) {
      parts <- c(parts, sprintf(stake_phrases[[kind]], h[[kind]]))
    }
  }
  c(parts, a$stuck, h$stuck)
}

stakes_lines <- function(w, tw) {
  nxt <- season_games |> filter(week == w + 1) |> arrange(gameday, gametime)
  if (nrow(nxt) == 0) {
    return("- **The regular season is over.** The tiers are final.")
  }
  out <- character()
  for (i in seq_len(nrow(nxt))) {
    g <- nxt[i, ]
    parts <- game_stakes(tw[tw$team == g$away_team, ], tw[tw$team == g$home_team, ])
    if (length(parts) == 0) next
    out <- c(out, sprintf("- **%s at %s** (%s): %s.", nick_of(g$away_team),
                          nick_of(g$home_team), fmt_day(g$gameday),
                          cap1(paste(parts, collapse = "; "))))
  }
  if (length(out) == 0) {
    out <- sprintf("- **No tier can change in week %d.** Every team is still inside the floors for its next game.", w + 1)
  }
  byes <- setdiff(season_teams$team, c(nxt$away_team, nxt$home_team))
  if (length(byes)) {
    out <- c(out, sprintf("- **On a bye:** %s. A tier can't change on a bye.",
                          join_and(nick_of(sort(byes)))))
  }
  out
}

# --- Smoothed odds by record -------------------------------------------------

odds_table <- function(tw) {
  rows <- tw |>
    mutate(loss_equiv = losses + 0.5 * ties) |>
    left_join(odds_full, by = c("games_played", "loss_equiv")) |>
    group_by(record, games_played, losses, title_odds, playoff_odds) |>
    summarise(teams = join_and(nick_of(team)), .groups = "drop") |>
    arrange(desc(title_odds), desc(playoff_odds), losses, record)
  c(
    "| Record | Teams | Title | Playoffs |",
    "|:-------|:------|------:|---------:|",
    sprintf("| %s | %s | %s | %s |", rows$record, rows$teams, pct(rows$title_odds),
            pct(rows$playoff_odds))
  )
}

# --- Assemble one week -------------------------------------------------------

story_path <- function(w) file.path(stories_dir, sprintf("week%02d.qmd", w))

week_section <- function(w) {
  tw <- tiers_wide |>
    filter(week == w) |>
    mutate(tier_full = as.character(tier_full)) |>
    left_join(teams, by = "team")
  counts <- map_int(tier_levels, \(t) sum(tw$tier_full == t))
  last_game <- season_games |> filter(week == w) |> pull(gameday) |> max()

  if (!file.exists(story_path(w))) {
    writeLines(c(
      sprintf("**[PLACEHOLDER: Jackson's take on week %d. Replace this line; the rest of the week is generated.]**", w),
      ""
    ), story_path(w))
  }

  c(
    sprintf("::: {#week-%d .sg-week%s}", w, if (w == completed_week) " .is-active" else ""),
    "",
    sprintf("### After week %d {.unlisted}", w),
    "",
    sprintf("%d on the Championship Path, %d on the Playoff Path, %d in the Graveyard. Games through %s.",
            counts[1], counts[2], counts[3], fmt_day(last_game)),
    "",
    tier_boxes_html(tw),
    "",
    "#### What moved {.unlisted}",
    "",
    moved_lines(w, tw),
    standing_lines(tw),
    "",
    "#### My take {.unlisted}",
    "",
    sprintf("{{< include _nfl-graveyard/stories/week%02d.qmd >}}", w),
    "",
    sprintf("#### At stake in week %d {.unlisted}", w + 1),
    "",
    stakes_lines(w, tw),
    "",
    sprintf('::: {.callout-note collapse="true" title="Title and playoff odds by record, after week %d"}', w),
    odds_table(tw),
    "",
    sprintf("Share of team-seasons %d-%d with the same losses after the same number of games that won the Super Bowl or made the playoffs, smoothed so the odds never rise with more losses (ties count as half a loss).",
            history_years[1], history_years[2]),
    ":::",
    "",
    ":::",
    ""
  )
}


# =============================================================================
# 4. The tracker fragment and the article metadata
# =============================================================================

latest <- tiers_wide |>
  filter(week == completed_week) |>
  mutate(tier_full = as.character(tier_full))
latest_counts <- map_int(tier_levels, \(t) sum(latest$tier_full == t))

week_nav <- c(
  "```{=html}",
  '<div class="sg-week-nav">',
  '<label for="sg-week-select">// Show the tiers after</label>',
  '<select id="sg-week-select">',
  sprintf('<option value="%d"%s>Week %d</option>', seq_len(completed_week),
          if_else(seq_len(completed_week) == completed_week, " selected", ""),
          seq_len(completed_week)),
  '</select>',
  '</div>',
  "```"
)

# Runs after the week sections, so it can find them on load
week_script <- c(
  "```{=html}",
  '<script>',
  '(function () {',
  '  var sel = document.getElementById("sg-week-select");',
  '  if (!sel) return;',
  '  function show(n, remember) {',
  '    document.querySelectorAll(".sg-week").forEach(function (el) {',
  '      el.classList.toggle("is-active", el.id === "week-" + n);',
  '    });',
  '    sel.value = n;',
  '    if (remember && history.replaceState) history.replaceState(null, "", "#week-" + n);',
  '  }',
  '  sel.addEventListener("change", function () { show(sel.value, true); });',
  '  var m = location.hash.match(/^#week-(\\d+)$/);',
  '  if (m && document.getElementById("week-" + m[1])) show(m[1], false);',
  '})();',
  '</script>',
  "```"
)

tracker <- c(
  sprintf("<!-- Generated by %s/graveyard_site.R (via weekly_update.R). Do not edit: edit the story files in _nfl-graveyard/stories/ instead. -->",
          project_dir),
  "",
  week_nav,
  "",
  unlist(map(seq_len(completed_week), week_section)),
  week_script
)
writeLines(tracker, file.path(site_dir, "tracker.qmd"))

latest_day <- season_games |> filter(week == completed_week) |> pull(gameday) |> max()
writeLines(c(
  "# Generated by graveyard_site.R: the parts of the front matter that change",
  "# every week. The rest lives in nfl-graveyard.qmd.",
  sprintf('date: %s', format(latest_day + 1)),
  "facts:",
  "  - label: Through",
  sprintf('    value: "Week %d"', completed_week),
  "  - label: In the Graveyard",
  sprintf('    value: "%d of %d"', latest_counts[3], nrow(latest)),
  "  - label: History",
  sprintf('    value: "%d–%d"', history_years[1], history_years[2]),
  "  - label: Verdict",
  '    value: "Nobody has ever come back"'
), file.path(site_dir, "meta.yml"))

cat("Wrote the website tracker for", current_season, "through week", completed_week,
    "to", site_dir, "\n")
