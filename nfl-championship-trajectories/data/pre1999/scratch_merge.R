# ---- merge.R (run first) ----
suppressMessages(library(tidyverse))
tm <- read_lines("spreadspoke_teams.csv") %>% str_split("\r") %>% unlist() %>% I() %>% read_csv(show_col_types=FALSE) %>% select(team_name, team_id) %>%
  mutate(team_id = recode(team_id, WAS="WSH")) %>% bind_rows(tibble(team_name="Boston Patriots", team_id="NE")) %>% distinct()
s <- read_csv("spreadspoke_scores.csv", show_col_types=FALSE, col_types=cols(.default="c")) %>%
  mutate(date=mdy(schedule_date), season=as.integer(schedule_season)) %>% filter(season>=1966, season<=1998) %>%
  left_join(tm, by=c(team_home="team_name")) %>% rename(h=team_id) %>% left_join(tm, by=c(team_away="team_name")) %>% rename(a=team_id)
cat("unmapped:", sum(is.na(s$h)|is.na(s$a)), "\n")
s <- s %>% transmute(date, k1=pmin(h,a), k2=pmax(h,a), week=schedule_week, home=h, sp_home_name=team_home, sp_away_name=team_away)
e <- read_csv("nfl_games_538.csv", show_col_types=FALSE) %>% filter(season>=1966, season<=1998) %>% mutate(k1=pmin(team1,team2), k2=pmax(team1,team2))
m <- e %>% left_join(s, by=c("date","k1","k2"))
cat("538 rows:", nrow(e), " merged rows:", nrow(m), " no week:", sum(is.na(m$week)), "\n")
print(m %>% filter(is.na(week)) %>% count(season, playoff))
# fill 1987 replacement weeks by date rank within season gaps
m <- m %>% group_by(season) %>% mutate(week = ifelse(is.na(week) & season==1987 & playoff==0,
        as.character(case_when(date<=ymd("1987-10-05")~4, date<=ymd("1987-10-12")~5, TRUE~6)), week)) %>% ungroup()
cat("still no week:", sum(is.na(m$week)), "\n")
out <- m %>% transmute(season, date, week, playoff, neutral, team1, team2, score1, score2, home_team_538=team1, sp_home=home, sp_home_name, sp_away_name) %>% select(-home_team_538)
write_csv(out, "nfl_games_1966_1998_with_weeks.csv")
long <- bind_rows(out %>% transmute(season,week,playoff,team=team1,pf=score1,pa=score2), out %>% transmute(season,week,playoff,team=team2,pf=score2,pa=score1))
chk <- long %>% filter(playoff==0) %>% count(season,team,week) %>% filter(n>1); cat("dup team-weeks:", nrow(chk), "\n"); print(head(chk))
print(long %>% filter(season==1987, playoff==0) %>% count(week))
# denominators example: 0-3 teams after week 3 per season (by game count)

# ---- fix.R (run second) ----
suppressMessages(library(tidyverse))
o <- read_csv("nfl_games_1966_1998_with_weeks.csv", show_col_types=FALSE, col_types=cols(week="c")) %>%
  mutate(week = ifelse(date==ymd("1990-10-07") & week=="4", "5", week))
write_csv(o, "nfl_games_1966_1998_with_weeks.csv")
long <- bind_rows(o %>% transmute(season,week,playoff,team=team1,pf=score1,pa=score2), o %>% transmute(season,week,playoff,team=team2,pf=score2,pa=score1))
cat("dup team-weeks:", nrow(long %>% filter(playoff==0) %>% count(season,team,week) %>% filter(n>1)), "\n")
print(table(o$week[o$playoff==1])); print(head(o,3))
