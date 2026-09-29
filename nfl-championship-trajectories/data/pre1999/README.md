# 1966–1998 games (temporary)

`nfl_games_1966_1998.csv` holds every NFL/AFL game from 1966 to 1998 (7,137 games, regular season and playoffs). It was built during a one-off scratch session and hasn't been added to `analysis.R` yet. The plan is for `analysis.R` to download and merge the two sources itself, then drop this folder.

## Sources

- **FiveThirtyEight** `nfl_games.csv`: https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv. Complete for every season, but it has no week numbers.
- **spreadspoke** `spreadspoke_scores.csv`: https://raw.githubusercontent.com/tobycrabtree/spreadspoke/master/spreadspoke_scores.csv. Has official week numbers, but leaves out the 42 replacement-player games from 1987.

Games were matched on date and team pair. Every spreadspoke game matched one in 538.

## Fixes applied

- 1987 replacement games: weeks 4–6 assigned by date.
- 1990-10-07 CIN@LAR and DET@MIN: relabelled from week 4 to week 5, where they were actually played.

`scratch_merge.R` is the merge code as it was run: `merge.R`, then `fix.R`, with the source files in the working directory. It's kept for reference and isn't part of the pipeline.

## Notes

- Team codes are 538's franchise codes (e.g. IND = Baltimore Colts, TEN = Houston Oilers, OAK = LA Raiders). `sp_home_name` and `sp_away_name` hold the names as they were in that season. They follow spreadspoke's home/away, which isn't always 538's `team1`/`team2` at neutral sites, so use the codes to identify teams.
- `week` is text for playoff games (Wildcard, Division, Conference, Superbowl). The regular season is `playoff == 0`.
- Checked: 1972 MIA 14-0, 1985 CHI 15-1, 1982 nine-game season, 1987 fifteen-game season, and no team plays twice in one week.
