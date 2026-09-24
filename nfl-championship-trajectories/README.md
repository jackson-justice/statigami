# NFL Championship Trajectories

**How strongly does an NFL team's record at each point in the regular season relate to eventually winning the Super Bowl?**

The starting point is a common claim: *roughly 60% of Super Bowl winners started 2-0, and only a few started 0-2.* This project checks that claim, then puts it in context by comparing eventual champions with every NFL team, week by week.

## Status

- [x] Data source chosen
- [x] Team-season-week dataset built and validated
- [x] First look at the 2-0 claim
- [ ] Week-by-week trajectories (champions vs. field)
- [ ] Figures

## First look: the 2-0 claim (1999–2025, 27 Super Bowls)

| Start after 2 games | All teams | Champions | Share of champions | Champion rate | vs. baseline |
|---|---:|---:|---:|---:|---:|
| 2-0 | 225 | 14 | 52% | 6.2% | 2.0× |
| 1-1 | 401 | 11 | 41% | 2.7% | 0.9× |
| 0-2 | 227 | 2 | 7% | 0.9% | 0.3× |
| 1-0-1 / 0-1-1 | 8 | 0 | — | — | — |

Baseline = 27 champions / 861 team-seasons = 3.1%.

- **About half of champions started 2-0, not 60%**, at least since 1999. The 60% figure may hold over the full Super Bowl era (1966–present), which this dataset does not cover yet (see below).
- **"Only a few started 0-2" holds up:** 2 of 27 (2001 Patriots, 2007 Giants).
- **The key context is the denominator.** About a quarter of all teams start 2-0, so a 2-0 start roughly doubles a team's title odds, from ~3% to ~6%. It still means a 94% chance of *not* winning.

## Data

**Source:** [nflverse `games.csv`](https://github.com/nflverse/nfldata), a game-level results file (1999–present) maintained by the nflverse project. Each game carries its Pro-Football-Reference ID, and the file is the same data `nflreadr::load_schedules()` returns. The script downloads it to `data/raw/` (git-ignored) on first run.

**Output:** `data/team_week_records.csv`, with one row per team × season × regular-season week (14,797 rows).

| Column | Meaning |
|---|---|
| `season`, `week`, `team` | Keys. `season` is the year the season started (the 2025 season's Super Bowl was played in Feb 2026). |
| `is_bye` | TRUE on a bye week. The record carries forward. |
| `opponent`, `home_away`, `outcome` | That week's game (NA on byes) |
| `games_played` | Games played through this week |
| `wins`, `losses`, `ties`, `record` | Cumulative record *after* this week's game |
| `win_pct` | (W + 0.5·T) / games played |
| `point_diff` | Cumulative point differential |
| `final_wins`, `final_losses`, `final_ties` | End-of-regular-season record |
| `made_playoffs` | Appeared in any playoff game that season |
| `sb_champion` | Won that season's Super Bowl |

## Data decisions

1. **1999–2025 only.** nflverse's clean game file starts in 1999. Pre-1999 results live mainly on Pro-Football-Reference, which blocks scripted downloads, and the FiveThirtyEight Elo archive that used to cover 1920+ has been taken offline. Extending back to 1966 would require manually exporting each season's schedule from PFR. That's worth doing if the full-history version of the claim matters, but it's a separate step.
2. **Only completed seasons.** `last_complete_season` (2025) is set at the top of the script. The in-progress season is excluded because it has no champion yet.
3. **Byes are rows.** Keeping bye weeks lets us ask "record after week *N*" (calendar) or "record after *N* games" (`games_played`). The 2-0 claim uses games played, so an early bye doesn't distort it.
4. **Regular season only.** Records are regular-season records. Playoff games only feed `made_playoffs` and `sb_champion`.
5. **Ties count as half a win** in `win_pct` and are kept as separate records (a 1-0-1 start is not a 2-0 or a 1-1). There are 15 ties in the window.
6. **Schedule length changed.** Seasons were 16 games through 2020 and 17 from 2021 (weeks 17 → 18). Comparisons late in the season should use `games_played` or win percentage, not raw wins.
7. **2022 BUF–CIN.** The Week 17 game was suspended after Damar Hamlin's cardiac arrest and cancelled, so both teams have 16 games. This is handled as a known exception.
8. **Team codes are era-specific** (STL → LA, SD → LAC, OAK → LV). Since the unit is a team-season, relocations don't matter here.
9. **Week labels follow calendar order.** The 2001 games postponed after 9/11 are labeled week 17 in the source, where they were actually played. The script verified that week ranges never overlap within a season.

## Validation checks

`analysis.R` stops before saving if any of these fail:

- Every season 1999–2025 is present, and no regular-season game is missing a score
- One Super Bowl per season, with no ties
- Derived champions match an independent hard-coded list
- Exactly one champion per season, and every champion made the playoffs
- 31 teams in 1999–2001 and 32 from 2002 on
- No team plays twice in one week, and games + byes = weeks
- Every team plays a full 16/17-game schedule, except the known 2022 BUF/CIN case
- Final W + L + T = games played, and league-wide wins = losses
- Cumulative records never decrease
- Spot checks: 2007 NE 16-0, 2008 DET 0-16, 2017 CLE 0-16

## Run it

From the repo root (where `statigami.Rproj` lives):

```r
source("nfl-championship-trajectories/analysis.R")
```

Requires `tidyverse`.

## Project structure

```text
nfl-championship-trajectories/
├── analysis.R
├── README.md
├── data/
│   ├── raw/games.csv            (downloaded, git-ignored)
│   └── team_week_records.csv
└── figures/
```
