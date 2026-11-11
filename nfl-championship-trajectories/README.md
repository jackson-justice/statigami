# NFL Championship Trajectories

**How strongly does an NFL team's record at each point in the regular season relate to eventually winning the Super Bowl?**

The starting point is a common claim: *roughly 60% of Super Bowl winners started 2-0, and only a few started 0-2.* This project checks that claim, then puts it in context by comparing eventual champions with every NFL team, week by week.

## Status

- [x] Data source chosen
- [x] Team-season-week dataset built and validated
- [x] First look at the 2-0 claim
- [x] Extended to the full Super Bowl era (1966–2025, 60 seasons)
- [x] Loss thresholds by games played, for two baselines
- [x] NFL Graveyard tiers for the 2026 season (backfilled weekly)
- [x] Figures: the floors, and the 2026 tiers week by week
- [x] Website tracker: tier boxes and weekly notes for every completed week (`website/articles/nfl-graveyard.qmd`)
- [ ] Week-by-week trajectories (champions vs. field)

## Two baselines

Everything is calculated twice, and both versions are kept:

| Baseline | Seasons | Super Bowls | Team-seasons | Title rate |
|------------|-----------:|-----------:|-----------:|-----------:|
| `full` | 1966–2025 | 60 | 1,770 | 3.4% |
| `modern` | 1999–2025 | 27 | 861 | 3.1% |

More history is not automatically a better comparison. The league changed underneath these numbers (see [What changed between eras](#what-changed-between-eras)), so the modern baseline stays available to show what the older seasons add or distort.

## First look: the 2-0 claim

| Start after 2 games | Baseline | All teams | Champions | Share of champions | Champion rate | vs. baseline |
|------------|------------|-----------:|-----------:|-----------:|-----------:|-----------:|
| 2-0 | full | 462 | 36 | 60% | 7.8% | 2.3× |
| 1-1 | full | 815 | 19 | 32% | 2.3% | 0.7× |
| 0-2 | full | 461 | 3 | 5% | 0.7% | 0.2× |
| 1-0-1 / 0-1-1 | full | 32 | 2 | 3% | — | — |
| 2-0 | modern | 225 | 14 | 52% | 6.2% | 2.0× |
| 1-1 | modern | 401 | 11 | 41% | 2.7% | 0.9× |
| 0-2 | modern | 227 | 2 | 7% | 0.9% | 0.3× |
| 1-0-1 / 0-1-1 | modern | 8 | 0 | — | — | — |

- **The 60% figure is right for the full Super Bowl era** (36 of 60 champions) **and too high for the modern one** (14 of 27, about half).
- **"Only a few started 0-2" holds up:** 3 of 60 (1993 Cowboys, 2001 Patriots, 2007 Giants).
- **The key context is the denominator.** About a quarter of all teams start 2-0, so a 2-0 start roughly doubles a team's title odds, from \~3% to \~6–8%. It still means a 92% or better chance of *not* winning.

## Loss thresholds by games played

The most losses any eventual champion, and any eventual playoff team, had after each number of games. These are the boundaries for the planned NFL Graveyard tiers:

1.  **Championship Path:** no more losses than the worst-placed eventual champion at that point.
2.  **Playoff Path:** more than that, but no more than the worst-placed eventual playoff team.
3.  **Graveyard:** more losses than any eventual playoff team.

| After game | Champion max losses (both baselines) | Playoff max losses, modern | Playoff max losses, full | Seasons in full baseline |
|-----------:|-----------:|-----------:|-----------:|-----------:|
| 1 | 1 | 1 | 1 | 60 |
| 2 | 2 | 2 | 2 | 60 |
| 3 | 2 | 3 | 3 | 60 |
| 4 | 3 | 3 | **4** | 60 |
| 5 | 3 | 4 | 4 | 60 |
| 6 | 3 | 5 | 5 | 60 |
| 7 | 4 | 5 | **6** | 60 |
| 8 | 4 | 6 | 6 | 60 |
| 9 | 4 | 7 | 7 | 60 |
| 10 | 5 | 7 | 7 | 59 |
| 11 | 5 | 7 | 7 | 59 |
| 12 | 6 | 8 | 8 | 59 |
| 13 | 6 | 8 | 8 | 59 |
| 14 | 7 | 8 | 8 | 59 |
| 15 | 7 | 9 | 9 | 47 |
| 16 | 7 | 9 | 9 | 46 |
| 17 | 6 (used as 7) | 9 | 9 | 5 |

- **The champion threshold is the same in both baselines at every point.** Thirty-three more champions did not move it. The older seasons only add teams that tie an existing boundary (1980 Raiders at 2-3 and 3-3, 1981 49ers at 1-2, 1986 Giants at 0-1, 1988 49ers at 6-5, 1993 Cowboys at 0-2).
- **The playoff threshold moves at two points**, each set by a single team:
    - **Game 4: the 1992 Chargers** started 0-4 and finished 11-5 as AFC West champions. They are the only one of 170 teams to start 0-4 and make the playoffs. On the modern baseline 0-4 is the Graveyard; on the full baseline it is the Playoff Path.
    - **Game 7: the 1970 Bengals** started 1-6, won their last seven and took the AFC Central at 8-6. They are the only one of 216 teams with six or more losses after seven games to make the playoffs. That was a four-team division in the first season after the merger, and nobody else in it finished above 7-7.
- Dropping 1982, 1987, the pre-merger seasons, or the 1968 Chiefs (whose only playoff game was a tiebreaker) changes no threshold.

The full tables, with tier counts and the team-seasons at each boundary, are in `data/loss_thresholds_by_games.csv`. The `champion_max_losses` and `playoff_max_losses` columns are the raw maxima above. The `champion_threshold` and `playoff_threshold` columns are the same numbers carried forward so they never decrease in games played; those are what the tiers use. The only cell that changes is game 17, where the champion threshold is 7 instead of the raw 6 (see below).

### What changed between eras

| Era | Schedule | Playoff teams | Share of teams in playoffs |
|------------|------------|------------|-----------:|
| 1966–69 | 14 games, AFL and NFL separate | 4, 6, 7, 8 | 25% |
| 1970–77 | 14 games | 8 | 30% |
| 1978–89 | 16 games (9 in 1982, 15 in 1987) | 10 (16 in 1982) | 38% |
| 1990–98 | 16 games | 12 | 42% |
| 1999–2019 | 16 games | 12 | 38% |
| 2020 | 16 games | 14 | 44% |
| 2021–25 | 17 games | 14 | 44% |

Things to keep in mind when reading a threshold:

- **The same game count is a different point in different seasons.** Game 14 was the finale through 1977 and has three games left today. Late thresholds are set by the long seasons, because a team with games left has room to recover.
- **Not every season reaches every count.** Games 15–16 have no pre-1978 teams, games 10 on have no 1982 teams, and game 17 has only five seasons. That is why the raw champion threshold *drops* from 7 losses at game 16 to 6 at game 17: the 2011 Giants (7-7, then 9-7) never played a 17th game. The tiers carry the 7 forward, so a team can't drop a tier by winning.
- **A playoff spot was harder to get.** With a quarter of teams qualifying instead of 44%, older seasons rarely set a playoff boundary. The two that do come from a weak division (1970) and a long winning streak (1992).
- **Ties were common before overtime.** There are 62 tie games in 1966–73 and 30 in the 52 seasons since. The thresholds count losses only, so a tie is not a step toward the Graveyard. Counting a tie as half a loss would move the playoff threshold only at games 11–14 (by half a loss) and change the tier of about 1% of team-games.
- **Thresholds are records, not odds.** One team sets each boundary, and the first team to beat it resets it.

## NFL Graveyard tiers for 2026

After every completed week of the current season, `graveyard_tiers.R` puts every team in one of the three tiers by comparing its losses with the thresholds above at the same number of games. It rebuilds every completed week on each run (the backfill) and tracks every move between tiers.

**Decisions:**

1.  **The full baseline (1966 on) is the primary tier.** The modern tier (1999 on) is carried alongside in every file as a sensitivity check. They differ only after game 4 (0-4 is the Playoff Path on full, the Graveyard on modern) and game 7 (1-6, likewise).
2.  **Thresholds are frozen at the end of the 2025 season.** They come from `analysis.R`, which only reads completed seasons. Nothing a 2026 team does changes them.
3.  **Losses only.** A tie is not a loss. The one tie so far (NYG 27, WAS 27 in week 5) changes no tier: WAS at 2-5-1 after 8 games would be on the Playoff Path either way.
4.  **Thresholds never decrease in games played.** The raw maxima are carried forward (`cummax`), which changes only game 17. So a team can move down only by losing, and up only when the threshold steps up at its next game count.
5.  **Indexed by games played.** A team on a bye keeps last week's tier.
6.  **Leaving the Graveyard is not a comeback.** The playoff threshold steps up at games 6, 7, 9, 12 and 15 (full baseline), so a Graveyard team that wins into one of those counts returns to the Playoff Path. Historically that is common and means nothing: 611 team-seasons since 1966 entered the Graveyard (322 since 1999), 186 of them left it at least once (226 exits in all), and none made the playoffs. The movement is recorded as `left Graveyard`, with the trigger `threshold stepped up`.
7.  **Breaking a threshold is flagged, not absorbed.** `losses_past_playoff` says how many losses past the playoff limit a team is, i.e. by how much a playoff berth would set a new record. Once the playoffs start, `made_playoffs`, `sb_champion` and `breaks_threshold_*` fill in, and the report lists any playoff team that had a Graveyard week or any champion with a week off the Championship Path. The thresholds themselves move only when `analysis.R` is rerun after the Super Bowl with `last_complete_season` advanced.

**Through week 9 (2026):**

| Week | Championship Path | Playoff Path | Graveyard | Modern baseline, if different |
|-----:|-----:|-----:|-----:|------------|
| 1 | 32 | 0 | 0 | |
| 2 | 32 | 0 | 0 | |
| 3 | 27 | 5 | 0 | |
| 4 | 27 | 5 | 0 | 27 / 0 / 5 |
| 5 | 25 | 4 | 3 | |
| 6 | 18 | 12 | 2 | |
| 7 | 21 | 10 | 1 | 21 / 9 / 2 |
| 8 | 20 | 10 | 2 | 20 / 9 / 3 |
| 9 | 16 | 13 | 3 | |

- **Graveyard entries and exits (full baseline):** LAC, MIA and TEN entered at 0-5 in week 5. TEN (week 6) and MIA (week 7) left at 1-5 when the limit stepped up to 5 at game 6, then re-entered at 1-7 (TEN week 8, MIA week 9). LAC has not left since week 5 and sits at 0-8, two losses past the limit of 6.
- **On the modern baseline** the five 0-4 teams (HOU, LAC, MIA, TB, TEN) entered in week 4; HOU and TB left at 1-4 the next week and have stayed out, and TEN and MIA each entered a week earlier at 1-6.
- **The two baselines disagree in 7 of 288 team-weeks**, all at the two known spots: the five 0-4 teams in week 4, TEN at 1-6 in week 7 and MIA at 1-6 in week 8. After week 9 they agree on all 32 teams.
- Every move so far is consistent with the results: 31 tier changes on the full baseline (35 on modern), every downward one on a loss and every upward one on a win into a step in the threshold. No team changed tier on a bye.

**Outputs** (`data/graveyard/`, rewritten every run):

- `2026_tiers.csv`: one row per team per completed week (288 rows through week 9).
- `2026_movements.csv`: one row per tier change per baseline.
- `2026_tier_counts.csv`: teams in each tier by week and baseline.

Columns of `2026_tiers.csv` (the `_full` and `_modern` suffixes give each baseline's version):

| Column | Meaning |
|------------------------------------|------------------------------------|
| `season`, `week`, `team` | Keys |
| `is_bye`, `opponent`, `home_away`, `outcome` | That week's game (NA on byes) |
| `games_played`, `wins`, `losses`, `ties`, `record`, `point_diff` | Cumulative record after this week, as in `team_week_records.csv` |
| `champion_threshold_*`, `playoff_threshold_*` | The thresholds in force at this games-played count |
| `tier_*` | Championship Path, Playoff Path or Graveyard |
| `prev_tier_*`, `movement_*`, `trigger_*` | Last week's tier; `none`, `down to Playoff Path`, `entered Graveyard`, `left Graveyard` or `up to Championship Path`; whether a `loss` or the `threshold stepped up` caused it |
| `losses_past_playoff_*` | Losses minus the playoff threshold. Positive means a playoff berth would set a record. |
| `weeks_in_tier_*` | Consecutive calendar weeks in the current tier, byes included |
| `tiers_disagree` | TRUE where the two baselines put the team in different tiers |
| `made_playoffs`, `sb_champion`, `breaks_threshold_*` | NA until the playoffs start; then whether the season's outcome contradicts the tier |

**Figures** (`figures/`, Statigami ink surface; the tiers take the brand's heat ramp in order, cobalt → magenta → amber, so the Graveyard is always the hot color):

| File | Made by | What it shows |
|------------------------------------|------------|------------------------------------|
| `01_loss_floors_1966_2025.png` | `analysis.R` §10 | The two floors as bands on games played × losses, with the 1992 Chargers, 1970 Bengals and the game-17 carry-forward annotated, and the two cells where the modern floor is lower |
| `02_tier_tiles_<season>.png` | `graveyard_tiers.R` §7 | Every team's tier after every week, one tile per team-week, sorted so the Graveyard is at the bottom. Letter = result, faded = bye, ring = left the Graveyard |
| `03_loss_paths_<season>.png` | `graveyard_tiers.R` §7 | Every team's losses by game on top of the bands; teams that have been in the Graveyard drawn in full, the rest as a faint bundle |
| `04_tier_counts_<season>.png` | `graveyard_tiers.R` §7 | Teams in each tier by week, stacked |

The current-season figures are rebuilt on every weekly run. `chart_setup.R` holds the shared brand setup (and a neutral fallback when `brand/` is missing), the tier colors, and the band/staircase helpers both scripts use.

**Website tracker** (`graveyard_site.R`, sourced at the end of `weekly_update.R`): the article at `website/articles/nfl-graveyard.qmd` has two tabs. "The tracker" opens on the latest completed week, with a week selector; "How it works" holds the explainer and the week 9 figures. For every completed week the script generates the three tier boxes (team logos by tier; a team that changed tier that week gets an arrow, green up or red down, solid when the move crossed the Graveyard line and outlined when it crossed the champion line; a team that has been in the Graveyard at any point this season keeps an amber dagger after its record, in whichever box it sits), a "What moved" list (entries, exits, drops, the Graveyard teams with how every team that had the same record finished, and teams on a floor whose next loss changes their tier), an "At stake" list for the following week (every game in which a result changes a tier, and the byes), and the smoothed title and playoff odds by record, all on the full baseline. It writes:

- `website/articles/_nfl-graveyard/tracker.qmd`: the generated weekly sections, included by the article. Never edit it by hand.
- `website/articles/_nfl-graveyard/meta.yml`: the article's date and facts strip.
- `website/articles/_nfl-graveyard/stories/week<NN>.qmd`: the hand-written take for each week, created with a placeholder when missing and never overwritten. Rerun `weekly_update.R` after editing one.
- `logos/<TEAM>.png`: team logos from nflverse, downloaded once; the site copies them next to the figures.

`graveyard_tiers.R` stops before saving if any of its 13 checks fail: thresholds never decrease and equal the raw maxima carried forward; every team has one row per completed week on both baselines, with a tier and a threshold; games + byes = weeks; records never decrease; every tier agrees with the thresholds when re-evaluated; downward moves happen only on a loss and upward moves only when the threshold stepped up without a loss; no move on a bye; the full tier is never worse than the modern one; and every saved `data/weekly/` table for the season shows the same records as the backfill.

## Data

**Sources:**

- **1999 on:** [nflverse `games.csv`](https://github.com/nflverse/nfldata), a game-level results file maintained by the nflverse project. Each game carries its Pro-Football-Reference ID, and the file is the same data `nflreadr::load_schedules()` returns.
- **1966–1998:** [FiveThirtyEight `nfl_games.csv`](https://github.com/fivethirtyeight/nfl-elo-game) (MIT license), the game file behind their NFL Elo ratings. It covers every NFL and AFL game from 1920 to 2020 with a playoff flag.

`analysis.R` downloads both to `data/raw/` (git-ignored) on first run and reshapes the FiveThirtyEight games into the nflverse columns.

**Outputs:**

- `data/team_week_records.csv`: one row per team × season × regular-season week (28,954 rows).
- `data/record_outcomes_by_games.csv`: for each baseline, every record after every number of games, with how many teams had it and how they finished.
- `data/loss_thresholds_by_games.csv`: for each baseline, the champion and playoff loss thresholds after every number of games.
- `data/reference_1999_2025/`: the first two files as they were before the extension to 1966 (nflverse only). `analysis.R` checks the new panel against them.
- `data/graveyard/`: the current season's tiers, movements and tier counts (see [NFL Graveyard tiers for 2026](#nfl-graveyard-tiers-for-2026)).
- `data/weekly/`: one team-by-team table per completed week of the current season, from `weekly_update.R`.

Columns of `team_week_records.csv`:

| Column | Meaning |
|------------------------------------|------------------------------------|
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

1.  **Two sources, joined at 1999.** nflverse starts in 1999, so 1966–1998 comes from FiveThirtyEight. The two files agree on every one of the 5,852 games they share (1999–2020), which is the basis for trusting the older seasons.
2.  **Only completed seasons.** `last_complete_season` (2025) is set at the top of the script. The in-progress season is excluded because it has no champion yet.
3.  **Byes are rows.** Keeping bye weeks lets us ask "record after week *N*" (calendar) or "record after *N* games" (`games_played`). The 2-0 claim and the loss thresholds use games played, so byes don't distort them.
4.  **Regular season only.** Records are regular-season records. Playoff games only feed `made_playoffs` and `sb_champion`.
5.  **A playoff team is one that played a playoff game.** That includes the 1968 Chiefs, who lost an AFL West tiebreaker playoff, and all 16 teams in the 1982 strike-season tournament. The 1966–69 third-place "Playoff Bowl" exhibitions are not in the data.
6.  **Ties count as half a win** in `win_pct` and are kept as separate records (a 1-0-1 start is not a 2-0 or a 1-1). There are 92 tie games in the window. The loss thresholds count losses only.
7.  **Schedule length changed.** Seasons were 14 games through 1977, 16 from 1978 and 17 from 2021. Comparisons late in the season should use `games_played` or win percentage, not raw wins.
8.  **Strike seasons are kept as played.** 1982 has 9 games per team and 1987 has 15, three of them played by replacement players. Those games count in the official standings, so they count here.
9.  **The AFL counts.** For 1966–69 the AFL and NFL are both included, since both sent a champion to the Super Bowl.
10. **2022 BUF–CIN.** The Week 17 game was suspended after Damar Hamlin's cardiac arrest and cancelled, so both teams have 16 games. This is handled as a known exception.
11. **Team codes are era-specific** (STL → LA, SD → LAC, OAK → LV). FiveThirtyEight's all-time franchise codes are converted to match: `BAL` is the Colts through 1983, `HOU` the Oilers through 1996, `STL` the Cardinals through 1987 (then `PHO` to 1993), `BOS` the Patriots through 1970, `LA` the Rams through 1994, and `RAI` the Los Angeles Raiders of 1982–94. Since the unit is a team-season, relocations don't matter here.
12. **Week labels follow calendar order.** For 1999 on they are nflverse's (the 2001 games postponed after 9/11 are labeled week 17, where they were actually played). FiveThirtyEight has no week numbers, so for 1966–1998 the week is the calendar week of the season, in Tuesday-to-Monday windows from the opening game. That matches the official week for 1970–1998. Weeks lost to the 1982 and 1987 strikes are gaps in the numbering, not byes. For 1966–69 it puts the two leagues, which numbered their weeks separately, on one calendar.

## Validation

`analysis.R` stops before saving if any of these fail. All 23 pass:

- Every season 1966–2025 is present, and no game is missing a score
- One Super Bowl per season, with no ties
- All 60 derived champions match an independent hard-coded list
- Exactly one champion per season, and every champion made the playoffs
- The number of teams matches the league's size in every season (24 in 1966, up to 32 from 2002)
- The number of playoff teams matches the format in every season
- No team plays twice in one week, and games + byes = weeks
- Every team plays a full schedule for its season (14, 16, 17, 9 in 1982, 15 in 1987), except the known 2022 BUF/CIN case
- Final W + L + T = games played, and league-wide wins = losses
- Cumulative records never decrease
- The 1999–2025 rows are identical to the original nflverse-only panel
- Spot checks: 1972 MIA 14-0, 1976 TB 0-14, 1982 WAS 8-1, 1985 CHI 15-1, 1987 SF 13-2, 2007 NE 16-0, 2008 DET 0-16, 2017 CLE 0-16

`validate_sources.R` checks the 1966–1998 source against two others. All 7 checks pass:

- **Against [spreadspoke](https://github.com/tobycrabtree/spreadspoke), 1966–1998:** all 7,095 games in both files have the same score and playoff flag. The only games spreadspoke lacks are the 42 replacement-player games of 1987. (spreadspoke has no license, so it is read for checking only and none of it is stored here.)
- **Against nflverse, 1999–2020:** the two list the same 5,852 games, with the same scores and playoff flags.
- **The 1987 replacement games** have no second game file to check against, so the final 1987 records were compared by hand with the published standings. All 28 match.

## Run it

From the repo root (where `statigami.Rproj` lives):

``` r
source("nfl-championship-trajectories/analysis.R")
source("nfl-championship-trajectories/validate_sources.R")  # optional
source("nfl-championship-trajectories/graveyard_tiers.R")   # current-season tiers
```

Requires `tidyverse`. The figures also need `ragg`, `systemfonts` and `rsvg` for the brand fonts and logo (see `brand/README.md`).

## Weekly update

Every Tuesday during the season, after Monday Night Football:

``` r
source("nfl-championship-trajectories/weekly_update.R")
```

It first sources `graveyard_tiers.R`, which re-downloads the current season's scores, rebuilds the three tiers for every completed week and saves `data/graveyard/`. It then compares every team's record with every team in the historical baseline that had the same record after the same number of games, prints a ready-to-post summary (title and playoff odds by record, unbeaten teams, the three tiers with this week's Graveyard entries and exits, where the modern baseline disagrees, the champion-floor watch) and saves a team-by-team table to `data/weekly/<season>_week<NN>.csv`. Finally it sources `graveyard_site.R`, which rebuilds the website tracker (see above). Run `analysis.R` first; the scripts read its outputs.

After the run: write the week's take in `website/articles/_nfl-graveyard/stories/week<NN>.qmd`, run `weekly_update.R` again (or just `quarto render website` to preview), and commit `data/`, `figures/`, `logos/` and `website/articles/_nfl-graveyard/`.

`history_baseline` at the top of the script picks the baseline for the odds and the floor watch. It is set to `"modern"` (1999 on), which is what every weekly table so far was built with; `"full"` uses 1966 on. The tiers always use both baselines, full as primary. The weekly table keeps its original 18 columns unchanged (the week 3 table reproduces exactly) and appends the tier columns: `tier_full`, `tier_modern`, the previous tiers and movements, `losses_past_playoff_*`, `playoff_threshold_*` and `weeks_in_tier_full`.

Title and playoff odds are smoothed with isotonic regression on losses: the raw rate for each record, pooled with its neighbours wherever a record with more losses would otherwise rank higher. With only 27 champions on the modern baseline, raw rates are noisy (7-1 teams have won more often than 8-0 teams), and a logistic curve overstates the extremes.

## Project structure

``` text
nfl-championship-trajectories/
├── analysis.R
├── validate_sources.R
├── graveyard_tiers.R            (current-season tiers; sourced by weekly_update.R)
├── weekly_update.R
├── graveyard_site.R             (website tracker; sourced by weekly_update.R)
├── chart_setup.R                (brand setup and helpers shared by the figure code)
├── README.md
├── logos/                       (team logos from nflverse, for the website tracker)
├── data/
│   ├── raw/                     (downloads, git-ignored)
│   ├── reference_1999_2025/     (original nflverse-only outputs)
│   ├── graveyard/               (current-season tiers, movements, counts)
│   ├── weekly/                  (one table per week)
│   ├── loss_thresholds_by_games.csv
│   ├── record_outcomes_by_games.csv
│   └── team_week_records.csv
└── figures/
    ├── 01_loss_floors_1966_2025.png
    ├── 02_tier_tiles_2026.png
    ├── 03_loss_paths_2026.png
    └── 04_tier_counts_2026.png
```
