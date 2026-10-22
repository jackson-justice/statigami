# Statigami — National Tight Ends Day

**Working title:** Do Tight Ends Actually Play Better on National Tight Ends Day?

**GitHub issue:** https://github.com/jackson-justice/statigami/issues/2\
**Target publication:** Before Sunday, October 25, 2026\
**Historical window:** 2019–2025; reserve 2026 for a prospective prediction and eventual postgame evaluation.\
**Primary stack:** R, tidyverse, nflreadr, ggplot2, Quarto; Claude Code for implementation assistance.

## Why this story matters

National Tight Ends Day is on the fourth Sunday in October. The NFL-wide celebration began in 2019. The interesting question is not just whether TEs record more yards or touchdowns; it is whether teams *allocate more of the passing offense* to tight ends on their designated day.

**Main research question:** Do NFL tight ends perform better on National Tight Ends Day than they normally do?

This is observational analysis. Avoid concluding that the holiday *causes* any observed change.

## Four research questions

### 1. Individual performance

For each TE active on NTED, compare his targets, receptions, receiving yards and receiving TDs against his own season baseline. Start with other regular-season Sunday appearances in the same season; assess nearby weeks separately. Report both the absolute difference and the percentage difference when the baseline is sufficiently large. Avoid unstable percent changes for players whose expected value is near zero.

**Why:** Can reveal which TEs genuinely outperformed their usual production versus simply having a big game in absolute terms.

### 2. Team TE unit performance

Sum production across every TE for each team-game, including backups and complementary TEs. Calculate TE targets, receptions, yards, TDs, and team target/receiving-yard shares.

**Why:** A team's TE group may outperform even when its TE1 does not, revealing changes in depth usage or play-calling.

### 3. Position-group comparison

Group receivers into TE, WR and RB. Compare each group's targets, receptions, receiving yards and receiving TDs on NTED versus comparable ordinary Sundays. Prefer **shares of team targets** and **shares of receiving yards** to raw totals alone, which can be distorted by number of games or team pass volume. Check how QB or other receivers are represented and account for unusual categorization.

**Why:** Helps distinguish a TE-specific allocation shift from a high-volume passing week across the league.

### 4. 2026 prediction (stretch goal)

Before the October 25 games, rank tight ends or teams by **projected production relative to their own expected baseline**, not simply who is likely to have the most yards. Also show an ordinary expected-yards/targets leaderboard as a separate perspective if helpful.

- Initial baseline: 2026 season-to-date player or team performance, with shrinkage toward a broader historical baseline for low-sample players.
- Use only information available **before** each prediction game, particularly in retrospective tests.
- Candidate inputs: prior targets/routes or team TE target share, pass volume, opponent TE allowance (carefully regularized), injuries/playing status, and historical individual performance.
- Historical NTED adjustment must be estimated from prior seasons only, not the season under evaluation.
- Compare a simple baseline model with a model including an NTED feature. If NTED does not improve held-out accuracy, say so.
- Optional deliverable: ranked chart of biggest predicted **uplifts** with uncertainty and notes about playing-time assumptions.
- After the games: compare predictions with outcomes, and report errors without retroactively changing the forecast.

## Data and methodology

**Main source:** `nflreadr::load_player_stats(2019:2026, summary_level = "week")` plus `nflreadr::load_schedules()` to identify the correct game date, weekday and teams. Use only completed games; 2026 NTED remains untouched for any pregame predictions. Weekly player stats have a `game_id` field; validate the join against schedules and check for changes in schema.

**Follow-up source if necessary:** `nflreadr::load_pbp()` for receiver targets, player position verification, specific game details, or usage metrics not adequately covered by weekly stats. Do not fetch or process full play-by-play unless needed.

**Event definition:** fourth Sunday of October of each season. Join on actual game date, not just week number. Only games **played that Sunday** are NTED games for the primary analysis; Thursday/Monday games in the same NFL week are not NTED. Optionally explore the full NFL week as a clearly labeled sensitivity analysis.

**Observational units:** player-game, team-game TE unit, and team-game-position-group. For league totals, normalize by team-games so bye weeks, overseas schedules, and varying numbers of Sunday games do not manufacture effects.

**Baseline:** Within-player/within-season and within-team/within-season average production on other Sundays. Also compare nearby Sundays or the same seasonal window (e.g., October/adjacent NFL weeks) as sensitivity checks. Beware of byes, missing participation, roster changes, and differences in number of games played. Distinguish zero targets in a played game from a DNP (which should not be silently imputed to zero).

**Uncertainty and dependence:** Plot distributions of differences, not just mean effects. Consider bootstrap uncertainty resampled at the team-game or season level, recognizing only seven NTED seasons exist. Avoid treating multiple TEs on the same team or many plays within one game as independent experiments. TDs are rare and especially volatile. Don't claim statistical significance without a defensible design.

**Exploratory vs confirmatory:** Historical patterns are exploratory. The 2026 prediction and later result provide a cleaner prospective check. Keep the narrative grounded in measured effect sizes and uncertainty.

## Analysis sequence and MVP

- [x] Create the project directory and save this plan.
- [x] Acquire 2019–2025 player-level weekly data and game schedules with R.
- [x] Flag exact NTED Sundays for each year; verify counts, game dates and joins.
- [x] Check TE participation, missing rows, and position definitions.
- [x] Build team-game position-group aggregates and targets/receiving-yard shares.
- [x] Produce three initial views: NTED vs other Sundays, within-TE baseline differences, and team TE target-share differences.
- [x] Add opponent/team/season sensitivity checks only if they would materially change interpretation.
- [x] Decide whether the predictive stretch goal is feasible before Sunday.
  - **Decision: skipped.** No detectable NTED effect on usage, so an NTED-adjusted forecast would reduce to each TE's normal baseline plus noise.
- [x] Case studies (replaces the forecast): George Kittle's NTED games vs his own seasons, and the TEs with the strongest NTED records, benchmarked against what random games from their own seasons would produce (figures 05-06).
- [x] Create article-ready figures and write the Quarto article. Framing: the day does not change TE usage; then individual stories, with luck made explicit.
- [x] Verify citation/data attribution and publication constraints; publish with clearly disclosed sources.
- [ ] Optional after Sunday: add 2026 NTED as one more season of evidence (no pregame forecast to audit).

**Time priority:** If pressed for time, finish the historical descriptive article with strong visuals first. A simple, clearly labeled forecast is better than a complicated unvalidated model.

## Possible visualizations

1.  **Big opening chart:** TE share of team targets on each historical NTED versus corresponding Sunday baseline, with each season as a connected dot or small multiple.
2.  **Individual surprises:** Dumbbell plot of standout TEs' NTED yardage versus their own season averages (filter or annotate tiny-sample cases).
3.  **Position allocation:** TE/WR/RB target-share comparisons, normal Sunday versus NTED.
4.  **Team leaderboard:** Biggest team TE target-share changes, ideally labeled as percentage-point differences.
5.  **Optional pregame graphic:** 2026 projected above-baseline TE performers, with interval/uncertainty where defensible.

## Story and article outline

- **Hook:** The NFL has a holiday for tight ends. Do the statistics show anything unusual about that Sunday?
- **Definition:** Explain what NTED is and why 2019–2025 is the historical sample.
- **First reveal:** League/position-group picture — usage versus performance.
- **Second reveal:** Player-level examples, including George Kittle if interesting.
- **Third reveal:** Team-wide TE usage, showing why backups and secondary TEs matter.
- **Reality check:** Effects, counterexamples, bye weeks, volatility and limitations.
- **2026 forecast:** Optional prediction of biggest above-baseline performers, clearly time-stamped.
- **Conclusion:** Answer the overarching question in measured language, not a manufactured definitive yes/no.

**Authoring workflow:** Analyst supplies spoken reactions or notes to the charts; use these words as the basis for polished editing, without inventing quotes, claims, or results.

## Repository layout recommendation

Keep the existing project-by-project structure. Add a folder for this investigation, and a separate lightweight Quarto site root for publication. A project can generate an article, short video and post without duplicating its source files or treating these as mutually exclusive project categories.

``` text
statigami/
├── README.md
├── national-tight-ends-day/
│   ├── PLAN.md              # this document
│   ├── README.md            # concise project overview
│   ├── analysis.R            # main analysis
│   ├── data/                 # curated small derived data, if appropriate
│   └── figures/              # charts used by article and social content
├── nfl-championship-trajectories/
├── sacrifice-bunt-analysis/
├── ...                      # existing investigations stay where they are
└── website/                 # future Quarto site, not required for first analysis
    ├── _quarto.yml
    ├── index.qmd
    ├── articles/
    │   └── national-tight-ends-day.qmd
    └── assets/
        ├── logo.*
        └── styles.scss
```

The article should **reference or copy generated figures at render time** rather than duplicate analytical source code and data. For GitHub Pages, configure the Quarto project and output directory/publishing workflow when ready; nested `website/` is viable, but its site build/deploy location must be set correctly (GitHub Pages Actions or a suitable publishing workflow). Keep the repository README as the developer/project index and the Quarto homepage as the public storytelling front door.

**Navigation at launch:** Home \| Articles \| About. Do not create empty sports/topic sections yet; add categories or listing filters when enough articles exist. The site can later support NFL, baseball, sports, and non-sports topics using Quarto categories/tags rather than rigid folders.

**Brand:** Reuse existing Statigami colors, logo and visual motifs after confirming the actual brand files. Avoid inventing colors from memory.

## Questions to resolve after the first data audit

1.  Does the weekly player data accurately capture zero-target TEs, or should play-by-play/participation data supplement it?
    - **Answer (data audit):** No. A weekly stats row exists only when a player records some stat, so \~22–27% of TE games with offensive snaps have no row at all. Snap counts (`load_snap_counts`) now supply participation, and season roster positions replace the static `position` label.
2.  Is TE **target share** or **production above baseline** the strongest single headline result?
3.  For the 2026 preview, is a team-unit ranking more stable and interpretable than an individual-TE ranking?
4.  Should the first article be historical-only, with a separate 2026 forecast/social post, if time is limited?

## Sources

- Original issue: https://github.com/jackson-justice/statigami/issues/2
- 49ers on the origin and fourth Sunday: https://www.49ers.com/news/george-kittle-national-tight-end-day
- Pro Football Hall of Fame (2019 first official celebration): https://www.profootballhof.com/news/celebrating-hall-of-fame-tight-ends-on-national-tight-ends-day
- nflreadr player stats: https://nflreadr.nflverse.com/reference/load_player_stats.html
- nflreadr play-by-play: https://nflreadr.nflverse.com/reference/load_pbp.html