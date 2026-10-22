# When MLB Still Bunts

Moneyball taught baseball to stop giving away outs.

So why are MLB teams still sacrifice bunting?

This project started after a Salt Lake Bees game where a bunt with a runner on second and no outs directly led to a run scoring later in the inning. That raised a simple question:

> If modern baseball analytics killed the sacrifice bunt, when do teams still use it?

Using MLB play-by-play data from 2023–2026, this project analyzes when sacrifice bunts still appear in modern baseball and which situations lead to the highest chance of scoring later in the inning.

## Main Findings

- Sacrifice bunts are now extremely rare in MLB
- Most successful bunts occur with:
  - 0 outs
  - runners already in scoring position
- Bunts with 1 out are rarely successful
- Modern baseball did not completely eliminate bunting — it made teams far more selective about when to use it

## Data

- Source: MLB play-by-play data via `baseballr`
- Seasons analyzed:
  - 2023
  - 2024
  - 2025
  - 2026

Over 350,000 MLB plate appearances were analyzed.

## Tools Used

- R
- tidyverse
- baseballr
- ggplot2
- Claude Design
- CapCut

## Statigami

Statigami is a sports analytics and storytelling project focused on finding interesting, unusual, or meaningful patterns in sports using data, visualization, and observational analysis.
