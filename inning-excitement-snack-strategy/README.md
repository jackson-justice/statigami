# Best and Worst Times to Leave for Snacks

Using inning-level data to determine when it's safe to leave your seat during a game.

Salt Lake Bees • April 17, 2026

---

## Question

When are the best and worst times to leave your seat during a baseball game?

---

## Data

* Play-by-play data from MLB API 
* Salt Lake Bees home games (2025–2026)
* Metrics derived from events within each inning

---

## Method

Built an **excitement score** for each inning based on events:

* Positive events: runs, hits, extra-base hits, home runs
* Neutral events: walks, hit by pitch
* Negative events: strikeouts, double plays

Each event was weighted and summed to create an overall score per inning.

Innings were then grouped into:

* **Do Not Leave** (high excitement)
* **Risky**
* **Safe** (low excitement)

---

## Result

* The most dangerous times to leave were concentrated in high-scoring or high-impact innings
* Some innings consistently showed low activity, making them safer for breaks
* Historical averages helped identify patterns beyond a single game

---

## Takeaway

Not all innings are equal. Timing matters.

---

## Tools

* R (tidyverse, baseballr, ggplot2)
* MLB Stats API

---
