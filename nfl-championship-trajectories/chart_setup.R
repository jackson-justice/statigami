# =============================================================================
# Shared chart setup for this project's figures
#
# Sourced by analysis.R (section 10) and graveyard_tiers.R (section 7).
# Expects `project_dir` to be set. Uses the Statigami brand (brand/, see
# brand/LICENSE.md) when it is available, and a neutral palette otherwise, so
# the public repo still runs. See brand/README.md for STATIGAMI_BRAND_DIR.
# =============================================================================

figures_dir <- file.path(project_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

tier_levels <- c("Championship Path", "Playoff Path", "Graveyard")

brand_file <- file.path(Sys.getenv("STATIGAMI_BRAND_DIR", "brand"), "R",
                        "theme_statigami.R")
has_brand <- file.exists(brand_file)

if (has_brand) {
  source(brand_file)
  # Ink surface. The tiers are ordered (cool -> hot), so they take the heat
  # ramp: cobalt = Championship Path (the ordinary state), magenta = Playoff
  # Path, amber = the Graveyard, the hero of every chart. Cobalt is faint on
  # ink (2.1:1), so cobalt marks always carry a direct label.
  chart_surface <- sg[["ink"]]
  chart_fg <- sg[["fg_on_ink"]]
  chart_fg_2 <- sg[["fg_on_ink_2"]]
  chart_fg_3 <- sg[["fg_on_ink_3"]]
  chart_grid <- sg[["grid_ink"]]
  chart_mono <- statigami_mono
  chart_sans <- statigami_sans
  tier_colors <- set_names(statigami_heat(3), tier_levels)
  theme_chart <- function(...) theme_statigami("ink", ...)
} else {
  chart_surface <- "#fcfcfb"
  chart_fg <- "#0b0b0b"
  chart_fg_2 <- "#52514e"
  chart_fg_3 <- "#898781"
  chart_grid <- "#e1e0d9"
  chart_mono <- "mono"
  chart_sans <- "sans"
  tier_colors <- set_names(c("#2a78d6", "#b5479a", "#d9891c"), tier_levels)
  theme_chart <- function(...) {
    theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = chart_surface, color = NA),
        panel.grid.major = element_line(color = chart_grid, linewidth = 0.3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = chart_fg, face = "bold"),
        plot.subtitle = element_text(color = chart_fg_2),
        plot.caption = element_text(color = chart_fg_2, hjust = 0),
        axis.text = element_text(color = chart_fg_2),
        axis.title = element_text(color = chart_fg_2),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(color = chart_fg_2)
      )
  }
}

# Text drawn on top of a tier's fill: light on cobalt, dark on magenta/amber
tier_text_color <- function(tier) {
  if_else(tier == tier_levels[1], chart_fg, chart_surface)
}

save_figure <- function(plot, name, format = "article", height = NULL) {
  path <- file.path(figures_dir, name)
  if (has_brand) {
    # Brand fonts via ragg, plus the Statigami signature in the corner
    save_statigami(plot, path, format = format, surface = "ink", height = height)
  } else {
    spec <- list(article = c(8, 5), article_tall = c(8, 9))[[format]]
    ggsave(path, plot, device = ragg::agg_png, width = spec[1],
           height = coalesce(height, spec[2]), units = "in", dpi = 200,
           bg = chart_surface)
  }
  invisible(path)
}

# The three tiers as shaded bands on a games-played x losses chart. `th` is
# one baseline's rows of loss_thresholds_by_games.csv (needs games_played,
# champion_threshold, playoff_threshold). Cells are centred on the integers,
# so each band runs from threshold + 0.5 to the next.
tier_bands <- function(th, y_max) {
  bind_rows(
    th |> transmute(games_played, tier = tier_levels[1],
                    ymin = -0.5, ymax = champion_threshold + 0.5),
    th |> transmute(games_played, tier = tier_levels[2],
                    ymin = champion_threshold + 0.5,
                    ymax = playoff_threshold + 0.5),
    th |> transmute(games_played, tier = tier_levels[3],
                    ymin = playoff_threshold + 0.5, ymax = y_max + 0.5)
  ) |>
    mutate(
      xmin = games_played - 0.5, xmax = games_played + 0.5,
      tier = factor(tier, levels = tier_levels)
    )
}

# A threshold as a staircase (for geom_step with direction = "hv")
tier_steps <- function(th, column) {
  th <- arrange(th, games_played)
  bind_rows(
    th |> transmute(x = games_played - 0.5, y = .data[[column]] + 0.5),
    th |> slice_tail(n = 1) |>
      transmute(x = games_played + 0.5, y = .data[[column]] + 0.5)
  )
}
