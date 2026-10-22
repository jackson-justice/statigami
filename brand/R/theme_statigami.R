# =============================================================================
# Statigami brand for R / ggplot2
#
# Colors are read from statigami-ds/colors_and_type.css (the design system's
# single source of truth), so no hex values are duplicated here. Fonts are
# registered from the bundled IBM Plex TTFs, so nothing needs installing.
#
# Usage (from any script; see brand/README.md):
#   source(file.path(Sys.getenv("STATIGAMI_BRAND_DIR", "brand"), "R", "theme_statigami.R"))
#   ggplot(...) + theme_statigami()
#   save_statigami(p, "figures/chart.png", format = "article")
#
# Requires: ggplot2, systemfonts, ragg
# =============================================================================


# -----------------------------------------------------------------------------
# Locate the brand folder
# -----------------------------------------------------------------------------
# 1. STATIGAMI_BRAND_DIR environment variable (set in ~/.Renviron), else
# 2. the nearest "brand/" folder at or above the working directory.

statigami_brand_dir <- function() {
  env_dir <- Sys.getenv("STATIGAMI_BRAND_DIR")
  if (nzchar(env_dir)) {
    return(normalizePath(env_dir, winslash = "/", mustWork = TRUE))
  }

  dir <- normalizePath(getwd(), winslash = "/")
  repeat {
    candidate <- file.path(dir, "brand")
    if (dir.exists(file.path(candidate, "statigami-ds"))) return(candidate)
    parent <- dirname(dir)
    if (parent == dir) {
      stop("Statigami brand folder not found. Set STATIGAMI_BRAND_DIR or ",
           "run from inside the statigami repo.", call. = FALSE)
    }
    dir <- parent
  }
}

statigami_ds_path <- function(...) {
  file.path(statigami_brand_dir(), "statigami-ds", ...)
}


# -----------------------------------------------------------------------------
# Colors (parsed from colors_and_type.css)
# -----------------------------------------------------------------------------

statigami_colors <- function() {
  css <- readLines(statigami_ds_path("colors_and_type.css"), warn = FALSE)
  css <- paste(css, collapse = "\n")
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)

  decls <- regmatches(css, gregexpr("--sg-[a-z0-9-]+\\s*:\\s*[^;]+;", css))[[1]]
  names <- sub("^--(sg-[a-z0-9-]+).*", "\\1", decls)
  values <- trimws(sub("^--sg-[a-z0-9-]+\\s*:\\s*([^;]+);", "\\1", decls))
  tokens <- setNames(values, names)

  # Resolve var(--sg-x) references (one level is all the CSS uses)
  refs <- grepl("^var\\(--sg-[a-z0-9-]+\\)$", tokens)
  tokens[refs] <- tokens[sub("^var\\(--(sg-[a-z0-9-]+)\\)$", "\\1", tokens[refs])]

  to_hex <- function(x) {
    if (grepl("^#[0-9A-Fa-f]{6}$", x)) return(toupper(x))
    if (grepl("^rgba?\\(", x)) {
      parts <- as.numeric(strsplit(gsub("rgba?\\(|\\)|\\s", "", x), ",")[[1]])
      alpha <- if (length(parts) == 4) parts[4] else 1
      return(grDevices::rgb(parts[1], parts[2], parts[3], alpha * 255, maxColorValue = 255))
    }
    NA_character_
  }

  colors <- vapply(tokens, to_hex, character(1))
  colors <- colors[!is.na(colors)]
  # sg-fg-on-ink-2 -> fg_on_ink_2
  names(colors) <- gsub("-", "_", sub("^sg-", "", names(colors)))
  colors
}

sg <- statigami_colors()

# The heat ramp is sequential (cool -> hot), not a categorical palette.
statigami_heat <- function(n = 3) {
  ramp <- unname(sg[c("cobalt", "magenta", "amber")])
  if (n == 3) return(ramp)
  grDevices::colorRampPalette(ramp)(n)
}


# -----------------------------------------------------------------------------
# Fonts (registered from the bundled TTFs; "bold" maps to the brand weights)
# -----------------------------------------------------------------------------
# Plex Sans: bold face = SemiBold 600, the brand's display weight.
# Plex Mono: bold face = Medium 500, the brand's tag/eyebrow weight.

statigami_register_fonts <- function() {
  registered <- systemfonts::registry_fonts()$family
  sans <- statigami_ds_path("IBM_Plex_Sans", "static")
  mono <- statigami_ds_path("IBM_Plex_Mono")

  if (!"Statigami Sans" %in% registered) {
    systemfonts::register_font(
      "Statigami Sans",
      plain = file.path(sans, "IBMPlexSans-Regular.ttf"),
      bold = file.path(sans, "IBMPlexSans-SemiBold.ttf"),
      italic = file.path(sans, "IBMPlexSans-Italic.ttf"),
      bolditalic = file.path(sans, "IBMPlexSans-SemiBoldItalic.ttf")
    )
  }
  if (!"Statigami Mono" %in% registered) {
    systemfonts::register_font(
      "Statigami Mono",
      plain = file.path(mono, "IBMPlexMono-Regular.ttf"),
      bold = file.path(mono, "IBMPlexMono-Medium.ttf"),
      italic = file.path(mono, "IBMPlexMono-Italic.ttf"),
      bolditalic = file.path(mono, "IBMPlexMono-MediumItalic.ttf")
    )
  }
  invisible(c(sans = "Statigami Sans", mono = "Statigami Mono"))
}

statigami_register_fonts()
statigami_sans <- "Statigami Sans"
statigami_mono <- "Statigami Mono"


# -----------------------------------------------------------------------------
# Theme
# -----------------------------------------------------------------------------
# Two surfaces only: paper (default) and ink. The faint gridline is the
# brand's coordinate-grid motif. Sans for ideas (titles), mono for metadata
# (axis labels, legend, caption).

statigami_surface_colors <- function(surface = c("paper", "ink")) {
  surface <- match.arg(surface)
  if (surface == "paper") {
    c(bg = sg[["paper"]], fg_1 = sg[["fg_1"]], fg_2 = sg[["fg_2"]],
      fg_3 = sg[["fg_3"]], grid = sg[["grid"]], rule = sg[["rule"]])
  } else {
    c(bg = sg[["ink"]], fg_1 = sg[["fg_on_ink"]], fg_2 = sg[["fg_on_ink_2"]],
      fg_3 = sg[["fg_on_ink_3"]], grid = sg[["grid_ink"]], rule = sg[["rule_ink"]])
  }
}

theme_statigami <- function(surface = c("paper", "ink"), base_size = 12,
                            grid = c("both", "y", "x", "none")) {
  grid <- match.arg(grid)
  s <- statigami_surface_colors(surface)
  grid_line <- ggplot2::element_line(color = s[["grid"]], linewidth = 0.4)

  ggplot2::theme_minimal(base_size = base_size, base_family = statigami_sans) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = s[["bg"]], color = NA),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("both", "x")) grid_line else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("both", "y")) grid_line else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(
        family = statigami_sans, face = "bold", size = base_size * 1.6,
        color = s[["fg_1"]], margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        family = statigami_sans, size = base_size * 1.05, color = s[["fg_2"]],
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        family = statigami_mono, size = base_size * 0.7, color = s[["fg_3"]],
        hjust = 0, margin = ggplot2::margin(t = 12)
      ),

      axis.text = ggplot2::element_text(
        family = statigami_mono, size = base_size * 0.75, color = s[["fg_2"]]
      ),
      axis.title = ggplot2::element_text(
        family = statigami_mono, size = base_size * 0.75, color = s[["fg_2"]]
      ),
      strip.text = ggplot2::element_text(
        family = statigami_sans, face = "bold", size = base_size,
        color = s[["fg_1"]], hjust = 0, margin = ggplot2::margin(b = 6)
      ),

      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = statigami_mono, size = base_size * 0.75, color = s[["fg_2"]]
      ),

      plot.margin = ggplot2::margin(24, 24, 16, 24)
    )
}


# -----------------------------------------------------------------------------
# Color scales
# -----------------------------------------------------------------------------
# Continuous values and *ordered* categories use the heat ramp (cool -> hot).
# Unordered categories: highlight one with amber or magenta and draw the rest
# in sg[["fg_3"]] rather than cycling brand colors.
#
# Contrast: amber on paper (1.7:1) and cobalt on ink (2.1:1) are faint as
# small marks. Give those points an outline (shape = 21 with an ink or paper
# stroke) or label them directly.

scale_color_statigami_c <- function(...) {
  ggplot2::scale_color_gradientn(colours = statigami_heat(3), ...)
}

scale_fill_statigami_c <- function(...) {
  ggplot2::scale_fill_gradientn(colours = statigami_heat(3), ...)
}

scale_color_statigami_d <- function(...) {
  ggplot2::discrete_scale("colour", palette = statigami_heat, ...)
}

scale_fill_statigami_d <- function(...) {
  ggplot2::discrete_scale("fill", palette = statigami_heat, ...)
}


# -----------------------------------------------------------------------------
# Saving
# -----------------------------------------------------------------------------
# ragg renders the registered Plex fonts. Formats cover articles and social.

statigami_formats <- list(
  article       = list(width = 8,    height = 5,    units = "in", dpi = 200),
  article_tall  = list(width = 8,    height = 9,    units = "in", dpi = 200),
  social_square = list(width = 1080, height = 1080, units = "px", dpi = 150),
  social_4x5    = list(width = 1080, height = 1350, units = "px", dpi = 150),
  social_wide   = list(width = 1920, height = 1080, units = "px", dpi = 200)
)

# Every saved chart carries the brand signature in the bottom-right corner:
# the mark plus the "./STATIGAMI" mono path label. Set logo = FALSE to skip.
# width/height override the format's size (same units as the format).

save_statigami <- function(plot, filename, format = names(statigami_formats),
                           surface = c("paper", "ink"), logo = TRUE,
                           width = NULL, height = NULL) {
  format <- match.arg(format)
  surface <- match.arg(surface)
  spec <- statigami_formats[[format]]
  if (!is.null(width)) spec$width <- width
  if (!is.null(height)) spec$height <- height
  bg <- statigami_surface_colors(surface)[["bg"]]

  ragg::agg_png(filename, width = spec$width, height = spec$height,
                units = spec$units, res = spec$dpi, background = bg)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  if (logo) grid::grid.draw(statigami_signature_grob(surface))
  invisible(filename)
}


# -----------------------------------------------------------------------------
# Logos
# -----------------------------------------------------------------------------

statigami_logo <- function(variant = c("mark", "mark-circle", "wordmark", "socials-png")) {
  variant <- match.arg(variant)
  if (variant == "socials-png") return(statigami_ds_path("statigami-socials-logo.png"))
  statigami_ds_path("assets", paste0("statigami-", variant, ".svg"))
}

# The mark as a raster grob. Its grid and connector use currentColor, which
# is set to the surface's text color so the mark works on paper and ink.
statigami_mark_grob <- function(surface = c("paper", "ink"), px = 240, ...) {
  surface <- match.arg(surface)
  fg <- substr(statigami_surface_colors(surface)[["fg_1"]], 1, 7)
  svg <- paste(readLines(statigami_logo("mark"), warn = FALSE), collapse = "\n")
  svg <- gsub("currentColor", fg, svg, fixed = TRUE)
  # Crop the built-in padding so the mark reads at small sizes
  svg <- sub('viewBox="0 0 100 100"', 'viewBox="12 12 76 76"', svg, fixed = TRUE)
  img <- rsvg::rsvg(charToRaw(svg), width = px, height = px)
  grid::rasterGrob(img, interpolate = TRUE, ...)
}

# Mark + "./STATIGAMI" label, anchored to the bottom-right corner of the
# image, on the same line as the (left-aligned) caption.
statigami_signature_grob <- function(surface = c("paper", "ink"),
                                     mark_size = grid::unit(0.36, "in"),
                                     margin = grid::unit(c(0.24, 0.12), "in")) {
  surface <- match.arg(surface)
  s <- statigami_surface_colors(surface)
  right <- grid::unit(1, "npc") - margin[1]
  bottom <- margin[2]

  label <- grid::textGrob(
    "./STATIGAMI",
    x = right, y = bottom + 0.5 * mark_size, just = c("right", "centre"),
    gp = grid::gpar(fontfamily = statigami_mono, fontsize = 9,
                    col = s[["fg_2"]])
  )
  mark <- statigami_mark_grob(
    surface,
    x = right - grid::grobWidth(label) - grid::unit(0.06, "in"),
    y = bottom, width = mark_size, height = mark_size,
    just = c("right", "bottom")
  )
  grid::grobTree(mark, label)
}
