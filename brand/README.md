# Statigami brand — how projects use it

The parts of the brand that charts and the website need are committed to the repo, so a fresh clone builds branded output on any computer. **The Statigami name, logo and visual identity are not MIT-licensed**; see [LICENSE.md](LICENSE.md).

``` text
brand/
├── README.md                  this file
├── LICENSE.md                 brand assets: all rights reserved
├── R/
│   └── theme_statigami.R      ggplot2 theme, scales, fonts, save helper
└── statigami-ds/              the original design system (do not edit here)
    ├── colors_and_type.css    design tokens (source of truth for colors)
    ├── assets/                logo SVGs (mark, mark-circle, wordmark)
    ├── statigami-socials-logo.png
    ├── IBM_Plex_Sans/  IBM_Plex_Mono/   fonts (OFL)
    │
    │   local only (git-ignored; not used by any build):
    ├── README.md  SETUP.md  SKILL.md    brand bible and Claude Design notes
    ├── preview/  ui_kits/  slides/      reference designs and templates
    └── Plex Sans Condensed + variable font files
```

`statigami-ds/` is kept exactly as delivered. `theme_statigami.R` reads its colors from `colors_and_type.css` and its fonts from the bundled TTFs, so a token change there flows into every chart with no second copy of any value.

------------------------------------------------------------------------

## Finding the folder (portable paths)

Projects never hard-code a computer-specific path. They look for the brand in this order:

1.  The `STATIGAMI_BRAND_DIR` environment variable, if set.
2.  Otherwise, `brand/` relative to the repo root (where scripts run from).

Step 2 is the normal case: clone the repo and run scripts from the root. Only set the variable if you keep a brand folder somewhere else. Add one line to your **user** `.Renviron` (open it with `usethis::edit_r_environ()`, or edit `~/.Renviron` directly), then restart R:

``` text
STATIGAMI_BRAND_DIR=C:/Users/<you>/somewhere/brand
```

Use forward slashes on Windows.

### On a new computer

`git clone` brings everything the builds need. The local-only design-system files (brand bible, UI kit, slide templates, previews) are not in the repo; copy them over separately only if you need them.

------------------------------------------------------------------------

## R / ggplot2

Requires `ggplot2`, `systemfonts`, `ragg` and `rsvg` (rsvg renders the logo; install it once per computer with `install.packages("rsvg")`). The Plex fonts are registered from this folder, so nothing needs installing system-wide.

``` r
source(file.path(Sys.getenv("STATIGAMI_BRAND_DIR", "brand"), "R", "theme_statigami.R"))

p <- ggplot(df, aes(x, y)) +
  geom_point(color = sg[["fg_3"]]) +
  theme_statigami()                    # or theme_statigami("ink")

save_statigami(p, "figures/chart.png", format = "article")
```

What the file provides:

| Object | Purpose |
|------------------------------------|------------------------------------|
| `theme_statigami(surface, base_size, grid)` | Paper (default) or ink surface; faint coordinate grid; Plex Sans titles, Plex Mono axis/legend/caption |
| `sg` | Named brand colors parsed from the CSS: `sg[["cobalt"]]`, `sg[["paper"]]`, `sg[["fg_3"]]`, … |
| `statigami_heat(n)` | The cobalt → magenta → amber ramp, interpolated to `n` colors |
| `scale_color_statigami_c()` / `scale_fill_statigami_c()` | Continuous heat scale |
| `scale_color_statigami_d()` / `scale_fill_statigami_d()` | Heat ramp for **ordered** categories only |
| `save_statigami(plot, file, format, surface, logo, width, height)` | Saves via ragg so the Plex fonts render, and adds the brand signature (mark + `./STATIGAMI`) bottom-right; `logo = FALSE` skips it; `width`/`height` override the format size |
| `statigami_logo(variant)` | Path to `mark`, `mark-circle`, `wordmark` SVG or `socials-png` |

`save_statigami()` formats: `article` (8×5 in), `article_tall` (8×9 in), `social_square` (1080×1080), `social_4x5` (1080×1350), `social_wide` (1920×1080).

### Color rules for charts

From the brand bible, plus contrast checks run against the two surfaces:

- **The heat ramp is sequential** (cool → hot), not a set of category colors. Use it for magnitude or ordered groups. Cobalt = baseline, amber = the extreme/hero.
- **Unordered categories:** highlight the one that matters (amber, or magenta) and draw everything else in `sg[["fg_3"]]`.
- **Amber is the hero.** Use it once per chart.
- **Contrast:** amber on paper (1.7:1) and cobalt on ink (2.1:1) are faint as small marks. Give those points an outline (`shape = 21` with an `sg[["fg_1"]]` stroke on paper) or label them directly.
- Captions are metadata, so they are mono and may use `//` tags: `"// SOURCE · nflverse · N=487"`. No emoji.

### Fallback when the brand is missing

Scripts still check for the brand file and fall back to a neutral theme if it is missing (for example, if `STATIGAMI_BRAND_DIR` points somewhere wrong):

``` r
brand_file <- file.path(Sys.getenv("STATIGAMI_BRAND_DIR", "brand"), "R", "theme_statigami.R")
has_brand <- file.exists(brand_file)
if (has_brand) source(brand_file)
# ... then use theme_statigami() if has_brand, else a neutral theme
```

------------------------------------------------------------------------

## Quarto website

- **Charts:** articles include the PNGs that analysis scripts save to each project's `figures/`; they do not re-run brand code.
- **Styles and logo:** `website/scripts/prepare-site.ts` builds `website/assets/brand/` (a few tokens as SCSS, plus the navbar logo) from this folder on every render. Don't edit those generated files.
- **Fonts:** the site loads IBM Plex Sans and Mono from Google Fonts. Do not copy the TTFs into `website/`.

------------------------------------------------------------------------

## Social content

- Use `save_statigami()` with `social_square`, `social_4x5` or `social_wide`.
- Prefer `theme_statigami("ink")` for social posts: ink is the brand's signature surface and amber reads strongly on it.
- Profile/avatar image: `statigami_logo("socials-png")` or the `mark-circle` SVG.
- Slide templates for video episodes: `statigami-ds/slides/index.html` (local only).