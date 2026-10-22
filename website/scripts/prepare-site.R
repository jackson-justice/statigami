# =============================================================================
# Prepare the Statigami site before rendering (Quarto pre-render script)
#
# 1. Copies each project's generated figures into articles/figures/<project>/
#    so articles reference figures without duplicating them in git.
# 2. Builds assets/brand/ (generated, git-ignored) from the brand folder:
#    _brand.scss (a handful of design tokens) and logo.svg. Without the
#    brand folder, a neutral fallback is written so the site still renders.
#
# Runs from the website/ directory. Base R only.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Figures
# -----------------------------------------------------------------------------

article_projects <- c("national-tight-ends-day")

for (project in article_projects) {
  from <- file.path("..", project, "figures")
  to <- file.path("articles", "figures", project)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  pngs <- list.files(from, pattern = "\\.png$", full.names = TRUE)
  if (length(pngs) == 0) stop("No figures found in ", from, call. = FALSE)
  file.copy(pngs, to, overwrite = TRUE)
}

# -----------------------------------------------------------------------------
# 2. Brand styles and logo
# -----------------------------------------------------------------------------

brand_dir <- Sys.getenv("STATIGAMI_BRAND_DIR", file.path("..", "brand"))
tokens_css <- file.path(brand_dir, "statigami-ds", "colors_and_type.css")
out_dir <- file.path("assets", "brand")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_tokens <- function(path) {
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hex <- regmatches(css, gregexpr("--sg-[a-z0-9-]+:\\s*#[0-9A-Fa-f]{6}", css))[[1]]
  setNames(sub(".*(#[0-9A-Fa-f]{6})$", "\\1", hex),
           sub("^--sg-([a-z0-9-]+):.*", "\\1", hex))
}

if (file.exists(tokens_css)) {
  t <- read_tokens(tokens_css)
  scss <- c(
    "/*-- scss:defaults --*/",
    "@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;600&family=IBM+Plex+Mono:wght@400;500&display=swap');",
    sprintf("$body-bg: %s;", t[["paper"]]),
    sprintf("$body-color: %s;", t[["ink"]]),
    sprintf("$link-color: %s;", t[["cobalt"]]),
    sprintf("$navbar-bg: %s;", t[["ink"]]),
    sprintf("$navbar-fg: %s;", t[["paper"]]),
    sprintf("$footer-bg: %s;", t[["paper-2"]]),
    "$font-family-sans-serif: 'IBM Plex Sans', system-ui, sans-serif;",
    "$font-family-monospace: 'IBM Plex Mono', ui-monospace, monospace;",
    "$headings-font-weight: 600;",
    "$border-radius: 2px;",
    "",
    "/*-- scss:rules --*/",
    "// Metadata is mono (brand rule: mono for facts about facts)",
    ".quarto-title-meta, figcaption, .listing-date, .listing-categories,",
    ".quarto-category, .table { font-family: $font-family-monospace; }",
    "h1, h2, h3 { letter-spacing: -0.5px; text-wrap: balance; }",
    "p { text-wrap: pretty; }"
  )
  file.copy(file.path(brand_dir, "statigami-ds", "assets", "statigami-mark-circle.svg"),
            file.path(out_dir, "logo.svg"), overwrite = TRUE)
  message("Brand styles built from ", tokens_css)
} else {
  scss <- c("/*-- scss:defaults --*/", "", "/*-- scss:rules --*/")
  writeLines(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"></svg>',
    file.path(out_dir, "logo.svg")
  )
  message("Brand folder not found; using neutral site styles")
}

writeLines(scss, file.path(out_dir, "_brand.scss"))
