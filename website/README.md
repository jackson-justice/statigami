# Statigami website

Quarto site for Statigami articles.

## Build

From the repo root:

```bash
quarto preview website   # live preview while writing
quarto render website    # full build into website/_site/
```

Quarto ships with Positron and RStudio; on Positron's terminal, `quarto` may
need its full path if it isn't on your PATH.

## What happens on every render

`scripts/prepare-site.R` runs first (Quarto pre-render) and generates three
git-ignored things, so none of them need to be copied between computers:

- `articles/figures/<project>/`: copied from each project's `figures/`.
  Re-run the project's `analysis.R` first if the figures changed.
- `assets/brand/`: site styles and navbar logo, built from the repo's
  `brand/` folder (or `STATIGAMI_BRAND_DIR`).
- `_site/`: the rendered site.

If the build prints **"Brand folder not found; using neutral site styles"**,
the brand files are missing (or `STATIGAMI_BRAND_DIR` points somewhere
wrong). The site still renders, just without brand styling and logo.

## Adding an article

1. Add the project folder name to `article_projects` in
   `scripts/prepare-site.R`.
2. Create `articles/<project>.qmd` and reference figures as
   `figures/<project>/<file>.png`.
