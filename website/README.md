# Statigami website

Quarto site for Statigami articles.

## Build

From the repo root:

``` bash
quarto preview website   # live preview while writing
quarto render website    # full build into website/_site/
```

Quarto ships with Positron and RStudio; on Positron's terminal, `quarto` may need its full path if it isn't on your PATH.

## What happens on every render

`scripts/prepare-site.ts` runs first (Quarto pre-render) and generates three git-ignored things, so none of them need to be copied between computers:

- `articles/figures/<project>/`: copied from each project's `figures/`. Re-run the project's `analysis.R` first if the figures changed.
- `assets/brand/`: the design tokens (as CSS custom properties), navbar logo and favicon, built from the repo's `brand/` folder (or `STATIGAMI_BRAND_DIR`).
- `_site/`: the rendered site.

If the build prints **"Brand folder not found; using neutral site styles"**, the brand files are missing (or `STATIGAMI_BRAND_DIR` points somewhere wrong). The site still renders, just without brand styling and logo.

## How the site is put together

| File | Role |
|----|----|
| `styles/statigami.scss` | All site styles. Colors and fonts come only from the `--sg-*` tokens. |
| `partials/title-block.html` | The title banner: kicker, title, dek, byline and facts strip. |
| `partials/after-body.html` | The reading-progress bar on articles. |
| `listings/home.ejs` | Home page: the latest article as a featured card, its key stat, older articles as tiles. |
| `listings/archive.ejs` | The Articles page list. |
| `articles/_metadata.yml` | Turns on the article layout for every article. |

## Adding an article

1.  Add the project folder name to `articleProjects` in `scripts/prepare-site.ts`.
2.  Create `articles/<project>.qmd` and reference figures as `figures/<project>/<file>.png`.
3.  Optional front matter the design uses (see the NTED article for an example):
    - `title`: wrap one word in `*asterisks*` for the amber accent.
    - `image`: the chart shown on the home page card and in link previews.
    - `facts`: a list of `label`/`value` pairs for the facts strip under the title.
    - `key-stat`: `value`, `unit`, `headline`, `body` and `source` for the big number on the home page (shown while the article is the latest).
    - A `::: sg-answer` block for the "short answer" panel.