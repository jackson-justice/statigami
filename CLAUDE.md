# Statigami

Sports data projects (R) published as articles on a Quarto site (`website/`).
See `README.md` for the repo layout, `brand/README.md` for charts and brand
rules, and `website/README.md` for how the site is built.

## Making a new article

When asked to turn a project into an article, do all of these:

1. **Figures.** The project's `analysis.R` saves PNGs to `<project>/figures/`
   with `theme_statigami("ink")` and `save_statigami()` (see `brand/README.md`).
   Run it so the figures exist.
2. **Wire up figures.** Add the project folder name to `articleProjects` in
   `website/scripts/prepare-site.ts`.
3. **Write `website/articles/<project>.qmd`.** Copy the front matter shape of
   `website/articles/national-tight-ends-day.qmd`:
   - `title` with one accent word in `*asterisks*`, `subtitle`, `author`,
     `date`, `categories`, `description`
   - `image`: the main chart (home page card and link previews)
   - `facts`: 3–4 `label`/`value` pairs, ending with a one-line `Verdict`
   - `key-stat`: `value`, `unit`, `headline`, `body`, `source`. Use the
     article's central number, and take every figure from the analysis.
   - `draft: true` until the user says to publish
   - Open with a `::: sg-answer` block holding the short answer
   - Figures: `![caption](figures/<project>/<file>.png){fig-alt="..."}`
   - End with a collapsed `Data and methods` callout (data source and
     license, definitions, uncertainty, limitations, link to the code)
4. **Check it.** Render with Quarto (`quarto render website`, or Positron's
   bundled copy at `%LOCALAPPDATA%/Programs/Positron/resources/app/quarto/bin/quarto.exe`)
   and look at the article page and the home page.
5. **Publish** only when the user asks: remove `draft: true`, add the
   project to the Projects list in the root `README.md`, commit and push.
   The GitHub Action publishes the site to GitHub Pages.

The writing is the user's. Draft in their first-person, conversational voice
(see the NTED article), but don't invent their opinions or anecdotes: ask, or
leave a clearly marked placeholder. No emoji.
