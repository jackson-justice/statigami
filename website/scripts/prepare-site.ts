// =============================================================================
// Prepare the Statigami site before rendering (Quarto pre-render script)
//
// 1. Copies each project's generated figures into articles/figures/<project>/
//    so articles reference figures without duplicating them in git.
// 2. Builds assets/brand/ (generated, git-ignored) from the brand folder:
//    _brand.scss (a handful of design tokens) and logo.svg. Without the
//    brand folder, a neutral fallback is written so the site still renders.
//
// Runs from the website/ directory with Quarto's bundled Deno, so building
// the site needs nothing installed beyond Quarto.
// =============================================================================

const exists = async (path: string): Promise<boolean> => {
  try {
    await Deno.stat(path);
    return true;
  } catch {
    return false;
  }
};

// -----------------------------------------------------------------------------
// 1. Figures
// -----------------------------------------------------------------------------

const articleProjects = ["national-tight-ends-day"];

for (const project of articleProjects) {
  const from = `../${project}/figures`;
  const to = `articles/figures/${project}`;
  await Deno.mkdir(to, { recursive: true });

  let copied = 0;
  for await (const entry of Deno.readDir(from)) {
    if (entry.isFile && entry.name.endsWith(".png")) {
      await Deno.copyFile(`${from}/${entry.name}`, `${to}/${entry.name}`);
      copied++;
    }
  }
  if (copied === 0) throw new Error(`No figures found in ${from}`);
}

// -----------------------------------------------------------------------------
// 2. Brand styles and logo
// -----------------------------------------------------------------------------

const brandDir = Deno.env.get("STATIGAMI_BRAND_DIR") || "../brand";
const tokensCss = `${brandDir}/statigami-ds/colors_and_type.css`;
const outDir = "assets/brand";
await Deno.mkdir(outDir, { recursive: true });

const readTokens = async (path: string): Promise<Record<string, string>> => {
  const css = await Deno.readTextFile(path);
  const tokens: Record<string, string> = {};
  for (const match of css.matchAll(/--sg-([a-z0-9-]+):\s*(#[0-9A-Fa-f]{6})/g)) {
    tokens[match[1]] = match[2];
  }
  return tokens;
};

let scss: string[];

if (await exists(tokensCss)) {
  const t = await readTokens(tokensCss);
  scss = [
    "/*-- scss:defaults --*/",
    "@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;600&family=IBM+Plex+Mono:wght@400;500&display=swap');",
    `$body-bg: ${t["paper"]};`,
    `$body-color: ${t["ink"]};`,
    `$link-color: ${t["cobalt"]};`,
    `$navbar-bg: ${t["ink"]};`,
    `$navbar-fg: ${t["paper"]};`,
    `$footer-bg: ${t["paper-2"]};`,
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
    "p { text-wrap: pretty; }",
  ];
  await Deno.copyFile(
    `${brandDir}/statigami-ds/assets/statigami-mark-circle.svg`,
    `${outDir}/logo.svg`,
  );
  console.log(`Brand styles built from ${tokensCss}`);
} else {
  scss = ["/*-- scss:defaults --*/", "", "/*-- scss:rules --*/"];
  await Deno.writeTextFile(
    `${outDir}/logo.svg`,
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"></svg>\n',
  );
  console.log("Brand folder not found; using neutral site styles");
}

await Deno.writeTextFile(`${outDir}/_brand.scss`, scss.join("\n") + "\n");
