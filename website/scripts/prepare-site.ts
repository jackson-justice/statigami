// =============================================================================
// Prepare the Statigami site before rendering (Quarto pre-render script)
//
// 1. Copies each project's generated figures into articles/figures/<project>/
//    so articles reference figures without duplicating them in git.
// 2. Builds assets/brand/ (generated, git-ignored) from the brand folder:
//    _brand.scss (the design tokens as CSS custom properties, plus the few
//    Bootstrap defaults that need literal values), mark.svg (navbar logo)
//    and mark-circle.svg (favicon and home page avatar).
//    Without the brand folder, neutral stand-ins are written so the site
//    still renders.
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

// Every `--sg-*` color and font token in the design system's :root block
const readTokens = async (path: string): Promise<Record<string, string>> => {
  const css = await Deno.readTextFile(path);
  const root = css.match(/:root\s*\{([\s\S]*?)\n\}/)?.[1] ?? "";
  const tokens: Record<string, string> = {};
  for (const match of root.matchAll(/--sg-([a-z0-9-]+):\s*([^;]+);/g)) {
    tokens[match[1]] = match[2].trim();
  }
  return tokens;
};

// Neutral stand-ins for the tokens the site styles use, so the site still
// renders (unbranded) without the brand folder
const neutralTokens: Record<string, string> = {
  "ink": "#1a1a1a", "ink-2": "#262626", "ink-3": "#3a3a3a",
  "paper": "#f5f5f5", "paper-2": "#e8e8e8", "paper-3": "#d4d4d4",
  "cobalt": "#5b6b8c", "magenta": "#a0a0a0", "amber": "#d0d0d0",
  "fg-1": "#1a1a1a", "fg-2": "rgba(26, 26, 26, 0.65)", "fg-3": "rgba(26, 26, 26, 0.45)",
  "fg-on-ink": "#f5f5f5", "fg-on-ink-2": "rgba(245, 245, 245, 0.65)",
  "fg-on-ink-3": "rgba(245, 245, 245, 0.40)",
  "rule": "rgba(26, 26, 26, 0.12)", "rule-ink": "rgba(245, 245, 245, 0.14)",
  "sans": "system-ui, sans-serif", "mono": "ui-monospace, monospace",
  "shadow-lift": "0 12px 40px rgba(0, 0, 0, .3)",
  "ease": "cubic-bezier(.2, .7, .3, 1)",
};

const hasBrand = await exists(tokensCss);
const t = hasBrand ? await readTokens(tokensCss) : neutralTokens;

// Logo: the bare mark with its grid and connector drawn in paper (it uses
// currentColor, which an <img> can't inherit), cropped to the mark so it
// fills its box in the navbar
const logo = hasBrand
  ? (await Deno.readTextFile(`${brandDir}/statigami-ds/assets/statigami-mark.svg`))
    .replaceAll("currentColor", t["paper"])
    .replace('viewBox="0 0 100 100"', 'viewBox="12 12 76 76"')
  : '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10"></svg>\n';
// Also as a CSS token, since stylesheet url() paths can't point into the site
const logoUri = `data:image/svg+xml;base64,${btoa(logo)}`;

// Bootstrap defaults need literal values; everything else in
// styles/statigami.scss reads the tokens as CSS custom properties
const scss = [
  "/*-- scss:defaults --*/",
  hasBrand
    ? "@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;0,700;1,500;1,600&display=swap');"
    : "",
  `$body-bg: ${t["ink"]};`,
  `$body-color: ${t["paper"]};`,
  `$link-color: ${t["paper"]};`,
  `$headings-color: ${t["paper"]};`,
  `$navbar-bg: ${t["ink"]};`,
  `$navbar-fg: ${t["paper"]};`,
  `$footer-bg: ${t["ink"]};`,
  `$border-color: ${t["rule-ink"]};`,
  `$code-color: ${t["paper"]};`,
  `$font-family-sans-serif: ${t["sans"]};`,
  `$font-family-monospace: ${t["mono"]};`,
  "$headings-font-weight: 600;",
  "$border-radius: 2px;",
  "",
  "/*-- scss:rules --*/",
  ":root {",
  ...Object.entries(t).map(([name, value]) => `  --sg-${name}: ${value};`),
  `  --sg-logo: url("${logoUri}");`,
  "}",
];
await Deno.writeTextFile(`${outDir}/_brand.scss`, scss.join("\n") + "\n");
await Deno.writeTextFile(`${outDir}/mark.svg`, logo);

// The circle mark (favicon and home page avatar), without its ./STATIGAMI label
if (hasBrand) {
  const circle = await Deno.readTextFile(`${brandDir}/statigami-ds/assets/statigami-mark-circle.svg`);
  await Deno.writeTextFile(`${outDir}/mark-circle.svg`, circle.replace(/\s*<text[\s\S]*?<\/text>/, ""));
  console.log(`Brand styles built from ${tokensCss}`);
} else {
  await Deno.writeTextFile(`${outDir}/mark-circle.svg`, logo);
  console.log("Brand folder not found; using neutral site styles");
}
