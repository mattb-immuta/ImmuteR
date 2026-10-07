#' Interactive PBAC adoption scatter plot
#'
#' Writes a self-contained HTML page plotting every account on the two axes the
#' PBAC rating is built from: seat consumption ratio on x, weighted complexity
#' score on y. The plot background is shaded with the rating each region of the
#' grid produces, so a point's colour band \emph{is} its
#' \code{pbac_adoption} rating — the chart and
#' \code{\link{op_list_pbac_adoption}} can't drift apart, because both read the
#' same thresholds.
#'
#' A summary panel to the right of the plot carries two blocks, matching the two
#' axes:
#' \itemize{
#'   \item \strong{Consumption} — registered users, licensed users, ratio.
#'   \item \strong{Complexity} — score, platforms in Production, and the
#'     feature signals earned (from \code{pbac_feature_labels()}, so the wording
#'     matches the account cards and health tiles).
#' }
#'
#' Hovering a point fills the panel; clicking pins it, so the summary stays put
#' while the cursor moves away — which a tooltip anchored to the dot can't do.
#' Points are drawn small, filled with their rating colour and outlined in ink,
#' so a pale amber dot reads as clearly against its own band as a dark one.
#'
#' Two presentation decisions worth knowing about, both of which only affect
#' where a dot is \emph{drawn} — never the tooltip, which always reports the
#' true figures:
#' \itemize{
#'   \item \strong{Outliers are pinned.} The ratio runs past 100 on a handful of
#'     accounts (unlimited-user contracts with a nominal licensed count), which
#'     would squash every other point against the y-axis. Anything above
#'     \code{ratio_max} is drawn in a pinned column at the right edge, marked
#'     with a heavier ring.
#'   \item \strong{Points are jittered.} \code{complexity_score} is an integer
#'     and most accounts sit on a handful of values, so a deterministic offset
#'     (seeded from the account name, therefore stable between runs) spreads
#'     co-located dots instead of hiding them behind one another.
#'   \item \strong{A missing ratio is drawn at zero}, but hollow, and the
#'     tooltip says "not available" rather than showing a figure. The rating
#'     logic treats an unknown ratio as "not yet demonstrated", not as zero —
#'     the hollow marker is the reminder that the dot's x position is a
#'     placeholder.
#' }
#'
#' The page is a single file with no external assets, so it opens from disk and
#' can be mailed as-is.
#'
#' @param op_list Named list from \code{enrich_opportunities()}. Needs
#'   \code{consumption$pbac_adoption}; uses \code{consumption$low_consumption}
#'   for user counts and platform names when present.
#' @param output_path Character. Where to write the HTML. Parent directories
#'   are created. Defaults to \code{"output/pbac_scatter.html"}.
#' @param ratio_max Numeric. Right-hand end of the x-axis; accounts above it are
#'   pinned to the edge. Default \code{1.25}.
#' @param score_max Numeric. Top of the y-axis. Default \code{7}, the maximum
#'   attainable \code{complexity_score}.
#' @param ratio_minimal,ratio_moderate,complexity_min Numeric. The rating
#'   thresholds used to shade the background. Defaults match
#'   \code{\link{op_list_pbac_adoption}} — pass the same values if you override
#'   them there, or the shading will disagree with the ratings.
#'
#' @return Invisibly returns \code{output_path}.
#' @examples
#' \dontrun{
#'   report_pbac_scatter(op_list)
#'   report_pbac_scatter(op_list, "output/pbac_scatter.html", ratio_max = 2)
#' }
#' @export
report_pbac_scatter <- function(op_list,
                                output_path    = "output/pbac_scatter.html",
                                ratio_max      = 1.25,
                                score_max      = 7,
                                ratio_minimal  = 0.25,
                                ratio_moderate = 0.75,
                                complexity_min = 4) {

  pbac <- op_list$consumption$pbac_adoption
  if (is.null(pbac) || !is.data.frame(pbac) || nrow(pbac) == 0) {
    stop("op_list$consumption$pbac_adoption is missing or empty — ",
         "run enrich_opportunities(looker = TRUE) first.")
  }

  lc <- op_list$consumption$low_consumption

  # Row lookup into low_consumption by trimmed name. The two frames are built
  # from the same account list but the Salesforce labels can carry stray
  # whitespace, so neither side is trusted to match exactly.
  lc_idx <- if (!is.null(lc) && is.data.frame(lc) && "Account Name" %in% names(lc)) {
    match(trimws(as.character(pbac$`Account Name`)),
          trimws(as.character(lc$`Account Name`)))
  } else {
    rep(NA_integer_, nrow(pbac))
  }

  .lc <- function(col) {
    if (is.null(lc) || !is.data.frame(lc) || !(col %in% names(lc))) {
      return(rep(NA, nrow(pbac)))
    }
    lc[[col]][lc_idx]
  }

  .lc_num <- function(col) {
    suppressWarnings(as.numeric(gsub(",", "", as.character(.lc(col)))))
  }

  # Production platforms, falling back to the combined string when the split
  # columns aren't there (an older snapshot), then to an em-dash.
  plat_prod <- as.character(.lc("platform_prod"))
  plat_html <- as.character(.lc("platform_html"))
  platforms <- ifelse(!is.na(plat_prod) & nzchar(plat_prod) & plat_prod != "—",
                      plat_prod,
                      ifelse(!is.na(plat_html) & nzchar(plat_html), plat_html, "—"))

  # Feature labels per row, reusing the shared helper so the wording here is
  # the same as on the account cards. Multi-platform is deliberately not in
  # that list — the Platforms line already says it.
  features <- vapply(seq_len(nrow(pbac)), function(i) {
    labs <- pbac_feature_labels(pbac[i, , drop = FALSE])
    if (length(labs) == 0) "None" else paste(labs, collapse = ", ")
  }, character(1))

  points <- data.frame(
    name       = as.character(pbac$`Account Name`),
    ratio      = suppressWarnings(as.numeric(pbac$consumption_ratio)),
    score      = suppressWarnings(as.numeric(pbac$complexity_score)),
    registered = .lc_num("Registered Users"),
    licensed   = .lc_num("Licensed Users"),
    platforms  = platforms,
    features   = features,
    colour     = unname(vapply(pbac$pbac_adoption, pbac_rating_colour, character(1))),
    rating     = unname(vapply(pbac$pbac_adoption, pbac_rating_label,  character(1))),
    # Accounts still in S1 (Installation and Config.) plot as squares: low
    # consumption there is expected, not a warning sign.
    install    = .pbac_install_stage(op_list$accounts, pbac$`Account Name`),
    stringsAsFactors = FALSE
  )
  points <- points[order(points$name), , drop = FALSE]

  cfg <- list(
    ratioMax      = ratio_max,
    scoreMax      = score_max,
    ratioMinimal  = ratio_minimal,
    ratioModerate = ratio_moderate,
    complexityMin = complexity_min,
    generated     = format(Sys.time(), "%B %d, %Y at %H:%M")
  )

  # Literal splice, not sub(): the JSON payload is full of backslash escapes
  # ("—", \"), which sub() would try to read as backreferences in the
  # replacement even with fixed = TRUE.
  .splice <- function(tpl, token, value) {
    parts <- strsplit(tpl, token, fixed = TRUE)[[1]]
    paste0(parts[1], value, parts[2])
  }

  html <- .pbac_scatter_template()
  html <- .splice(html, "__POINTS__",
                  as.character(jsonlite::toJSON(points, na = "null", auto_unbox = TRUE)))
  html <- .splice(html, "__CONFIG__",
                  as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE)))

  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  writeLines(html, output_path, useBytes = TRUE)

  message("PBAC scatter written to ", output_path,
          " (", nrow(points), " accounts)")
  invisible(output_path)
}


# The page itself. Kept as a template with __POINTS__ / __CONFIG__ placeholders
# rather than assembled with paste0(), so the HTML/CSS/JS reads as HTML/CSS/JS
# and isn't mangled by R string escaping. Self-contained by design: no CDN, no
# charting library — the plot is SVG built in a few dozen lines of JS, so the
# file opens from disk and survives being emailed.
#' @noRd
.pbac_scatter_template <- function() {
r"---(<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>PBAC Adoption — Consumption vs Complexity</title>
<style>
  :root {
    --bg: #f4f6f8;
    --surface: #ffffff;
    --ink: #161a21;
    --muted: #5b6675;
    --faint: #8792a3;
    --line: #dce1e8;
    --line-strong: #c3cbd6;
    --accent: #2d4c7e;

    --black: #343a44;  --black-bg: #e9ebef;
    --red:   #b04234;  --red-bg:   #fbeae6;
    --amber: #b47b1c;  --amber-bg: #fdf4e3;
    --green: #35704c;  --green-bg: #e8f3ed;
  }
  * { box-sizing: border-box; }
  body {
    margin: 0;
    background: var(--bg);
    color: var(--ink);
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    font-size: 14px;
    line-height: 1.5;
    -webkit-font-smoothing: antialiased;
  }
  .wrap { max-width: 1040px; margin: 0 auto; padding: 32px 20px 56px; }

  header { margin-bottom: 20px; }
  h1 { font-size: 21px; font-weight: 600; margin: 0 0 4px; letter-spacing: -0.01em; }
  .sub { color: var(--muted); margin: 0; font-size: 13px; }

  .panel {
    background: var(--surface);
    border: 1px solid var(--line);
    border-radius: 8px;
    padding: 18px 18px 10px;
  }

  .controls {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    gap: 8px;
    margin-bottom: 14px;
  }
  .pill {
    font: inherit;
    font-size: 12px;
    font-weight: 600;
    border: 1px solid var(--line-strong);
    background: var(--surface);
    color: var(--muted);
    border-radius: 999px;
    padding: 4px 12px;
    cursor: pointer;
    display: inline-flex;
    align-items: center;
    gap: 6px;
    transition: background .12s, border-color .12s, opacity .12s;
  }
  .pill .dot { width: 8px; height: 8px; border-radius: 50%; }
  .pill[aria-pressed="false"] { opacity: .4; }
  .pill:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
  .pill .n { font-variant-numeric: tabular-nums; color: var(--faint); font-weight: 500; }

  .search {
    font: inherit;
    font-size: 12px;
    border: 1px solid var(--line-strong);
    border-radius: 999px;
    padding: 5px 12px;
    min-width: 170px;
    background: var(--surface);
    color: var(--ink);
    margin-left: auto;
  }
  .search:focus-visible { outline: 2px solid var(--accent); outline-offset: 1px; }

  .plot-row { display: flex; gap: 16px; align-items: flex-start; }
  .plot-wrap { flex: 1 1 auto; min-width: 0; overflow-x: auto; }
  svg { display: block; width: 100%; min-width: 620px; height: auto; }

  /* ---- account summary panel ---- */
  .side {
    flex: 0 0 268px;
    align-self: stretch;
    border: 1px solid var(--line);
    border-radius: 6px;
    background: #fbfcfd;
    padding: 14px 15px;
    font-size: 12.5px;
    min-height: 300px;
  }
  .side.pinned { border-color: var(--accent); box-shadow: 0 0 0 2px rgba(45, 76, 126, .1); }
  .s-empty { color: var(--faint); font-size: 12px; line-height: 1.6; }
  .s-name { font-size: 15px; font-weight: 700; line-height: 1.3; margin-bottom: 3px; }
  .s-rating {
    font-size: 10px; font-weight: 700; letter-spacing: .09em;
    text-transform: uppercase; margin-bottom: 12px;
  }
  .s-block { padding-top: 10px; border-top: 1px solid var(--line); margin-top: 10px; }
  .s-block:first-of-type { border-top: 0; padding-top: 0; margin-top: 0; }
  .s-h {
    font-size: 9.5px; font-weight: 700; letter-spacing: .1em;
    text-transform: uppercase; color: var(--accent); margin-bottom: 6px;
  }
  .s-kv { display: flex; justify-content: space-between; gap: 10px; padding: 2px 0; }
  .s-kv dt { color: var(--muted); }
  .s-kv dd { margin: 0; font-variant-numeric: tabular-nums; font-weight: 600; text-align: right; }
  .s-list { margin: 3px 0 0; padding: 0; list-style: none; }
  .s-list li {
    display: inline-block; font-size: 11px; font-weight: 600;
    border: 1px solid var(--line-strong); border-radius: 4px;
    padding: 1px 7px; margin: 0 4px 4px 0; color: var(--muted); background: var(--surface);
  }
  .s-none { color: var(--faint); font-style: italic; }
  .s-note { font-size: 11px; color: var(--faint); margin-top: 3px; line-height: 1.45; }
  .s-hint { font-size: 11px; color: var(--faint); margin-top: 12px; padding-top: 9px; border-top: 1px solid var(--line); }
  .s-hint button {
    font: inherit; font-size: 11px; background: none; border: 0; padding: 0;
    color: var(--accent); text-decoration: underline; cursor: pointer;
  }

  @media (max-width: 820px) {
    .plot-row { flex-direction: column; }
    .side { flex: 1 1 auto; width: 100%; min-height: 0; }
  }

  .grid-line { stroke: var(--line); stroke-width: 1; }
  .band-edge { stroke: var(--line-strong); stroke-width: 1; stroke-dasharray: 3 3; }
  .axis-text { fill: var(--faint); font-size: 10px; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
  .axis-title { fill: var(--muted); font-size: 11px; font-weight: 600; letter-spacing: .06em; }
  .band-label { font-size: 9px; font-weight: 700; letter-spacing: .1em; }

  .pt { cursor: pointer; transition: opacity .12s; }
  .pt circle, .pt rect.mark { stroke-width: 1.5; }
  .pt.dim { opacity: .12; }
  .pt.hit { fill: transparent; stroke: none; }

  footer {
    margin-top: 16px;
    font-size: 11.5px;
    color: var(--faint);
    display: flex;
    flex-wrap: wrap;
    gap: 6px 18px;
  }
  .key { display: inline-flex; align-items: center; gap: 6px; }
  .key svg { width: 14px; height: 14px; min-width: 0; }
  .stamp { margin-top: 14px; font-size: 11px; color: var(--faint); }
</style>
</head>
<body>
<div class="wrap">

  <header>
    <h1>PBAC adoption — consumption vs complexity</h1>
    <p class="sub">Each account plotted on the two axes the rating is built from. The shaded regions are the rating grid, so a point's background <em>is</em> its stoplight.</p>
  </header>

  <div class="panel">
    <div class="controls" id="controls"></div>
    <div class="plot-row">
      <div class="plot-wrap">
        <svg id="plot" viewBox="0 0 900 560" role="img" aria-label="Scatter plot of accounts by consumption ratio and PBAC complexity score"></svg>
      </div>
      <aside class="side" id="side" aria-live="polite"></aside>
    </div>
    <footer>
      <span class="key"><svg viewBox="0 0 14 14"><circle cx="7" cy="7" r="4" fill="#8792a3" stroke="#161a21" stroke-width="1"/></svg> account, filled by rating</span>
      <span class="key"><svg viewBox="0 0 14 14"><circle cx="7" cy="7" r="4" fill="#8792a3" stroke="#161a21" stroke-width="1" stroke-dasharray="1.8 1.8"/></svg> consumption ratio unavailable (drawn at zero)</span>
      <span class="key"><svg viewBox="0 0 14 14"><circle cx="7" cy="7" r="4" fill="#8792a3" stroke="#161a21" stroke-width="2.4"/></svg> ratio above axis maximum, pinned to edge</span>
      <span class="key"><svg viewBox="0 0 14 14"><rect x="3" y="3" width="8" height="8" fill="#8792a3" stroke="#161a21" stroke-width="1"/></svg> in Installation and Config. (lifecycle S1)</span>
      <span class="key">points are jittered to separate co-located accounts</span>
    </footer>
    <div class="stamp" id="stamp"></div>
  </div>

</div>

<script>
const POINTS = __POINTS__;
const CFG    = __CONFIG__;

const COLOURS = {
  black: { fg: "#343a44", bg: "#e9ebef" },
  red:   { fg: "#b04234", bg: "#fbeae6" },
  amber: { fg: "#b47b1c", bg: "#fdf4e3" },
  green: { fg: "#35704c", bg: "#e8f3ed" }
};
const ORDER = ["green", "amber", "red", "black"];

const SVGNS = "http://www.w3.org/2000/svg";
const W = 900, H = 560;
const M = { top: 26, right: 30, bottom: 58, left: 62 };
const PW = W - M.left - M.right;
const PH = H - M.top - M.bottom;

// Reserve a pinned column at the right edge for ratios past the axis maximum,
// so an unlimited-user contract can't flatten everyone else against the y-axis.
const PIN_W = 46;
const AXIS_W = PW - PIN_W;

const x = r => M.left + Math.min(r, CFG.ratioMax) / CFG.ratioMax * AXIS_W;
const y = s => M.top + PH - (Math.min(s, CFG.scoreMax) / CFG.scoreMax) * PH;

// Deterministic jitter: same account lands in the same place on every run, so
// the chart is comparable week to week.
function seeded(str) {
  let h = 2166136261;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 16777619); }
  return ((h >>> 0) % 10000) / 10000;
}

// Which rating a coordinate falls in — the same grid op_list_pbac_adoption()
// applies, so the shading can't disagree with the stored rating.
// Band caption, one entry per line; the two red cells are named for the
// failing axis, matching the stored rating labels.
function bandLines(key, highComplexity) {
  if (key === "red")   return ["RED: HIGH RISK", highComplexity ? "CONSUMPTION" : "ADOPTION"];
  if (key === "black") return ["BLACK: CRITICAL", "CHURN RISK"];
  return [key.toUpperCase()];
}

function ratingAt(score, ratio) {
  const complex = score >= CFG.complexityMin;
  const heavy   = ratio !== null && ratio >= CFG.ratioModerate;
  const some    = ratio !== null && ratio >= CFG.ratioMinimal;
  if (complex) return heavy ? "green" : (some ? "amber" : "red");
  return heavy ? "amber" : (some ? "red" : "black");
}

const el = (tag, attrs, text) => {
  const n = document.createElementNS(SVGNS, tag);
  for (const k in attrs) n.setAttribute(k, attrs[k]);
  if (text !== undefined) n.textContent = text;
  return n;
};

const fmtNum = v => (v === null || v === undefined || isNaN(v)) ? "—"
                  : v.toLocaleString(undefined, { maximumFractionDigits: 0 });
const fmtRatio = v => (v === null || v === undefined || isNaN(v)) ? "not available"
                    : v.toFixed(2);

const svg  = document.getElementById("plot");
const side = document.getElementById("side");
const active = new Set(ORDER);

// The panel shows `pinned` when one is set (click), otherwise whatever is
// under the cursor, otherwise the opening selection. Pinning matters because
// the panel is off to the side: without it the summary vanishes the moment you
// move the mouse towards what you are trying to read.
let pinnedName = null;
let hoverName  = null;
let selRing    = null;
const visible  = {};            // account name -> {cx, cy} when drawn, else null
const byName   = Object.fromEntries(POINTS.map(p => [p.name, p]));

function draw() {
  svg.textContent = "";

  // ---- background bands: the rating grid itself --------------------------
  const xEdges = [0, CFG.ratioMinimal, CFG.ratioModerate, CFG.ratioMax];
  const yEdges = [0, CFG.complexityMin, CFG.scoreMax];

  for (let r = 0; r < 2; r++) {
    for (let c = 0; c < 3; c++) {
      const mid = (xEdges[c] + xEdges[c + 1]) / 2;
      const key = ratingAt(r === 1 ? CFG.scoreMax : 0, mid);
      const x0 = x(xEdges[c]);
      // The last column swallows the pinned lane, so an outlier sits on the
      // same colour its ratio actually earns.
      const x1 = c === 2 ? M.left + PW : x(xEdges[c + 1]);
      const yTop = y(r === 1 ? yEdges[2] : yEdges[1]);
      const yBot = y(r === 1 ? yEdges[1] : yEdges[0]);
      svg.appendChild(el("rect", {
        x: x0, y: yTop, width: x1 - x0, height: yBot - yTop,
        fill: COLOURS[key].bg
      }));
      bandLines(key, r === 1).forEach((line, i) => svg.appendChild(el("text", {
        x: x0 + 7, y: yTop + 15 + i * 11, class: "band-label", fill: COLOURS[key].fg,
        "fill-opacity": ".85"
      }, line)));
    }
  }

  // ---- band edges ---------------------------------------------------------
  [CFG.ratioMinimal, CFG.ratioModerate].forEach(v => {
    svg.appendChild(el("line", { x1: x(v), y1: M.top, x2: x(v), y2: M.top + PH, class: "band-edge" }));
  });
  svg.appendChild(el("line", {
    x1: M.left, y1: y(CFG.complexityMin), x2: M.left + PW, y2: y(CFG.complexityMin), class: "band-edge"
  }));
  // Divider marking off the pinned lane.
  svg.appendChild(el("line", {
    x1: x(CFG.ratioMax), y1: M.top, x2: x(CFG.ratioMax), y2: M.top + PH,
    stroke: "#c3cbd6", "stroke-width": 1
  }));

  // ---- axes ---------------------------------------------------------------
  for (let s = 0; s <= CFG.scoreMax; s++) {
    svg.appendChild(el("text", {
      x: M.left - 10, y: y(s) + 3.5, class: "axis-text", "text-anchor": "end"
    }, s));
  }
  const xTicks = [0, 0.25, 0.5, 0.75, 1.0, 1.25].filter(v => v <= CFG.ratioMax);
  xTicks.forEach(v => {
    svg.appendChild(el("text", {
      x: x(v), y: M.top + PH + 16, class: "axis-text", "text-anchor": "middle"
    }, v.toFixed(2)));
  });
  svg.appendChild(el("text", {
    x: M.left + PW - PIN_W / 2, y: M.top + PH + 16, class: "axis-text", "text-anchor": "middle"
  }, ">" + CFG.ratioMax));

  svg.appendChild(el("rect", {
    x: M.left, y: M.top, width: PW, height: PH,
    fill: "none", stroke: "#c3cbd6", "stroke-width": 1
  }));

  svg.appendChild(el("text", {
    x: M.left + PW / 2, y: H - 16, class: "axis-title", "text-anchor": "middle"
  }, "CONSUMPTION RATIO  (registered ÷ licensed users, Production)"));
  const yt = el("text", { x: 16, y: M.top + PH / 2, class: "axis-title", "text-anchor": "middle" },
                "COMPLEXITY SCORE");
  yt.setAttribute("transform", "rotate(-90 16 " + (M.top + PH / 2) + ")");
  svg.appendChild(yt);

  // ---- points -------------------------------------------------------------
  const q = (document.getElementById("q") || {}).value || "";
  const needle = q.trim().toLowerCase();

  POINTS.forEach(p => {
    const known  = p.ratio !== null && p.ratio !== undefined && !isNaN(p.ratio);
    const pinned = known && p.ratio > CFG.ratioMax;
    const rBase  = known ? p.ratio : 0;

    const jx = (seeded(p.name) - 0.5) * (pinned ? PIN_W * 0.7 : 16);
    const jy = (seeded(p.name + "y") - 0.5) * 30;

    // Clamped to the frame: jitter must not push a dot outside the plot area,
    // where it would read as belonging to no band at all.
    const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
    const cx = clamp(pinned ? (M.left + PW - PIN_W / 2 + jx) : x(rBase) + jx,
                     M.left + 7, M.left + PW - 7);
    const cy = clamp(y(p.score) + jy, M.top + 7, M.top + PH - 7);

    const col = COLOURS[p.colour] || COLOURS.black;
    const on  = active.has(p.colour) && (!needle || p.name.toLowerCase().includes(needle));

    visible[p.name] = on ? { cx: cx, cy: cy } : null;

    const g = el("g", { class: "pt" + (on ? "" : " dim") });

    // Fill carries the rating; the outline is always ink, so a pale amber dot
    // reads as clearly against its own band as a dark one does.
    // Square for accounts in Installation and Config.; same area as the dot.
    const c = p.install
      ? el("rect", { x: cx - 4, y: cy - 4, width: 8, height: 8, class: "mark",
                     fill: col.fg, stroke: "#161a21", "stroke-width": pinned ? 2.4 : 1 })
      : el("circle", {
          cx: cx, cy: cy, r: 4.5,
          fill: col.fg,
          stroke: "#161a21",
          "stroke-width": pinned ? 2.4 : 1
        });
    // Unknown ratio keeps the rating fill but breaks its outline: the dot's x
    // position is a placeholder, not a measurement.
    if (!known) {
      c.setAttribute("stroke-dasharray", "1.8 1.8");
      c.setAttribute("fill-opacity", ".45");
    }
    g.appendChild(c);

    // Generous invisible hit area — 4.5px dots are fiddly to hit.
    const hit = el("circle", { cx: cx, cy: cy, r: 11, class: "hit" });
    g.appendChild(hit);

    if (on) {
      g.addEventListener("mouseenter", () => { hoverName = p.name; render(); });
      g.addEventListener("mouseleave", () => { hoverName = null;   render(); });
      g.addEventListener("click", () => {
        pinnedName = (pinnedName === p.name) ? null : p.name;
        render();
      });
    }
    svg.appendChild(g);
  });

  // One reusable selection ring, drawn last so it sits over the dots and moved
  // on selection. Deliberately NOT re-running draw() on hover: rebuilding the
  // nodes under the cursor re-fires mouseenter on the replacements, which is an
  // endless redraw.
  selRing = el("circle", {
    cx: 0, cy: 0, r: 9, fill: "none", stroke: "#2d4c7e",
    "stroke-width": 2, "stroke-opacity": ".9", visibility: "hidden",
    "pointer-events": "none"
  });
  svg.appendChild(selRing);

  render();
}

// Repaint the side panel and move the selection ring. Cheap — no redraw.
function render() {
  // Hover wins while the cursor is on a point, so a pin never blocks you from
  // reading the next account; leaving falls back to whatever is pinned.
  const name = hoverName || pinnedName;
  const p    = name ? byName[name] : null;
  const at   = name ? visible[name] : null;

  if (selRing) {
    if (p && at) {
      selRing.setAttribute("cx", at.cx);
      selRing.setAttribute("cy", at.cy);
      selRing.setAttribute("visibility", "visible");
    } else {
      selRing.setAttribute("visibility", "hidden");
    }
  }

  const isPinned = pinnedName !== null && name === pinnedName;
  side.className = "side" + (isPinned ? " pinned" : "");

  // A selection that a filter or search has hidden falls back to the empty
  // state — a summary for a dot that isn't on the plot is just confusing.
  if (!p || !at) {
    side.innerHTML =
      '<p class="s-empty">Hover a point for its account summary, or click to keep it ' +
      'on screen while you look elsewhere.</p>';
    return;
  }

  const col    = COLOURS[p.colour] || COLOURS.black;
  const known  = p.ratio !== null && p.ratio !== undefined && !isNaN(p.ratio);
  const pinned = known && p.ratio > CFG.ratioMax;

  const feats = (p.features && p.features !== "None")
    ? '<ul class="s-list">' +
        p.features.split(", ").map(f => '<li>' + esc(f) + '</li>').join("") +
      '</ul>'
    : '<div class="s-none">None</div>';

  const plats = (p.platforms && p.platforms !== "—")
    ? '<ul class="s-list">' +
        p.platforms.split(" · ").map(f => '<li>' + esc(f) + '</li>').join("") +
      '</ul>'
    : '<div class="s-none">None recorded</div>';

  side.innerHTML =
    '<div class="s-name">' + esc(p.name) + '</div>' +
    '<div class="s-rating" style="color:' + col.fg + '">' + esc(p.rating) + '</div>' +

    '<div class="s-block">' +
      '<div class="s-h">Consumption</div>' +
      '<dl class="s-kv"><dt>Registered users</dt><dd>' + fmtNum(p.registered) + '</dd></dl>' +
      '<dl class="s-kv"><dt>Licensed users</dt><dd>' + fmtNum(p.licensed) + '</dd></dl>' +
      '<dl class="s-kv"><dt>Consumption ratio</dt><dd>' +
        (known ? fmtRatio(p.ratio) : '<span class="s-none">n/a</span>') + '</dd></dl>' +
      (known
        ? (pinned
            ? '<p class="s-note">Above the axis maximum of ' + CFG.ratioMax +
              ' — drawn in the pinned column at the right edge.</p>'
            : '')
        : '<p class="s-note">No ratio available. Drawn at zero as a placeholder; ' +
          'the rating treats it as not yet demonstrated, not as zero usage.</p>') +
    '</div>' +

    '<div class="s-block">' +
      '<div class="s-h">Complexity</div>' +
      '<dl class="s-kv"><dt>Score</dt><dd>' + p.score + ' / ' + CFG.scoreMax + '</dd></dl>' +
      '<div style="margin-top:8px"><dt style="color:var(--muted)">Platforms</dt>' + plats + '</div>' +
      '<div style="margin-top:6px"><dt style="color:var(--muted)">Feature adoption</dt>' + feats + '</div>' +
    '</div>' +

    '<div class="s-hint">' +
      (isPinned
        ? 'Pinned. <button type="button" id="unpin">Clear</button>'
        : 'Click the point to keep this on screen.') +
    '</div>';

  const un = document.getElementById("unpin");
  if (un) un.addEventListener("click", () => { pinnedName = null; render(); });
}

function esc(s) {
  return String(s === null || s === undefined ? "—" : s)
    .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// ---- controls -------------------------------------------------------------
const controls = document.getElementById("controls");
ORDER.forEach(k => {
  const n = POINTS.filter(p => p.colour === k).length;
  const b = document.createElement("button");
  b.className = "pill";
  b.setAttribute("aria-pressed", "true");
  b.innerHTML = '<span class="dot" style="background:' + COLOURS[k].fg + '"></span>' +
                k.charAt(0).toUpperCase() + k.slice(1) +
                ' <span class="n">' + n + '</span>';
  b.addEventListener("click", () => {
    if (active.has(k)) { active.delete(k); b.setAttribute("aria-pressed", "false"); }
    else { active.add(k); b.setAttribute("aria-pressed", "true"); }
    hoverName = null; draw();
  });
  controls.appendChild(b);
});

const q = document.createElement("input");
q.id = "q";
q.className = "search";
q.type = "search";
q.placeholder = "Find an account…";
q.addEventListener("input", () => { hoverName = null; draw(); });
controls.appendChild(q);

document.getElementById("stamp").textContent =
  POINTS.length + " accounts · thresholds: complexity ≥ " + CFG.complexityMin +
  ", ratio " + CFG.ratioMinimal + " / " + CFG.ratioModerate +
  " · generated " + CFG.generated;

// Open on the most complex account rather than an empty panel, so the summary's
// shape is visible before anyone touches the plot.
pinnedName = POINTS.reduce((a, b) => (b.score > a.score ? b : a), POINTS[0]).name;

draw();
</script>
</body>
</html>
)---"
}


# TRUE for each name whose account is in lifecycle stage S1 — "S1: Installation
# and Config." in Salesforce. Matched on the stage number or the words, so a
# relabel of the picklist doesn't silently turn every square back into a dot.
.pbac_install_stage <- function(accounts, names) {
  if (is.null(accounts) || !all(c("Account Name", "Lifecycle Stage") %in% names(accounts))) {
    return(rep(FALSE, length(names)))
  }
  stage <- as.character(accounts$`Lifecycle Stage`)[
    match(trimws(as.character(names)), trimws(as.character(accounts$`Account Name`)))]
  !is.na(stage) & grepl("^\\s*S1\\b|install", stage, ignore.case = TRUE, perl = TRUE)
}
