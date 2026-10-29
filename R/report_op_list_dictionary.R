#' Data dictionary for op_list (HTML + CSV)
#'
#' Walks \code{op_list} and writes a self-contained HTML page with one
#' expandable entry per object (tables, nested lists such as
#' \code{consumption} and \code{changes_detail}, and scalars). For every table it
#' lists each field with its data source, R type, how often it is filled in, the
#' number of distinct values, an example value and a description. A search box,
#' source filters, "undocumented only" and expand / collapse all make it easy to
#' explore; clicking a field copies its R accessor
#' (e.g. \code{op_list$accounts$`Account ID`}). The same rows go to a CSV.
#'
#' Structure, types, fill rates and examples are read from \code{op_list}
#' itself, so the page always matches the run that built it. Descriptions and
#' sources come from the hand-maintained \code{descriptions} CSV, with columns
#' \code{object}, \code{field}, \code{description}, \code{source}:
#' \itemize{
#'   \item \code{object} is a dotted path (\code{accounts},
#'     \code{consumption.low_consumption}) and may use \code{*} as a wildcard
#'     (\code{changes_detail.*.field_changes}). \code{*} on its own means "any
#'     object": use it for fields that mean the same everywhere
#'     (\code{Account ID}).
#'   \item A blank \code{field} describes the object itself; its \code{source}
#'     is inherited by every field that doesn't set its own.
#'   \item A field \code{source} overrides the object's, for fields computed in
#'     ImmuteR or read from \code{data/input/}.
#'   \item Lookup order for a field: this object, a wildcard object, this
#'     object's \code{*} field, then the \code{*} object. \code{<field>.new} /
#'     \code{<field>.old} columns in \code{changes_detail} fall back to the
#'     description of \code{<field>}.
#' }
#' Source labels used so far: Salesforce, Looker, BigQuery, Jira, Productboard,
#' Local input, Derived (ImmuteR), Snapshot diff, ImmuteR constants (a
#' parenthetical, e.g. "Derived (from Looker)", is shown but grouped under its
#' first word for the source filter).
#'
#' @param op_list Named list from \code{\link{enrich_opportunities}} /
#'   \code{\link{enrich_list_object}}.
#' @param descriptions Path to the descriptions CSV. Missing file = no
#'   descriptions (the structure is still documented).
#' @param output_path HTML file to write.
#' @param csv_path CSV export of the dictionary (one row per object and per
#'   field). \code{NULL} skips it.
#' @param open Logical. Open the page in the browser when done.
#'
#' @return Invisibly, the dictionary as a data frame (the CSV rows).
#'
#' @examples
#' \dontrun{
#'   report_op_list_dictionary(op_list)
#' }
#' @export
report_op_list_dictionary <- function(op_list,
                                      descriptions = "data/input/op_list_dictionary.csv",
                                      output_path  = "output/op_list_dictionary.html",
                                      csv_path     = sub("\\.html$", ".csv", output_path),
                                      open         = interactive()) {
  dict <- .dd_read(descriptions)
  objs <- .dd_walk(op_list, path = "", r_path = "op_list")

  # Object-level description / source, then each field's.
  for (i in seq_along(objs)) {
    o <- objs[[i]]
    od <- .dd_lookup(dict, o$path, "")
    objs[[i]]$desc   <- od$description
    objs[[i]]$source <- od$source
  }
  obj_src <- stats::setNames(vapply(objs, `[[`, "", "source"), vapply(objs, `[[`, "", "path"))
  inherit_src <- function(path) {
    while (!is.na(path) && nzchar(path)) {
      if (nzchar(obj_src[[path]])) return(obj_src[[path]])
      path <- if (grepl(".", path, fixed = TRUE)) sub("\\.[^.]*$", "", path) else NA_character_
    }
    ""
  }
  for (i in seq_along(objs)) {
    o <- objs[[i]]
    if (!nzchar(o$source)) objs[[i]]$source <- inherit_src(o$parent)
    if (o$kind != "table") next
    objs[[i]]$fields <- lapply(.dd_profile(o$df), function(f) {
      d <- .dd_lookup(dict, o$path, f$name)
      f$desc   <- d$description
      f$source <- if (nzchar(d$source)) d$source else objs[[i]]$source
      f
    })
    objs[[i]]$df <- NULL
  }

  # ── CSV ────────────────────────────────────────────────────────────────────
  rows <- do.call(rbind, lapply(objs, function(o) {
    head <- data.frame(object = o$path, r_path = o$r_path, kind = o$kind,
                       rows = if (is.null(o$rows)) NA_integer_ else o$rows, field = "",
                       type = if (is.null(o$type)) "" else o$type, filled_pct = NA_real_,
                       distinct = NA_integer_, example = if (is.null(o$value)) "" else o$value,
                       description = o$desc, source = o$source, stringsAsFactors = FALSE)
    if (is.null(o$fields) || !length(o$fields)) return(head)
    rbind(head, do.call(rbind, lapply(o$fields, function(f) data.frame(
      object = o$path, r_path = paste0(o$r_path, .dd_acc(f$name)), kind = "field", rows = o$rows,
      field = f$name, type = f$type, filled_pct = f$filled, distinct = f$distinct,
      example = f$example, description = f$desc, source = f$source, stringsAsFactors = FALSE))))
  }))
  rownames(rows) <- NULL
  if (!is.null(csv_path)) {
    dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(rows, csv_path, row.names = FALSE, na = "")
  }

  # ── HTML ───────────────────────────────────────────────────────────────────
  payload <- list(
    updated   = if (is.null(op_list$updated)) "" else as.character(op_list$updated)[1],
    generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
    objects   = lapply(objs, function(o) {
      o$parent <- if (is.na(o$parent)) NULL else o$parent
      o
    })
  )
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  html <- .ao_fill(.DD_TEMPLATE, "{{DATA}}", json)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_path, useBytes = TRUE)

  f <- rows[rows$kind == "field", ]
  message("op_list dictionary: ", sum(rows$kind == "table"), " tables, ", nrow(f), " fields (",
          sum(!nzchar(f$description)), " without a description, ", sum(!nzchar(f$source)),
          " without a source) -> ", output_path)
  if (isTRUE(open)) utils::browseURL(normalizePath(output_path))
  invisible(rows)
}


# ── Internal ──────────────────────────────────────────────────────────────────

.dd_read <- function(path) {
  empty <- data.frame(object = character(0), field = character(0), description = character(0),
                      source = character(0), stringsAsFactors = FALSE)
  if (is.null(path) || !file.exists(path)) {
    warning("op_list dictionary: descriptions file not found (", path, "); fields will have no descriptions.",
            call. = FALSE)
    return(empty)
  }
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(0),
                       encoding = "UTF-8")
  for (k in names(empty)) if (!k %in% names(d)) d[[k]] <- ""
  d <- d[names(empty)]
  d[] <- lapply(d, function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x })
  d
}

# `$name` or `$\`name\`` for an R accessor.
.dd_acc <- function(nm) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", nm) && !grepl("^\\.[0-9]", nm)) paste0("$", nm)
  else paste0("$`", nm, "`")
}

# Flatten op_list into one entry per object, depth-first, parents before children.
.dd_walk <- function(x, path, r_path) {
  out <- list()
  for (nm in names(x)) {
    v   <- x[[nm]]
    p   <- if (nzchar(path)) paste0(path, ".", nm) else nm
    rp  <- paste0(r_path, .dd_acc(nm))
    par <- if (nzchar(path)) path else NA_character_
    if (is.data.frame(v)) {
      out[[length(out) + 1]] <- list(path = p, r_path = rp, parent = par, kind = "table",
                                     rows = nrow(v), type = "data.frame", df = as.data.frame(v))
    } else if (is.list(v) && length(v) && !is.null(names(v)) && all(nzchar(names(v)))) {
      out[[length(out) + 1]] <- list(path = p, r_path = rp, parent = par, kind = "group",
                                     type = paste0("list (", length(v), ")"))
      out <- c(out, .dd_walk(v, p, rp))
    } else {
      val <- .dd_value(v)
      out[[length(out) + 1]] <- list(path = p, r_path = rp, parent = par, kind = "value",
                                     type = paste(class(v)[1], if (length(v) != 1) paste0("[", length(v), "]")),
                                     value = val, value_short = .dd_trunc(val, 60))
    }
  }
  out
}

.dd_trunc <- function(s, n) {
  s <- gsub("[\r\n\t]+", " ", s)
  ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
}

.dd_value <- function(v) {
  if (is.null(v)) return("NULL")
  if (is.list(v)) return(if (!length(v)) "empty list" else paste0("unnamed list of ", length(v)))
  if (is.atomic(v)) {
    s <- as.character(v)
    if (!length(s)) return("(empty)")
    if (length(s) > 10) return(paste0(paste(s[1:10], collapse = ", "), ", … (", length(s), " values)"))
    return(paste(s, collapse = ", "))
  }
  paste0("<", class(v)[1], ">")
}

# One entry per column: R type, % filled ("-", "", "NA" and em-dash count as
# missing, the same as the dashboards), distinct values, an example.
.dd_profile <- function(df) {
  lapply(names(df), function(nm) {
    v <- df[[nm]]
    chr <- if (is.list(v)) {
      vapply(v, function(e) if (!length(e) || all(is.na(e))) NA_character_ else paste(format(e), collapse = ", "), "")
    } else if (inherits(v, c("Date", "POSIXt"))) format(v) else as.character(v)
    miss <- is.na(chr) | trimws(chr) %in% c("", "-", "NA", "—")
    ok <- chr[!miss]
    list(name     = nm,
         type     = if (is.list(v)) "list" else class(v)[1],
         filled   = if (length(chr)) round(100 * mean(!miss)) else NA_real_,
         distinct = if (length(chr)) length(unique(ok)) else NA_integer_,
         example  = if (length(ok)) .dd_trunc(ok[1], 120) else "")
  })
}

# Description and source for an object (field = "") or one of its fields.
.dd_lookup <- function(dict, path, field) {
  is_glob <- grepl("*", dict$object, fixed = TRUE) & dict$object != "*"
  glob_hit <- is_glob & vapply(dict$object, function(p) grepl(utils::glob2rx(p), path), logical(1))
  cands <- if (!nzchar(field)) {
    list(dict$object == path & dict$field == "",
         glob_hit & dict$field == "")
  } else {
    list(dict$object == path & dict$field == field,
         glob_hit & dict$field == field,
         dict$object == path & dict$field == "*",
         dict$object == "*" & dict$field == field)
  }
  pick <- function(col) {
    for (m in cands) { v <- dict[[col]][m]; v <- v[nzchar(v)]; if (length(v)) return(v[1]) }
    ""
  }
  desc <- pick("description")
  if (!nzchar(desc) && nzchar(field) && grepl("\\.(new|old)$", field)) {
    base <- sub("\\.(new|old)$", "", field)
    bd <- .dd_lookup(dict, sub("^changes_detail\\.([^.]+).*$", "\\1", path), base)$description
    desc <- paste0(if (grepl("\\.new$", field)) "Value in this run" else "Value in the comparison snapshot",
                   " of `", base, "`.", if (nzchar(bd)) paste0(" ", bd) else "")
  }
  list(description = desc, source = pick("source"))
}

.DD_TEMPLATE <- r"---(<!doctype html>
<html lang="en"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>op_list Data Dictionary</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  :root {
    --im-black: #0D0D0D; --im-lime: #C8F135; --im-lime-dk: #A8CC18; --im-lime-lt: #F5FCD8; --im-white: #FFFFFF;
    --amber-lt: #FEF3C7; --red-lt: #FEE2E2;
    --text-1: #111111; --text-2: #555555; --text-3: #999999; --border: #E5E5E5;
    --surface: #F8F8F6; --surface-2: #F0F0EE; --radius: 10px; --radius-sm: 6px;
    --shadow: 0 1px 3px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.06);
    --font: "Inter", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    --mono: "JetBrains Mono", ui-monospace, SFMono-Regular, Menlo, monospace;
  }
  body { font-family: var(--font); font-size: 14px; line-height: 1.5; color: var(--text-1); background: var(--surface); }
  [hidden] { display: none !important; }
  .top { background: var(--im-black); color: #fff; padding: 14px 24px; position: sticky; top: 0; z-index: 10;
         display: flex; flex-wrap: wrap; align-items: center; gap: 10px 18px; }
  .top h1 { font-size: 18px; font-weight: 800; letter-spacing: -.01em; }
  .top h1 code { font-family: var(--mono); color: var(--im-lime); font-weight: 500; }
  .top .meta { font-size: 12px; color: #A8A8A8; }
  .top input { margin-left: auto; font: inherit; font-size: 13px; padding: 7px 12px; border-radius: var(--radius-sm);
               border: 1px solid #333; background: #1C1C1C; color: #fff; min-width: 300px; }
  .bar { background: var(--im-white); border-bottom: 1px solid var(--border); padding: 10px 24px;
         display: flex; flex-wrap: wrap; align-items: center; gap: 6px 8px; }
  .bar .lbl { font-size: 10.5px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--text-3); margin-right: 2px; }
  .bar .sep { width: 1px; height: 22px; background: var(--border); margin: 0 6px; }
  .pill { font: inherit; font-size: 12px; padding: 4px 10px; border-radius: 999px; border: 1px solid var(--border);
          background: var(--im-white); color: var(--text-2); cursor: pointer; white-space: nowrap; display: inline-flex; align-items: center; gap: 6px; }
  .pill:hover { border-color: var(--text-3); color: var(--text-1); }
  .pill.on { background: var(--im-black); color: var(--im-lime); border-color: var(--im-black); }
  .pill .n { font-size: 10.5px; font-weight: 700; color: var(--text-3); }
  .pill.on .n { color: var(--im-lime); }
  .btn { font: inherit; font-size: 12px; padding: 5px 10px; border-radius: var(--radius-sm); border: 1px solid var(--border);
         background: var(--im-white); color: var(--text-1); cursor: pointer; }
  .btn:hover { background: var(--surface-2); }
  .right { margin-left: auto; display: flex; gap: 6px; }
  .wrap { max-width: 1400px; margin: 0 auto; padding: 16px 24px 60px; }
  .summary { font-size: 12.5px; color: var(--text-2); margin-bottom: 12px; }
  .sw { width: 9px; height: 9px; border-radius: 50%; display: inline-block; flex: none; }
  .src { display: inline-flex; align-items: center; gap: 5px; font-size: 11px; font-weight: 600; padding: 1px 8px;
         border-radius: 999px; background: var(--surface-2); color: var(--text-2); white-space: nowrap; }
  .node { background: var(--im-white); border: 1px solid var(--border); border-radius: var(--radius); margin-bottom: 8px; overflow: hidden; }
  .node.open { border-color: var(--im-black); box-shadow: var(--shadow); }
  .node.group { background: transparent; border-style: dashed; }
  .node.group.open { border-color: var(--text-3); box-shadow: none; }
  .head { display: flex; align-items: center; gap: 10px; padding: 10px 14px; cursor: pointer; user-select: none; flex-wrap: wrap; }
  .head:hover { background: var(--surface); }
  .chev { font-size: 10px; color: var(--text-3); transition: transform .15s; width: 10px; flex: none; }
  .node.open > .head .chev { transform: rotate(90deg); }
  .path { font-family: var(--mono); font-size: 13.5px; font-weight: 500; }
  .path .pre { color: var(--text-3); }
  .kind { font-size: 10px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--text-3);
          border: 1px solid var(--border); border-radius: 4px; padding: 0 6px; }
  .dim { font-size: 12px; color: var(--text-2); font-variant-numeric: tabular-nums; }
  .desc-s { font-size: 12.5px; color: var(--text-2); flex: 1 1 320px; min-width: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .node.open > .head .desc-s { display: none; }
  .srcs { display: flex; gap: 4px; flex-wrap: wrap; margin-left: auto; }
  .body { display: none; padding: 0 14px 12px 34px; }
  .node.open > .body { display: block; }
  .node.group > .body { padding: 4px 10px 6px 24px; }
  .desc { font-size: 13px; color: var(--text-1); margin: 2px 0 8px; max-width: 900px; }
  .rpath { display: inline-flex; align-items: center; gap: 8px; font-family: var(--mono); font-size: 12px; color: var(--text-2);
           background: var(--surface); border: 1px solid var(--border); border-radius: var(--radius-sm); padding: 3px 8px; margin-bottom: 8px; cursor: pointer; }
  .rpath:hover { border-color: var(--text-3); }
  .rpath .cp { font-family: var(--font); font-size: 10.5px; font-weight: 700; color: var(--text-3); }
  .val { font-family: var(--mono); font-size: 12.5px; background: var(--surface); padding: 6px 10px; border-radius: var(--radius-sm); }
  .tw { overflow-x: auto; }
  table { width: 100%; border-collapse: collapse; font-size: 12.5px; }
  th { text-align: left; font-size: 10px; font-weight: 700; letter-spacing: .06em; text-transform: uppercase; color: var(--text-3);
       padding: 7px 10px; border-bottom: 1.5px solid var(--border); white-space: nowrap; cursor: pointer; }
  th:hover { color: var(--text-1); }
  td { padding: 7px 10px; border-bottom: 1px solid var(--surface-2); vertical-align: top; }
  tr:hover td { background: var(--im-lime-lt); }
  tr.hit td { background: #FEF9C3; }
  td.f { font-family: var(--mono); font-size: 12px; white-space: nowrap; cursor: pointer; }
  td.f:hover { text-decoration: underline; text-decoration-color: var(--im-lime-dk); }
  td.t { font-family: var(--mono); font-size: 11.5px; color: var(--text-2); white-space: nowrap; }
  td.num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
  td.ex { font-family: var(--mono); font-size: 11.5px; color: var(--text-2); max-width: 260px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  td.d { min-width: 260px; }
  .nodesc { color: #B45309; font-size: 11.5px; font-style: italic; }
  .fill { display: flex; align-items: center; gap: 6px; justify-content: flex-end; }
  .fill .tr { width: 46px; height: 6px; background: var(--surface-2); border-radius: 3px; overflow: hidden; }
  .fill .tr span { display: block; height: 100%; background: var(--im-lime-dk); }
  .fill .tr span.lo { background: #F59E0B; } .fill .tr span.zero { background: transparent; }
  .empty { padding: 30px; text-align: center; color: var(--text-3); }
  .toast { position: fixed; bottom: 18px; left: 50%; transform: translateX(-50%); background: var(--im-black); color: var(--im-lime);
           font-size: 12.5px; padding: 7px 14px; border-radius: var(--radius-sm); opacity: 0; transition: opacity .2s; pointer-events: none; }
  .toast.on { opacity: 1; }
  mark { background: #FDE68A; color: inherit; border-radius: 2px; }
  @media (max-width: 760px) { .top input { min-width: 0; width: 100%; margin-left: 0; } .wrap, .bar { padding-left: 12px; padding-right: 12px; }
                              .body { padding-left: 14px; } .desc-s { display: none; } }
</style></head>
<body>
<div class="top">
  <h1><code>op_list</code> data dictionary</h1>
  <div class="meta" id="meta"></div>
  <input id="q" type="search" placeholder="Search objects, fields, descriptions, examples...">
</div>
<div class="bar" id="src-bar"></div>
<div class="bar">
  <span class="lbl">Show</span>
  <button class="pill" id="t-undoc">Undocumented fields only</button>
  <button class="pill" id="t-empty">Hide empty objects</button>
  <div class="right">
    <button class="btn" id="b-open">Expand all</button>
    <button class="btn" id="b-close">Collapse all</button>
  </div>
</div>
<div class="wrap">
  <div class="summary" id="summary"></div>
  <div id="tree"></div>
</div>
<div class="toast" id="toast"></div>

<script>
const DD = {{DATA}};
// Colour per source family (matched on the start of the source label).
const SRC_COL = [
  [/^salesforce/i, "#00A1E0"], [/^looker/i, "#7C3AED"], [/^bigquery/i, "#EA4335"], [/^jira/i, "#2684FF"],
  [/^productboard/i, "#F59E0B"], [/^local input/i, "#14B8A6"], [/^derived/i, "#84CC16"],
  [/^snapshot/i, "#6B7280"], [/^immuter constants/i, "#111111"]
];
const srcColour = s => (SRC_COL.find(([re]) => re.test(s || "")) || [null, "#D1D5DB"])[1];
const srcFamily = s => !s ? "Unknown" : s.replace(/\s*\(.*$/, "").replace(/ \+ .*$/, "");

const esc = s => String(s === null || s === undefined ? "" : s).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
const state = { q: "", srcs: new Set(), undoc: false, hideEmpty: false, open: new Set(), closed: new Set(), sort: {} };

const OBJ = DD.objects;
const BY = Object.fromEntries(OBJ.map(o => [o.path, o]));
const kids = p => OBJ.filter(o => o.parent === p);
OBJ.forEach(o => (o.fields || []).forEach(f => { f.family = srcFamily(f.source); }));
const families = (() => {
  const c = {};
  OBJ.forEach(o => (o.fields || []).forEach(f => { c[f.family] = (c[f.family] || 0) + 1; }));
  OBJ.filter(o => o.kind === "value").forEach(o => { const k = srcFamily(o.source); c[k] = (c[k] || 0) + 1; });
  return Object.entries(c).sort((a, b) => b[1] - a[1]);
})();

function hl(text) {
  const t = esc(text);
  if (!state.q) return t;
  const re = new RegExp("(" + state.q.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + ")", "ig");
  return t.replace(re, "<mark>$1</mark>");
}
function fieldMatch(o, f) {
  if (state.srcs.size && !state.srcs.has(f.family)) return false;
  if (state.undoc && f.desc) return false;
  if (!state.q) return true;
  const q = state.q.toLowerCase();
  return [f.name, f.desc, f.source, f.example, f.type].some(v => v && String(v).toLowerCase().includes(q));
}
function objMatch(o) {
  if (!state.q) return false;
  const q = state.q.toLowerCase();
  return [o.path, o.desc, o.source, o.value].some(v => v && String(v).toLowerCase().includes(q));
}
// Fields of a table to show, or null when the object is filtered out.
function visibleFields(o) {
  const fs = o.fields || [];
  const filtering = state.q || state.srcs.size || state.undoc;
  if (state.hideEmpty && ((o.kind === "table" && !o.rows) || (o.kind === "value" && /^(empty list|NULL|\(empty\))$/.test(o.value || "")))) return null;
  if (o.kind === "value") {
    if (state.undoc) return o.desc ? null : [];
    if (state.srcs.size && !state.srcs.has(srcFamily(o.source))) return null;
    return (!state.q || objMatch(o)) ? [] : null;
  }
  if (!filtering) return fs;
  const hit = fs.filter(f => fieldMatch(o, f));
  if (hit.length) return hit;
  if (objMatch(o) && !state.srcs.size && !state.undoc) return fs;
  return null;
}
function groupVisible(o) {
  return kids(o.path).some(k => k.kind === "group" ? groupVisible(k) : visibleFields(k) !== null) ||
         (objMatch(o) && !state.srcs.size && !state.undoc);
}

function srcBadges(list) {
  const u = [...new Set(list.filter(Boolean))];
  return u.map(s => `<span class="src"><span class="sw" style="background:${srcColour(s)}"></span>${esc(s)}</span>`).join("");
}
function famOrder(list) {
  const c = {}; list.forEach(s => { c[s] = (c[s] || 0) + 1; });
  return Object.keys(c).sort((a, b) => c[b] - c[a]);
}
function fillHtml(p) {
  if (p === null || p === undefined) return "";
  const cls = p === 0 ? "zero" : p < 50 ? "lo" : "";
  return `<div class="fill">${p}%<div class="tr"><span class="${cls}" style="width:${p}%"></span></div></div>`;
}
function sortFields(o, fs) {
  const s = state.sort[o.path];
  if (!s) return fs;
  const v = f => s.col === "filled" || s.col === "distinct" ? (f[s.col] === null ? -1 : f[s.col]) : String(f[s.col] || "￿").toLowerCase();
  return fs.slice().sort((a, b) => { const x = v(a), y = v(b); return (x < y ? -1 : x > y ? 1 : 0) * s.dir; });
}
function rPathFor(o, field) {
  const ok = n => /^[A-Za-z.][A-Za-z0-9._]*$/.test(n) && !/^\.[0-9]/.test(n);
  const part = n => ok(n) ? "$" + n : "$`" + n + "`";
  return o.r_path + (field === undefined ? "" : part(field));
}

function nodeHtml(o, filtering) {
  if (o.kind === "group") {
    if (!groupVisible(o)) return "";
    const open = state.open.has(o.path) || (filtering && !state.closed.has(o.path));
    const inner = kids(o.path).map(k => nodeHtml(k, filtering)).join("");
    const nTables = OBJ.filter(x => x.path.startsWith(o.path + ".") && x.kind === "table").length;
    return `<div class="node group${open ? " open" : ""}" data-path="${esc(o.path)}">
      <div class="head"><span class="chev">&#9654;</span><span class="path">${hl(o.path)}</span>
        <span class="kind">list</span><span class="dim">${kids(o.path).length} items &middot; ${nTables} tables</span>
        <span class="desc-s">${hl(o.desc || "")}</span><div class="srcs">${srcBadges([o.source])}</div></div>
      <div class="body">${o.desc ? `<div class="desc">${hl(o.desc)}</div>` : ""}
        <div class="rpath" data-copy="${esc(o.r_path)}">${esc(o.r_path)}<span class="cp">COPY</span></div>${inner}</div></div>`;
  }
  const fs = visibleFields(o);
  if (fs === null) return "";
  const open = state.open.has(o.path) || (filtering && o.kind !== "value" && !state.closed.has(o.path));
  const pre = o.parent ? `<span class="pre">${esc(o.parent)}.</span>` : "";
  const name = o.parent ? o.path.slice(o.parent.length + 1) : o.path;
  if (o.kind === "value") {
    return `<div class="node${state.open.has(o.path) ? " open" : ""}" data-path="${esc(o.path)}">
      <div class="head"><span class="chev">&#9654;</span><span class="path">${pre}${hl(name)}</span><span class="kind">${esc(o.type)}</span>
        <span class="dim">${esc(o.value_short || "")}</span><span class="desc-s">${hl(o.desc || "")}</span><div class="srcs">${srcBadges([o.source])}</div></div>
      <div class="body">${o.desc ? `<div class="desc">${hl(o.desc)}</div>` : `<div class="desc nodesc">No description yet (data/input/op_list_dictionary.csv)</div>`}
        <div class="rpath" data-copy="${esc(o.r_path)}">${esc(o.r_path)}<span class="cp">COPY</span></div>
        <div class="val">${hl(o.value || "")}</div></div></div>`;
  }
  const all = o.fields || [];
  const nDoc = all.filter(f => f.desc).length;
  const s = state.sort[o.path] || {};
  const th = (col, label, cls) => `<th class="${cls || ""}" data-sort="${col}">${label}${s.col === col ? (s.dir === 1 ? " ↑" : " ↓") : ""}</th>`;
  const rows = sortFields(o, fs).map(f => `<tr class="${state.q && fieldMatch(o, f) && filtering ? "hit" : ""}">
      <td class="f" data-copy="${esc(rPathFor(o, f.name))}" title="Click to copy ${esc(rPathFor(o, f.name))}">${hl(f.name)}</td>
      <td>${srcBadges([f.source])}</td>
      <td class="t">${esc(f.type)}</td>
      <td class="num">${fillHtml(f.filled)}</td>
      <td class="num">${f.distinct === null ? "" : f.distinct}</td>
      <td class="ex" title="${esc(f.example || "")}">${hl(f.example || "")}</td>
      <td class="d">${f.desc ? hl(f.desc) : `<span class="nodesc">No description yet</span>`}</td></tr>`).join("");
  const fieldNote = fs.length < all.length ? `${fs.length} of ${all.length} fields` : `${all.length} fields`;
  return `<div class="node${open ? " open" : ""}" data-path="${esc(o.path)}">
    <div class="head"><span class="chev">&#9654;</span><span class="path">${pre}${hl(name)}</span><span class="kind">table</span>
      <span class="dim">${o.rows.toLocaleString()} rows &middot; ${fieldNote}${all.length ? ` &middot; ${Math.round(100 * nDoc / all.length)}% documented` : ""}</span>
      <span class="desc-s">${hl(o.desc || "")}</span><div class="srcs">${srcBadges(famOrder(all.length ? all.map(f => f.family) : [srcFamily(o.source)]))}</div></div>
    <div class="body">${o.desc ? `<div class="desc">${hl(o.desc)}</div>` : `<div class="desc nodesc">No description yet (data/input/op_list_dictionary.csv)</div>`}
      <div class="rpath" data-copy="${esc(o.r_path)}">${esc(o.r_path)}<span class="cp">COPY</span></div>
      ${all.length ? `<div class="tw"><table><thead><tr>${th("name", "Field")}${th("source", "Source")}${th("type", "Type")}${th("filled", "Filled", "num")}${th("distinct", "Distinct", "num")}${th("example", "Example")}${th("desc", "Description")}</tr></thead><tbody>${rows}</tbody></table></div>`
                   : `<div class="empty">No rows in this run, so no fields to list.</div>`}
    </div></div>`;
}

function render() {
  const filtering = !!(state.q || state.srcs.size || state.undoc);
  const html = OBJ.filter(o => !o.parent).map(o => nodeHtml(o, filtering)).join("");
  document.getElementById("tree").innerHTML = html || `<div class="empty">Nothing matches.</div>`;
  const tables = OBJ.filter(o => o.kind === "table"), fields = tables.reduce((s, o) => s + (o.fields || []).length, 0);
  const doc = tables.reduce((s, o) => s + (o.fields || []).filter(f => f.desc).length, 0);
  document.getElementById("summary").textContent =
    `${OBJ.filter(o => !o.parent).length} top-level objects · ${tables.length} tables · ${fields.toLocaleString()} fields · ${Math.round(100 * doc / Math.max(1, fields))}% of fields described` +
    (filtering ? " · filtered: matching objects are expanded" : "");
  document.getElementById("t-undoc").classList.toggle("on", state.undoc);
  document.getElementById("t-empty").classList.toggle("on", state.hideEmpty);
  document.querySelectorAll("#src-bar [data-src]").forEach(b => b.classList.toggle("on", state.srcs.has(b.dataset.src)));
}

function toast(msg) {
  const t = document.getElementById("toast"); t.textContent = msg; t.classList.add("on");
  clearTimeout(toast._t); toast._t = setTimeout(() => t.classList.remove("on"), 1400);
}
function copy(text) {
  const done = () => toast("Copied " + text);
  if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(text).then(done, done);
  else done();
}

document.getElementById("meta").textContent = [DD.updated && "op_list updated " + DD.updated, DD.generated && "dictionary built " + DD.generated].filter(Boolean).join(" · ");
document.getElementById("src-bar").innerHTML = `<span class="lbl">Source</span>` + families.map(([s, n]) =>
  `<button class="pill" data-src="${esc(s)}"><span class="sw" style="background:${srcColour(s)}"></span>${esc(s)}<span class="n">${n}</span></button>`).join("");
document.getElementById("src-bar").onclick = e => {
  const b = e.target.closest("[data-src]"); if (!b) return;
  state.srcs.has(b.dataset.src) ? state.srcs.delete(b.dataset.src) : state.srcs.add(b.dataset.src); render();
};
document.getElementById("q").oninput = e => { state.q = e.target.value.trim(); state.closed.clear(); render(); };
document.getElementById("t-undoc").onclick = () => { state.undoc = !state.undoc; render(); };
document.getElementById("t-empty").onclick = () => { state.hideEmpty = !state.hideEmpty; render(); };
document.getElementById("b-open").onclick = () => { state.closed.clear(); OBJ.forEach(o => state.open.add(o.path)); render(); };
document.getElementById("b-close").onclick = () => { state.open.clear(); OBJ.forEach(o => state.closed.add(o.path)); render(); };
document.getElementById("tree").onclick = e => {
  const c = e.target.closest("[data-copy]");
  if (c) { copy(c.dataset.copy); return; }
  const th = e.target.closest("th[data-sort]");
  if (th) {
    const p = th.closest(".node").dataset.path, col = th.dataset.sort, s = state.sort[p];
    state.sort[p] = s && s.col === col ? { col, dir: -s.dir } : { col, dir: col === "filled" || col === "distinct" ? -1 : 1 };
    render(); return;
  }
  const h = e.target.closest(".head");
  if (h) {
    const node = h.parentElement, p = node.dataset.path;
    node.classList.toggle("open");
    if (node.classList.contains("open")) { state.open.add(p); state.closed.delete(p); }
    else { state.open.delete(p); state.closed.add(p); }
  }
};
render();
</script>
</body></html>)---"
