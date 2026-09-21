# Internal utility helpers shared across ImmuteR reporting functions.
# These are NOT exported — they are for use within the package only.


# --- Value safety -------------------------------------------------------------

#' Coerce a value to character, returning an em-dash placeholder for
#' missing/empty values.
#' @noRd
safe_val <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- as.character(x)
  ifelse(is.na(x) | trimws(x) == "", "\u2014", x)
}


# --- Formatting helpers -------------------------------------------------------

#' Format a numeric value as USD currency, e.g. "$1,234,567".
#' Returns an em-dash for missing/non-numeric values.
#' @noRd
fmt_usd <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x[1]))
  if (is.na(x)) return("\u2014")
  scales::dollar(x)
}

#' Format a Date or date string as "Month DD, YYYY".
#' Returns an em-dash for missing values.
#'
#' Absent columns are the common case here: \code{df[["No Such Column"]]}
#' returns \code{NULL}, and an unparseable string makes \code{as.Date()} throw
#' rather than return \code{NA}. Both fold into the em-dash.
#' @noRd
fmt_date <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  d <- suppressWarnings(tryCatch(as.Date(x[1]), error = function(e) NA))
  if (length(d) == 0 || is.na(d)) return("\u2014")
  format(d, "%B %d, %Y")
}

#' Format a number of days as "X.X years (N days)".
#' Returns an em-dash for missing values.
#' @noRd
fmt_days <- function(x) {
  if (is.null(x) || length(x) == 0) return("\u2014")
  x <- suppressWarnings(as.numeric(x[1]))
  if (is.na(x)) return("\u2014")
  paste0(round(x / 365.25, 1), " years (", x, " days)")
}

#' Format a boolean-ish value as a checkmark "Yes" or plain "No".
#' Returns an em-dash for missing/empty values.
#' @noRd
fmt_bool <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || x == ""))) return("\u2014")
  if (x %in% c(TRUE, "Yes", "TRUE", "true", "yes")) return("\U2705 Yes")
  if (x %in% c(FALSE, "No", "FALSE", "false", "no")) return("No")
  as.character(x)
}

#' Format a risk level string with an emoji prefix.
#' Returns an em-dash for missing/empty values.
#' @noRd
fmt_risk <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || x == ""))) return("\u2014")
  switch(tolower(as.character(x)),
    "high"     = "\U26A0\UFE0F High",
    "moderate" = "\U1F7E1 Moderate",
    "low"      = "\U2705 Low",
    as.character(x)
  )
}


# --- Consumption ratio --------------------------------------------------------

#' Registered users over licensed users, with the ELA rule.
#'
#' A licensed-user count of \code{0} is not a division by zero — it is an
#' Enterprise License Agreement, where seats are unlimited and there is no
#' denominator to divide by. Those accounts are consuming everything they are
#' entitled to by definition, so the ratio is \code{1}. Until this rule existed
#' they came through as \code{NA}, which capped General Motors, JPMorgan Chase
#' and RTX (29k, 256k and 6k registered users apiece) at red on the PBAC rating
#' purely because the figure was missing.
#'
#' An ELA with nobody registered is the one exception: \code{0} registered users
#' is \code{0} consumption, unlimited seats or not.
#'
#' A \emph{missing} licensed count is a different thing entirely and stays
#' \code{NA} — absent data, not an unlimited contract. The PBAC rating relies on
#' that distinction (see \code{\link{op_list_pbac_adoption}}), so the two cases
#' must not be collapsed.
#'
#' Vectorised, and the single definition every caller uses — the account cards,
#' the health tiles, the slide payload, the consumption table and the PBAC
#' rating — so one account cannot show two different ratios in two places.
#' @noRd
consumption_ratio <- function(registered, licensed) {
  reg <- suppressWarnings(as.numeric(registered))
  lic <- suppressWarnings(as.numeric(licensed))
  ifelse(is.na(reg) | is.na(lic), NA_real_,
         ifelse(lic == 0, ifelse(reg > 0, 1, 0), reg / lic))
}


# --- PBAC adoption helpers ----------------------------------------------------
# Shared by the HTML account card and the PNG health summary so both read the
# same row and name the features identically.

#' One account's row from \code{op_list$consumption$pbac_adoption}, or NULL.
#' Names are trimmed on both sides: the Looker-derived frame is built from
#' \code{accounts$`Account Name`} as supplied, which can carry stray spaces.
#' @noRd
pbac_row <- function(op_list, account_name) {
  df <- op_list$consumption$pbac_adoption
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
      !("Account Name" %in% names(df))) {
    return(NULL)
  }
  hit <- df[!is.na(df$`Account Name`) &
              trimws(as.character(df$`Account Name`)) == trimws(as.character(account_name)),
            , drop = FALSE]
  if (nrow(hit) == 0) NULL else hit[1, ]
}

#' Display names for the PBAC complexity signals an account has earned.
#'
#' Multi-platform is deliberately absent: the adoption block lists platforms on
#' their own line, so repeating it as a feature says the same thing twice.
#' Returns \code{character(0)} when the row is missing or nothing is set, which
#' the callers render as "None".
#' @noRd
pbac_feature_labels <- function(row) {
  if (is.null(row)) return(character(0))

  labels <- c(
    sdd          = "SDD",
    domains      = "Domains",
    guardrail    = "Guardrail policies",
    dns_policies = "Data + subscription policies"
  )

  on <- vapply(names(labels), function(f) {
    isTRUE(suppressWarnings(as.numeric(row[[f]][1])) > 0)
  }, logical(1))

  unname(labels[on])
}

#' The four adoption rows for one account, as label/value/word/source.
#'
#' PBAC and Request are rated from Looker — PBAC from
#' \code{op_list$consumption$pbac_adoption}, Request from the stage tile the
#' account appears on in dashboard 1009 — and fall back to their Salesforce
#' picklist when those are absent, so an SFDC-only \code{op_list} still
#' renders. Comply and Agentic Governance are always the picklists.
#'
#' \code{value} is what the colour is read from; \code{word} is the text that
#' sits beside the dot. They differ because a picklist value is a whole
#' sentence (so the colour word is the only sane label), while a derived value
#' already names its state — "Retained", "Moderate adoption" — and the dot is
#' then carrying severity alone.
#'
#' Shared by \code{generate_slide_tile()} and \code{account_slide_payload()};
#' the HTML card does the same thing in JS, against the same payload fields.
#' @noRd
adoption_rows <- function(a_data, p_data) {

  pick <- function(col) {
    v <- a_data[[col]]
    if (is.null(v) || length(v) == 0) NA else v[1]
  }

  has_pbac  <- !is.null(p_data)
  pbac_val  <- if (has_pbac) p_data$pbac_adoption[1] else pick("PBAC Adoption")
  pbac_word <- if (has_pbac) pbac_rating_label(pbac_val) else .tile_stoplight_word(pbac_val)

  req_looker <- pick("request_adoption_looker")
  req_stage  <- pick("request_adoption_looker_stage")
  has_req    <- !is.na(req_looker) && nzchar(trimws(as.character(req_looker)))

  req_val  <- if (has_req) req_looker else pick("Request Adoption")
  req_word <- if (has_req && !is.na(req_stage)) {
    s <- trimws(as.character(req_stage))
    paste0(toupper(substring(s, 1, 1)), substring(s, 2))
  } else {
    .tile_stoplight_word(req_val)
  }

  list(
    list(label = "PBAC", value = pbac_val, word = pbac_word,
         source = if (has_pbac) "looker" else "salesforce"),
    list(label = "Comply", value = pick("Comply Adoption"),
         word = .tile_stoplight_word(pick("Comply Adoption")),
         source = "salesforce"),
    list(label = "Request", value = req_val, word = req_word,
         source = if (has_req) "looker" else "salesforce"),
    list(label = "Agentic", value = pick("Agentic Governance Adoption"),
         word = .tile_stoplight_word(pick("Agentic Governance Adoption")),
         source = "salesforce")
  )
}

#' Leading colour word of a "<colour>: <label>" rating, lowercased, or "".
#' @noRd
pbac_rating_colour <- function(x) {
  v <- tolower(trimws(safe_val(x)[1]))
  m <- regmatches(v, regexpr("^(green|amber|yellow|red|black)", v))
  if (length(m) == 0) "" else sub("^yellow$", "amber", m)
}

#' Label half of a "<colour>: <label>" rating, sentence-cased, or an em-dash.
#' \code{"amber: moderate adoption"} becomes \code{"Moderate adoption"}.
#' @noRd
pbac_rating_label <- function(x) {
  v <- trimws(safe_val(x)[1])
  if (v == "—") return(v)
  lab <- trimws(sub("^(?i)(green|amber|yellow|red|black)\\s*:\\s*", "", v, perl = TRUE))
  if (!nzchar(lab)) return("—")
  paste0(toupper(substr(lab, 1, 1)), substr(lab, 2, nchar(lab)))
}


# --- Markdown field helpers ---------------------------------------------------

#' Build a markdown bullet field: "- **Label:** value".
#' Substitutes an em-dash for blank/NA values.
#' @noRd
build_field <- function(label, value) {
  val <- if (is.null(value) || length(value) == 0 ||
             (length(value) == 1 && (is.na(value) || as.character(value) == ""))) {
    "\u2014"
  } else {
    as.character(value)
  }
  paste0("- **", label, ":** ", val)
}

#' Alias for \code{build_field}; used in markdown report builders.
#' @noRd
asana_field <- build_field


# --- HTML helpers -------------------------------------------------------------

#' Escape special HTML characters in a string.
#' @noRd
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Build an Asana-compatible HTML list item: "<li><strong>Label:</strong> value</li>".
#' @noRd
asana_li <- function(label, value) {
  paste0("<li><strong>", label, ":</strong> ", safe_val(value), "</li>")
}
