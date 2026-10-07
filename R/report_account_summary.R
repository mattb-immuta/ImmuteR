#' Render an account summary as HTML, RTF, or Markdown.
#'
#' Converts the structured list from \code{output_account_summary()} into a
#' formatted file. HTML can be opened in a browser and copied into any rich-text
#' tool (Cmd+A, Cmd+C, paste). RTF can be opened in Word or TextEdit and copied
#' from there. Markdown is useful for GitHub, Notion, or version-controlled docs.
#'
#' @param account_summary Named list returned by \code{output_account_summary()}.
#' @param output_path Character. Full file path for the output file. The
#'   extension should match the chosen \code{output} format
#'   (\code{.html}, \code{.rtf}, or \code{.md}). The parent directory is
#'   created automatically if it does not exist.
#' @param output Character. Output format: \code{"html"} (default),
#'   \code{"rtf"}, or \code{"markdown"}.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' \dontrun{
#'   s <- output_account_summary("Cigna", op_list)
#'   report_account_summary(s, "output/account_briefs/Cigna.html",    output = "html")
#'   report_account_summary(s, "output/account_briefs/Cigna.rtf",     output = "rtf")
#'   report_account_summary(s, "output/account_briefs/Cigna.md",      output = "markdown")
#' }
#' @export
report_account_summary <- function(account_summary,
                                   output_path,
                                   output = c("html", "rtf", "markdown")) {

  output <- match.arg(output)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  content <- switch(output,
    html     = build_report_html(account_summary),
    rtf      = build_report_rtf(account_summary),
    markdown = build_report_md(account_summary)
  )

  writeLines(content, output_path)
  message(".. account summary written to ", output_path)
  invisible(output_path)
}


# =============================================================================
# HTML builder
# =============================================================================

# The account summary as an HTML fragment (<body>...</body>): overview,
# architecture, open renewals, expansion pipeline. build_report_html() wraps it
# in a styled page.
build_summary_body_html <- function(account_summary) {

  acct <- account_summary$account
  opps <- account_summary$opportunities
  arch <- account_summary$architecture

  h <- function(...) paste0(...)

  # ── Account Overview --------------------------------------------------------
  overview_items <- paste0(collapse = "",
    html_li("Contract ARR",  fmt_usd(acct$contract_arr)),
    html_li("Region",        acct$region),
    html_li("Account Owner", acct$account_owner),
    html_li("Field CTO",     acct$field_cto),
    html_li("CSM",           acct$csm)
  )

  # ── Architecture ------------------------------------------------------------
  is_saas <- isTRUE(arch$is_saas)
  deploy_item <- if (is_saas) {
    html_li("Deployment", "\u2705 SaaS")
  } else {
    unsupported <- isTRUE(arch$unsupported_version %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    paste0(
      html_li("Deployment", "Self-Hosted"),
      html_li("Unsupported Version",
               if (unsupported) "\u26a0\ufe0f Yes \u2014 upgrade recommended"
               else "\u2705 No")
    )
  }

  if (nrow(arch$products) == 0) {
    product_items <- "<li><em>No active order products found.</em></li>"
  } else {
    product_items <- paste0(apply(arch$products, 1, function(r) {
      paste0("<li>", html_escape(r[["Product Name"]]),
             " \u00b7 Qty: ", html_escape(r[["Quantity"]]), "</li>")
    }), collapse = "")
  }

  # ── Renewals ----------------------------------------------------------------
  if (nrow(opps$renewals) == 0) {
    renewal_block <- "<li><em>No open renewals.</em></li>"
  } else {
    renewal_block <- paste0(apply(opps$renewals, 1, function(r) {
      paste0(
        "<li><strong>", html_escape(safe_val(r[["Opportunity Name"]])), "</strong>",
        "<ul>",
        html_li("Amount",            fmt_usd(r[["Amount"]])),
        html_li("Contract End Date", fmt_date(r[["Current Contract End Date"]])),
        html_li("Churn Quarter",     safe_val(r[["Churn Quarter"]])),
        "</ul></li>"
      )
    }), collapse = "")
  }

  # ── Expansions --------------------------------------------------------------
  if (nrow(opps$expansions) == 0) {
    expansion_block <- "<li><em>No open expansion opportunities.</em></li>"
  } else {
    expansion_block <- paste0(apply(opps$expansions, 1, function(r) {
      paste0(
        "<li><strong>", html_escape(safe_val(r[["Opportunity Name"]])), "</strong>",
        "<ul>",
        html_li("Owner",         safe_val(r[["Opportunity Owner"]])),
        html_li("Amount",        fmt_usd(r[["Amount"]])),
        html_li("Fiscal Period", safe_val(r[["Fiscal Period"]])),
        "</ul></li>"
      )
    }), collapse = "")
  }

  # ── Assemble ----------------------------------------------------------------
  paste0(
    "<body>",
    "<em>Updated: ", format(Sys.Date(), "%B %d, %Y"), "</em>",

    "<h2>\U1F4CB Account Overview</h2>",
    "<ul>", overview_items, "</ul>",

    "<h2>\U1F3D7\UFE0F Architecture</h2>",
    "<ul>", deploy_item, "</ul>",
    "<strong>Active Products</strong>",
    "<ul>", product_items, "</ul>",

    "<h2>\U1F504 Open Renewals</h2>",
    "<ul>", renewal_block, "</ul>",

    "<h2>\U1F4C8 Expansion Pipeline</h2>",
    "<ul>", expansion_block, "</ul>",

    "</body>"
  )
}


build_report_html <- function(account_summary) {
  inner <- build_summary_body_html(account_summary)
  paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'><style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
    "max-width:720px;margin:40px auto;padding:0 24px;color:#1a1a1a;line-height:1.6;}",
    "h1{font-size:1.6em;margin-bottom:2px;}",
    "h2{font-size:1.1em;margin-top:28px;margin-bottom:8px;",
    "border-bottom:1px solid #e0e0e0;padding-bottom:4px;}",
    "ul{margin:4px 0 12px 0;padding-left:20px;}",
    "li{margin:3px 0;}",
    "em{color:#666;font-size:0.9em;}",
    "strong{font-weight:600;}",
    "</style></head><body>",
    inner,
    "</body></html>"
  )
}


# =============================================================================
# RTF builder
# =============================================================================

build_report_rtf <- function(account_summary) {

  acct <- account_summary$account
  opps <- account_summary$opportunities
  arch <- account_summary$architecture

  lines <- c(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0",
    "{\\fonttbl{\\f0\\fswiss\\fcharset0 Helvetica;}}",
    "{\\colortbl;\\red26\\green26\\blue26;\\red102\\green102\\blue102;}",
    "\\f0\\fs24\\cf1",

    # Title
    paste0("\\pard\\sb100\\sa100{\\b\\fs32 ", rtf_escape(acct$account_name), "}\\par"),
    paste0("\\pard\\sa200{\\i\\cf2 Updated: ", format(Sys.Date(), "%B %d, %Y"), "}\\par"),

    # Account Overview
    rtf_heading("Account Overview"),
    rtf_item("Contract ARR",  fmt_usd(acct$contract_arr)),
    rtf_item("Region",        acct$region),
    rtf_item("Account Owner", acct$account_owner),
    rtf_item("Field CTO",     acct$field_cto),
    rtf_item("CSM",           acct$csm),
    "\\pard\\par",

    # Architecture
    rtf_heading("Architecture")
  )

  if (isTRUE(arch$is_saas)) {
    lines <- c(lines, rtf_item("Deployment", "SaaS"))
  } else {
    unsupported <- isTRUE(arch$unsupported_version %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    lines <- c(lines,
      rtf_item("Deployment", "Self-Hosted"),
      rtf_item("Unsupported Version", if (unsupported) "Yes - upgrade recommended" else "No")
    )
  }

  lines <- c(lines, "\\pard\\sa100{\\b Active Products}\\par")
  if (nrow(arch$products) == 0) {
    lines <- c(lines, "\\pard\\li400 No active order products found.\\par")
  } else {
    for (i in seq_len(nrow(arch$products))) {
      r <- arch$products[i, ]
      lines <- c(lines,
        paste0("\\pard\\fi-200\\li400 \\bullet  ",
               rtf_escape(r[["Product Name"]]), " - Qty: ", r[["Quantity"]], "\\par")
      )
    }
  }
  lines <- c(lines, "\\pard\\par")

  # Renewals
  lines <- c(lines, rtf_heading("Open Renewals"))
  if (nrow(opps$renewals) == 0) {
    lines <- c(lines, "\\pard\\li400 No open renewals.\\par")
  } else {
    for (i in seq_len(nrow(opps$renewals))) {
      r <- opps$renewals[i, ]
      lines <- c(lines,
        paste0("\\pard\\sb80\\sa0{\\b ", rtf_escape(safe_val(r[["Opportunity Name"]])), "}\\par"),
        rtf_item("Amount",            fmt_usd(r[["Amount"]])),
        rtf_item("Contract End Date", fmt_date(r[["Current Contract End Date"]])),
        rtf_item("Churn Quarter",     safe_val(r[["Churn Quarter"]])),
        "\\pard\\sa80\\par"
      )
    }
  }

  # Expansions
  lines <- c(lines, rtf_heading("Expansion Pipeline"))
  if (nrow(opps$expansions) == 0) {
    lines <- c(lines, "\\pard\\li400 No open expansion opportunities.\\par")
  } else {
    for (i in seq_len(nrow(opps$expansions))) {
      e <- opps$expansions[i, ]
      lines <- c(lines,
        paste0("\\pard\\sb80\\sa0{\\b ", rtf_escape(safe_val(e[["Opportunity Name"]])), "}\\par"),
        rtf_item("Owner",         safe_val(e[["Opportunity Owner"]])),
        rtf_item("Amount",        fmt_usd(e[["Amount"]])),
        rtf_item("Fiscal Period", safe_val(e[["Fiscal Period"]])),
        "\\pard\\sa80\\par"
      )
    }
  }

  c(lines, "}")
}

rtf_heading <- function(text) {
  paste0("\\pard\\sb240\\sa80{\\b\\fs26 ", rtf_escape(text), "}\\par")
}

rtf_item <- function(label, value) {
  paste0("\\pard\\fi-200\\li400 {\\b ", rtf_escape(label), ":} ",
         rtf_escape(safe_val(value)), "\\par")
}

rtf_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{",  "\\\\{",    x)
  x <- gsub("\\}",  "\\\\}",    x)
  # Replace common unicode with RTF equivalents
  x <- gsub("\u2014", "\\\\emdash ", x)
  x <- gsub("\u2013", "\\\\endash ", x)
  x <- gsub("\u00b7", "\\\\bullet ",  x)
  x
}


# =============================================================================
# Markdown builder
# =============================================================================

build_report_md <- function(account_summary) {

  acct <- account_summary$account
  opps <- account_summary$opportunities
  arch <- account_summary$architecture

  lines <- c(
    paste0("# ", acct$account_name),
    paste0("*Updated: ", format(Sys.Date(), "%B %d, %Y"), "*"),
    "",
    "## \U1F4CB Account Overview",
    "",
    build_field("Contract ARR",  fmt_usd(acct$contract_arr)),
    build_field("Region",        acct$region),
    build_field("Account Owner", acct$account_owner),
    build_field("Field CTO",     acct$field_cto),
    build_field("CSM",           acct$csm),
    "",
    "## \U1F3D7\UFE0F Architecture",
    ""
  )

  if (isTRUE(arch$is_saas)) {
    lines <- c(lines, build_field("Deployment", "\U2705 SaaS"), "")
  } else {
    unsupported <- isTRUE(arch$unsupported_version %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    lines <- c(lines,
      build_field("Deployment", "Self-Hosted"),
      build_field("Unsupported Version",
                  if (unsupported) "\U26A0\UFE0F Yes \u2014 upgrade recommended"
                  else "\U2705 No"),
      ""
    )
  }

  lines <- c(lines, "**Active Products**", "")
  if (nrow(arch$products) == 0) {
    lines <- c(lines, "*No active order products found.*", "")
  } else {
    lines <- c(lines,
      apply(arch$products, 1, function(r) {
        paste0("- ", r[["Product Name"]], " \u00B7 Qty: ", r[["Quantity"]])
      }), ""
    )
  }

  lines <- c(lines, "## \U1F504 Open Renewals", "")
  if (nrow(opps$renewals) == 0) {
    lines <- c(lines, "*No open renewals.*", "")
  } else {
    for (i in seq_len(nrow(opps$renewals))) {
      r <- opps$renewals[i, ]
      lines <- c(lines,
        paste0("**", safe_val(r[["Opportunity Name"]]), "**"),
        build_field("Amount",            fmt_usd(r[["Amount"]])),
        build_field("Contract End Date", fmt_date(r[["Current Contract End Date"]])),
        build_field("Churn Quarter",     safe_val(r[["Churn Quarter"]])),
        ""
      )
    }
  }

  lines <- c(lines, "## \U1F4C8 Expansion Pipeline", "")
  if (nrow(opps$expansions) == 0) {
    lines <- c(lines, "*No open expansion opportunities.*", "")
  } else {
    for (i in seq_len(nrow(opps$expansions))) {
      e <- opps$expansions[i, ]
      lines <- c(lines,
        paste0("**", safe_val(e[["Opportunity Name"]]), "**"),
        build_field("Owner",         safe_val(e[["Opportunity Owner"]])),
        build_field("Amount",        fmt_usd(e[["Amount"]])),
        build_field("Fiscal Period", safe_val(e[["Fiscal Period"]])),
        ""
      )
    }
  }

  lines
}
