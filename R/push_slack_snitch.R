#' Ask CSMs in Slack to update stale Next Steps (and overdue QBR/EBRs)
#'
#' Takes the result of \code{\link{report_next_step_snitch}} and posts one
#' Slack message per flagged account to that account's internal channel
#' (\code{slack_channel_id} / \code{slack_channel} on \code{op_list$accounts},
#' from \code{data/input/slack_channels.csv}), @-mentioning the account's CSM.
#'
#' The message text comes from a Markdown template,
#' \code{data/input/snitch_slack_template.md} by default — edit that file, not
#' this function, to change the wording or the Next Step format CSMs are asked
#' to follow. The template is ordinary Markdown with \code{{{placeholders}}} and
#' optional blocks; its header comment lists them. \code{**bold**},
#' \code{[text](url)}, \code{- } bullets and headings are converted to Slack's
#' own formatting when sent; code blocks are left as they are.
#'
#' \strong{Dry run by default.} With \code{dry_run = TRUE} nothing is posted:
#' every message is written to \code{preview_path} for review and returned.
#' Re-run with \code{dry_run = FALSE} to send. In an interactive session you are
#' asked to confirm before anything is posted.
#'
#' \strong{Setup for sending:}
#' \itemize{
#'   \item A Slack app with a bot token (scope \code{chat:write}; add
#'     \code{chat:write.public} to post in public channels the bot hasn't
#'     joined) saved as \code{SLACK_BOT_TOKEN} in \code{.Renviron}. For private
#'     channels, invite the bot to the channel.
#'   \item \code{slack_channel_id} filled in for each account in
#'     \code{data/input/slack_channels.csv} (a channel name also works for
#'     public channels).
#'   \item Each CSM's Slack member ID in \code{data/input/slack_users.csv}, so
#'     the message pings them. Without one the CSM's name is used as plain
#'     text, which does not notify them.
#' }
#'
#' Accounts with no channel are skipped (status \code{"skipped: no channel"})
#' and listed in a warning.
#'
#' @param snitch List returned by \code{\link{report_next_step_snitch}}.
#' @param op_list Named list from \code{enrich_opportunities()}; uses
#'   \code{op_list$accounts} for each account's channel.
#' @param include_saas Logical. Also mention stale SaaS migration notes
#'   (\code{snitch$saas_notes}), and message accounts that only have that
#'   issue. Default \code{TRUE}.
#' @param include_qbr Logical. Also mention an overdue QBR/EBR
#'   (\code{snitch$no_recent_qbr}), and message accounts that only have that
#'   issue. Default \code{TRUE}.
#' @param accounts Character vector of account names to limit the run to, e.g.
#'   for a test post. Default \code{NULL} (every flagged account).
#' @param dry_run Logical. \code{TRUE} (default) previews only; \code{FALSE}
#'   posts to Slack.
#' @param template_path Character. The message template.
#' @param users_path Character. CSV mapping \code{Customer Success Manager} to
#'   \code{slack_user_id}.
#' @param preview_path Character. Where the dry-run preview is written.
#' @param token Character. Slack bot token. Defaults to
#'   \code{Sys.getenv("SLACK_BOT_TOKEN")}; only needed when \code{dry_run =
#'   FALSE}.
#' @param confirm Logical. Ask before posting. Defaults to
#'   \code{interactive()}.
#'
#' @return Invisibly, a data frame with one row per flagged account:
#'   \code{Account Name}, \code{Customer Success Manager}, \code{channel},
#'   \code{renewals} (count), \code{saas} (logical), \code{qbr} (logical), \code{status}
#'   (\code{"dry run"}, \code{"sent"}, \code{"skipped: no channel"} or
#'   \code{"error: ..."}), \code{ts} (Slack message timestamp when sent) and
#'   \code{message} (the Slack-formatted text).
#' @examples
#' \dontrun{
#'   snitch <- report_next_step_snitch(op_list)
#'   push_slack_snitch(snitch, op_list)                        # preview only
#'   push_slack_snitch(snitch, op_list, accounts = "Cigna",
#'                     dry_run = FALSE)                        # one test post
#'   push_slack_snitch(snitch, op_list, dry_run = FALSE)       # send all
#' }
#' @export
push_slack_snitch <- function(snitch,
                              op_list,
                              include_saas  = TRUE,
                              include_qbr   = TRUE,
                              accounts      = NULL,
                              dry_run       = TRUE,
                              template_path = "data/input/snitch_slack_template.md",
                              users_path    = "data/input/slack_users.csv",
                              preview_path  = "output/snitch_slack_preview.md",
                              token         = Sys.getenv("SLACK_BOT_TOKEN"),
                              confirm       = interactive()) {

  if (!is.list(snitch) || !all(c("next_steps", "no_recent_qbr") %in% names(snitch))) {
    stop("`snitch` must be the list returned by report_next_step_snitch().")
  }
  if (!file.exists(template_path)) stop("Template not found: ", template_path)
  template <- paste(readLines(template_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  ns  <- snitch$next_steps
  qbr <- if (isTRUE(include_qbr)) snitch$no_recent_qbr else snitch$no_recent_qbr[0, , drop = FALSE]
  # saas_notes is absent from a snitch run before that check existed.
  saas <- snitch$saas_notes
  if (is.null(saas) || !isTRUE(include_saas)) {
    saas <- data.frame(`Account Name` = character(0), check.names = FALSE)
  }
  as_of <- attr(snitch, "as_of") %||% Sys.Date()

  # Channels: from op_list$accounts, or straight from the CSV for a snapshot
  # saved before the channel columns existed.
  accts <- op_list$accounts
  if (!all(c("slack_channel", "slack_channel_id") %in% names(accts))) {
    accts <- .accounts_add_slack_channels(accts, op_list_slack_channels())
  }
  user_ids <- .slack_user_ids(users_path)

  flagged <- unique(c(as.character(ns$`Account Name`), as.character(saas$`Account Name`),
                      as.character(qbr$`Account Name`)))
  if (!is.null(accounts)) flagged <- intersect(flagged, accounts)
  if (length(flagged) == 0) {
    message(".. push_slack_snitch: nothing to send")
    return(invisible(data.frame()))
  }

  rows <- lapply(flagged, function(acct) {
    r <- ns[ns$`Account Name` == acct, , drop = FALSE]
    q <- qbr[qbr$`Account Name` == acct, , drop = FALSE]
    m <- saas[saas$`Account Name` == acct, , drop = FALSE]

    csm <- c(as.character(r$`Customer Success Manager`),
             as.character(m$`Customer Success Manager`),
             as.character(q$`Customer Success Manager`))
    csm <- csm[!is.na(csm) & nzchar(csm)][1]

    a <- accts[trimws(as.character(accts$`Account Name`)) == trimws(acct), , drop = FALSE]
    ch_id   <- if (nrow(a)) a$slack_channel_id[1] else NA_character_
    ch_name <- if (nrow(a)) a$slack_channel[1]    else NA_character_
    channel <- if (!is.na(ch_id) && nzchar(ch_id)) ch_id else
               if (!is.na(ch_name) && nzchar(ch_name)) ch_name else NA_character_

    md <- .snitch_render(template, acct, csm, user_ids, r, q, as_of, m)
    data.frame(`Account Name` = acct, `Customer Success Manager` = csm %||% NA_character_,
               channel = channel, renewals = nrow(r), saas = nrow(m) > 0, qbr = nrow(q) > 0,
               status = NA_character_, ts = NA_character_,
               markdown = md, message = .md_to_slack(md),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)

  no_ch <- is.na(out$channel)
  out$status[no_ch] <- "skipped: no channel"
  if (any(no_ch)) {
    warning("No Slack channel for: ", paste(out$`Account Name`[no_ch], collapse = ", "),
            " - add them to data/input/slack_channels.csv.", call. = FALSE)
  }
  no_ping <- !is.na(out$`Customer Success Manager`) &
             !(out$`Customer Success Manager` %in% names(user_ids))
  if (any(no_ping)) {
    warning("No Slack user ID for CSM(s) ", paste(unique(out$`Customer Success Manager`[no_ping]),
            collapse = ", "), " - their name is used as plain text and won't notify them. ",
            "Add IDs to ", users_path, ".", call. = FALSE)
  }

  .snitch_write_preview(out, preview_path, as_of, dry_run)

  # Report links left as https://PASTE-...-URL in the template.
  unfilled <- unique(unlist(regmatches(out$markdown, gregexpr("https://PASTE-[A-Za-z0-9-]+", out$markdown))))
  if (length(unfilled)) {
    msg <- paste0("Template still has placeholder link(s): ", paste(unfilled, collapse = ", "),
                  " - paste the Salesforce report URLs into ", template_path, ".")
    if (isTRUE(dry_run)) warning(msg, call. = FALSE) else stop(msg, " Nothing was posted.", call. = FALSE)
  }

  sendable <- which(!no_ch)
  if (isTRUE(dry_run)) {
    out$status[sendable] <- "dry run"
    message(".. push_slack_snitch (dry run): ", length(sendable), " message(s) ready, ",
            sum(no_ch), " skipped. Preview: ", preview_path,
            "\n   Re-run with dry_run = FALSE to post.")
    return(invisible(out[, setdiff(names(out), "markdown")]))
  }

  if (!nzchar(token)) stop("SLACK_BOT_TOKEN is not set - add it to .Renviron.")
  if (isTRUE(confirm) && length(sendable)) {
    ans <- readline(sprintf("Post %d Slack message(s) to %d channel(s)? [y/N] ",
                            length(sendable), length(unique(out$channel[sendable]))))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message(".. push_slack_snitch: cancelled, nothing posted")
      out$status[sendable] <- "cancelled"
      return(invisible(out[, setdiff(names(out), "markdown")]))
    }
  }

  for (i in sendable) {
    res <- .slack_post(out$channel[i], out$message[i], token)
    out$status[i] <- res$status
    out$ts[i]     <- res$ts
    Sys.sleep(1.1)   # Slack allows ~1 message per second per channel
  }

  sent <- sum(out$status == "sent")
  message(".. push_slack_snitch: ", sent, " sent, ",
          sum(grepl("^error", out$status)), " failed, ", sum(no_ch), " skipped")
  if (any(grepl("^error", out$status))) {
    bad <- out[grepl("^error", out$status), ]
    warning("Slack post failed for: ",
            paste0(bad$`Account Name`, " (", bad$status, ")", collapse = "; "), call. = FALSE)
  }
  invisible(out[, setdiff(names(out), "markdown")])
}


# ---- Internal helpers ----------------------------------------------------------

#' Named vector CSM name -> Slack member ID, from the users CSV.
#' @noRd
.slack_user_ids <- function(path) {
  if (!file.exists(path)) {
    warning("Slack users file not found: ", path, call. = FALSE)
    return(character(0))
  }
  u <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                       na.strings = c("", "NA"), strip.white = TRUE, encoding = "UTF-8")
  if (!all(c("Customer Success Manager", "slack_user_id") %in% names(u))) {
    warning("Slack users file needs columns `Customer Success Manager` and `slack_user_id`.",
            call. = FALSE)
    return(character(0))
  }
  u <- u[!is.na(u$slack_user_id) & nzchar(u$slack_user_id), , drop = FALSE]
  bad <- !grepl("^[UW][A-Z0-9]{6,}$", u$slack_user_id)
  if (any(bad)) {
    warning("These slack_user_id values don't look like Slack member IDs: ",
            paste0(u$`Customer Success Manager`[bad], " (", u$slack_user_id[bad], ")",
                   collapse = ", "), call. = FALSE)
  }
  stats::setNames(u$slack_user_id, trimws(u$`Customer Success Manager`))
}

#' Slack's three reserved characters, for values dropped into a message.
#' @noRd
.slack_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Fill the template for one account. Returns Markdown (pre-Slack conversion).
#' @noRd
.snitch_render <- function(template, account, csm, user_ids, r, q, as_of,
                           m = data.frame()) {
  fmt_d <- function(d) if (is.na(d)) NA_character_ else format(as.Date(d), "%m/%d/%Y")

  renewal_items <- if (nrow(r) == 0) "" else paste(vapply(seq_len(nrow(r)), function(i) {
    opp  <- .slack_escape(r$`Opportunity Name`[i])
    url  <- r$opportunity_url[i]
    name <- if (!is.na(url) && nzchar(url)) paste0("[", opp, "](", url, ")") else opp
    why  <- switch(r$Issue[i],
      "No next step"           = "no Next Step entered",
      "No MM/DD date at start" = "Next Step doesn't start with an MM/DD date",
      sprintf("last update %s (%d days ago; due every %d days)",
              format(r$`Next Step Date`[i], "%m/%d"),
              r$`Days Since Update`[i], r$`Allowed Days`[i]))
    sprintf("- %s \u2014 %s \u00b7 %s \u00b7 %s \u2014 %s",
            name, r$`Churn Quarter`[i], .slack_escape(r$`Account Risk`[i]),
            fmt_usd(r$Amount[i]), why)
  }, character(1)), collapse = "\n")

  qbr_item <- if (nrow(q) == 0) "" else {
    last <- q$`Last Review`[1]
    nxt  <- suppressWarnings(max(c(q$`Next QBR`[1], q$`Next EBR`[1]), na.rm = TRUE))
    paste0(
      if (is.na(last)) "no QBR or EBR on record" else
        sprintf("last review %s (%d days ago)", fmt_d(last), q$`Days Since Review`[1]),
      if (is.finite(nxt) && as.Date(nxt) >= as_of) paste0("; next scheduled ", fmt_d(nxt)) else "")
  }

  saas_item <- if (nrow(m) == 0) "" else {
    why <- switch(m$Issue[1],
      "No notes"               = "no migration notes entered",
      "No MM/DD date at start" = "notes don't start with an MM/DD date",
      sprintf("last update %s (%d days ago; due every %d days)",
              format(m$`Notes Date`[1], "%m/%d"), m$`Days Since Update`[1], m$`Allowed Days`[1]))
    sprintf("migration targeted for %s (%s) \u2014 %s",
            m$`Estimated SaaS Migration Period`[1],
            .slack_escape(m$`SaaS Migration Pipeline Stage`[1]), why)
  }

  mention <- if (is.null(csm) || is.na(csm)) "team" else
             if (csm %in% names(user_ids)) paste0("<@", user_ids[[csm]], ">") else .slack_escape(csm)

  txt <- template
  # Optional blocks first, then any remaining (instruction) comments.
  keep <- c(renewals = nrow(r) > 0, saas = nrow(m) > 0, qbr = nrow(q) > 0)
  for (b in names(keep)) {
    re <- sprintf("(?s)<!--\\s*if %s\\s*-->(.*?)<!--\\s*end %s\\s*-->", b, b)
    txt <- gsub(re, if (keep[[b]]) "\\1" else "", txt, perl = TRUE)
  }
  txt <- gsub("(?s)<!--.*?-->", "", txt, perl = TRUE)

  vals <- c(csm = mention, account = .slack_escape(account),
            as_of = format(as.Date(as_of), "%B %d"),
            renewal_items = renewal_items, renewal_count = as.character(nrow(r)),
            saas_item = saas_item, qbr_item = qbr_item)
  for (k in names(vals)) txt <- gsub(paste0("{{", k, "}}"), vals[[k]], txt, fixed = TRUE)

  txt <- gsub("\n{3,}", "\n\n", txt)
  trimws(txt)
}

#' Markdown -> Slack mrkdwn, leaving ``` code blocks untouched.
#' @noRd
.md_to_slack <- function(md) {
  parts <- strsplit(md, "```", fixed = TRUE)[[1]]
  for (i in seq_along(parts)) {
    # Inside a code block: no formatting, but Slack still parses <...> as a
    # link or mention, so placeholders like <owner> must be escaped.
    if (i %% 2 == 0) { parts[i] <- .slack_escape(parts[i]); next }
    p <- parts[i]
    p <- gsub("\\[([^]]+)\\]\\((https?://[^)\\s]+)\\)", "<\\2|\\1>", p, perl = TRUE)
    p <- gsub("(?m)^#{1,6}\\s+(.+)$", "*\\1*", p, perl = TRUE)
    p <- gsub("\\*\\*(.+?)\\*\\*", "*\\1*", p, perl = TRUE)
    p <- gsub("(?m)^(\\s*)[-*]\\s+", "\\1\u2022 ", p, perl = TRUE)
    parts[i] <- p
  }
  paste(parts, collapse = "```")
}

#' One chat.postMessage call. Slack answers HTTP 200 with ok = false on most
#' failures, so the body is what decides.
#' @noRd
.slack_post <- function(channel, text, token) {
  resp <- tryCatch(
    httr2::request("https://slack.com/api/chat.postMessage") |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_body_json(list(channel = channel, text = text,
                                unfurl_links = FALSE, unfurl_media = FALSE)) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(),
    error = function(e) e
  )
  if (inherits(resp, "error")) return(list(status = paste("error:", conditionMessage(resp)), ts = NA_character_))
  body <- httr2::resp_body_json(resp)
  if (isTRUE(body$ok)) list(status = "sent", ts = body$ts %||% NA_character_)
  else list(status = paste("error:", body$error %||% "unknown"), ts = NA_character_)
}

#' Write every message to a Markdown preview file.
#' @noRd
.snitch_write_preview <- function(out, path, as_of, dry_run) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  blocks <- vapply(seq_len(nrow(out)), function(i) {
    paste0("## ", out$`Account Name`[i], "\n\n",
           "*Channel:* ", ifelse(is.na(out$channel[i]), "**none - will be skipped**", out$channel[i]),
           " \u00b7 *CSM:* ", ifelse(is.na(out$`Customer Success Manager`[i]), "\u2014",
                                    out$`Customer Success Manager`[i]), "\n\n",
           out$markdown[i], "\n")
  }, character(1))
  writeLines(c(sprintf("# Snitch Slack messages \u2014 %s%s\n", format(as.Date(as_of), "%Y-%m-%d"),
                       if (isTRUE(dry_run)) " (dry run, nothing posted)" else ""),
               paste(blocks, collapse = "\n---\n\n")), path, useBytes = FALSE)
}
