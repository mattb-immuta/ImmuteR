#' Write the snitch Slack messages as a payload for a Dust agent to post
#'
#' The Dust route to \code{\link{push_slack_snitch}}: instead of posting with a
#' Slack bot token from R, this writes every message to a JSON file you upload
#' to the Dust "Snitch poster" agent, which posts them with Dust's Slack tool
#' (as you, via your own Slack login). See \code{DUST_SNITCH_AGENT.md} at the
#' project root for the agent setup and instructions.
#'
#' The messages are built by the same code as \code{push_slack_snitch()} — same
#' template (\code{data/input/snitch_slack_template.md}), same channels
#' (\code{data/input/slack_channels.csv}), same Slack formatting — so the
#' preview here is exactly what gets posted.
#'
#' \strong{Mentions.} When a CSM has a Slack ID in
#' \code{data/input/slack_users.csv} the message carries \code{<@ID>}. When they
#' don't, the message carries the literal token \code{{{mention}}} plus a
#' \code{mention_name}, and the agent looks the person up with Slack's user
#' list and swaps in their \code{<@ID>} (or their plain name if there isn't
#' exactly one match).
#'
#' \strong{Test mode.} Set \code{test_channel} (e.g. \code{"#tmp-cs-test"}) and
#' every message is redirected there, capped at \code{test_limit} messages, with
#' a banner saying where it would have gone and who it would have pinged. The
#' CSM mention is replaced by \code{test_mention} (you), so no CSM is notified.
#' The file is stamped \code{"mode": "test"} and the agent refuses to post a
#' test file anywhere but its \code{test_channel}.
#'
#' Files written to \code{dir}:
#' \describe{
#'   \item{\code{snitch_messages_<mode>.json}}{What the agent posts. One object
#'     per message: \code{channel}, \code{message} (Slack mrkdwn, post as-is),
#'     \code{message_markdown} (the same text as standard Markdown, in case
#'     Dust's Slack tool converts Markdown itself), \code{mention_name}/
#'     \code{mention_id}, plus the account and CSM.}
#'   \item{\code{snitch_preview_<mode>.md}}{The same messages as readable
#'     Markdown, for you to review before uploading.}
#' }
#'
#' @param snitch List returned by \code{\link{report_next_step_snitch}}.
#' @param op_list Named list from \code{enrich_opportunities()}.
#' @param test_channel Character or \code{NULL}. A channel name
#'   (\code{"#tmp-cs-test"}) or ID. When set, writes a test payload.
#' @param test_mention Who to ping in test mode: a Slack member ID
#'   (\code{"U..."}), a name for the agent to look up, or \code{NULL} for no
#'   ping. Default \code{"Matt Benigni"}.
#' @param test_limit Integer. Maximum messages in a test payload (the accounts
#'   with the most sections come first). Ignored when \code{accounts} is given.
#' @param include_saas,include_qbr,accounts As in \code{\link{push_slack_snitch}}.
#' @param dir Character. Output directory. Default \code{"output/dust_snitch"}.
#' @param template_path,users_path As in \code{\link{push_slack_snitch}}.
#'
#' @return Invisibly, a named character vector of the paths written.
#' @examples
#' \dontrun{
#'   snitch <- report_next_step_snitch(op_list)
#'   output_dust_snitch(snitch, op_list, test_channel = "#tmp-cs-test")  # test
#'   output_dust_snitch(snitch, op_list)                                 # live
#' }
#' @export
output_dust_snitch <- function(snitch,
                               op_list,
                               test_channel  = NULL,
                               test_mention  = "Matt Benigni",
                               test_limit    = 3,
                               include_saas  = TRUE,
                               include_qbr   = FALSE,
                               accounts      = NULL,
                               dir           = "output/dust_snitch",
                               template_path = "data/input/snitch_slack_template.md",
                               users_path    = "data/input/slack_users.csv") {

  test <- !is.null(test_channel) && nzchar(test_channel)
  mode <- if (test) "test" else "live"

  # In test mode every message pings the tester, never the CSM.
  test_is_id <- test && !is.null(test_mention) && grepl("^[UW][A-Z0-9]{6,}$", test_mention)
  override <- if (!test) NULL else
              if (is.null(test_mention) || !nzchar(test_mention)) "" else
              if (test_is_id) paste0("<@", test_mention, ">") else "{{mention}}"

  built <- .snitch_build(snitch, op_list, include_saas, include_qbr, accounts,
                         template_path, users_path, caller = "output_dust_snitch",
                         unresolved_mention = "token", mention_override = override)
  if (is.null(built)) return(invisible(character(0)))
  out   <- built$out
  as_of <- built$as_of

  unfilled <- .snitch_unfilled_links(out$markdown)
  if (length(unfilled)) {
    stop("Template still has placeholder link(s): ", paste(unfilled, collapse = ", "),
         " - paste the Salesforce report URLs into ", template_path, ". Nothing was written.",
         call. = FALSE)
  }

  out$intended_channel <- out$channel
  if (test) {
    # Richest messages first, so a short test still exercises every section.
    if (is.null(accounts)) {
      ord <- order(-(as.integer(out$renewals > 0) + out$saas + out$qbr), -out$renewals)
      out <- out[utils::head(ord, test_limit), , drop = FALSE]
    }
    out$channel <- test_channel
    banner <- sprintf("_:test_tube: TEST — live, this would post to %s and ping %s._",
                      ifelse(is.na(out$intended_channel), "no channel (would be skipped)",
                      ifelse(grepl("^[CG][A-Z0-9]{8,}$", out$intended_channel),
                             paste0("<#", out$intended_channel, ">"),        # channel ID -> link
                             paste0("#", sub("^#", "", out$intended_channel)))),
                      ifelse(is.na(out$`Customer Success Manager`), "nobody",
                             .slack_escape(out$`Customer Success Manager`)))
    out$message  <- paste0(banner, "\n\n", out$message)
    out$markdown <- paste0(banner, "\n\n", out$markdown)
    out$mention_name <- if (identical(override, "{{mention}}")) test_mention else NA_character_
    out$mention_id   <- if (test_is_id) test_mention else NA_character_
  } else {
    out$mention_name <- ifelse(grepl("{{mention}}", out$message, fixed = TRUE),
                               out$`Customer Success Manager`, NA_character_)
    out$mention_id   <- out$csm_slack_id
  }

  skipped <- out[is.na(out$channel), , drop = FALSE]
  send    <- out[!is.na(out$channel), , drop = FALSE]
  send$channel <- sub("^#", "", send$channel)   # the Dust tool takes a bare name or an ID

  batch_id <- paste0("snitch-", mode, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  na_null  <- function(x) if (length(x) == 0 || is.na(x)) NULL else x

  payload <- list(
    batch_id     = batch_id,
    mode         = mode,
    test_channel = if (test) sub("^#", "", test_channel) else NULL,
    as_of        = format(as.Date(as_of), "%Y-%m-%d"),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    count        = nrow(send),
    messages     = lapply(seq_len(nrow(send)), function(i) list(
      n                = i,
      account          = send$`Account Name`[i],
      csm              = na_null(send$`Customer Success Manager`[i]),
      channel          = send$channel[i],
      intended_channel = na_null(send$intended_channel[i]),
      mention_name     = na_null(send$mention_name[i]),
      mention_id       = na_null(send$mention_id[i]),
      message          = send$message[i],          # Slack mrkdwn — what the agent posts
      message_markdown = send$markdown[i]          # same text as standard Markdown (fallback)
    )),
    skipped      = lapply(seq_len(nrow(skipped)), function(i) list(
      account = skipped$`Account Name`[i], reason = "no Slack channel in data/input/slack_channels.csv"
    ))
  )

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(json    = file.path(dir, sprintf("snitch_messages_%s.json", mode)),
             preview = file.path(dir, sprintf("snitch_preview_%s.md", mode)))
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             paths[["json"]], useBytes = TRUE)

  blocks <- vapply(seq_len(nrow(send)), function(i) {
    paste0("## ", i, ". ", send$`Account Name`[i], "\n\n",
           "*Channel:* ", send$channel[i],
           " · *CSM:* ", ifelse(is.na(send$`Customer Success Manager`[i]), "—",
                                    send$`Customer Success Manager`[i]),
           " · *Mention:* ", ifelse(!is.na(send$mention_id[i]), send$mention_id[i],
                                        ifelse(!is.na(send$mention_name[i]),
                                               paste0(send$mention_name[i], " (agent looks up)"),
                                               "none")),
           "\n\n", send$markdown[i], "\n")
  }, character(1))
  writeLines(c(sprintf("# Snitch messages for Dust — %s (%s, %d message%s)\n",
                       payload$as_of, toupper(mode), nrow(send), if (nrow(send) == 1) "" else "s"),
               if (nrow(skipped)) paste0("**Skipped (no channel):** ",
                                         paste(skipped$`Account Name`, collapse = ", "), "\n"),
               paste(blocks, collapse = "\n---\n\n")),
             paths[["preview"]], useBytes = TRUE)

  if (nrow(skipped)) {
    warning("No Slack channel for: ", paste(skipped$`Account Name`, collapse = ", "),
            " - add them to data/input/slack_channels.csv.", call. = FALSE)
  }
  message(".. output_dust_snitch (", toupper(mode), "): ", nrow(send), " message(s)",
          if (test) paste0(" -> ", test_channel) else "",
          if (nrow(skipped)) paste0(", ", nrow(skipped), " skipped (no channel)") else "",
          "\n   Review ", paths[["preview"]], ", then upload ", paths[["json"]],
          " to the Dust snitch agent.")
  invisible(paths)
}
