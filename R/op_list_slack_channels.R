#' Per-account internal Slack channels
#'
#' Reads the hand-maintained channel dictionary, one row per customer, for
#' joining onto \code{op_list$accounts} by \code{\link{enrich_opportunities}}.
#'
#' The CSV has four columns:
#' \describe{
#'   \item{\code{Account Name}}{Salesforce account name. Used to match only
#'     when \code{Account ID 18} is blank.}
#'   \item{\code{Account ID 18}}{The 18-character Salesforce account ID — the
#'     preferred match key, since it survives account renames.}
#'   \item{\code{slack_channel}}{Channel name, e.g. \code{cust-acme}. A leading
#'     \code{#} is stripped and the name is lowercased.}
#'   \item{\code{slack_channel_id}}{Slack's channel ID (e.g.
#'     \code{C0123ABCD}; in Slack, open the channel details and it is at the
#'     bottom). Optional for display, but posting to a channel through the
#'     Slack API needs the ID, not the name.}
#' }
#' Rows with neither channel column filled in are dropped, so the file can
#' carry every account with blanks still to fill. Duplicate accounts keep their
#' first row, with a warning; a \code{slack_channel_id} that doesn't look like a
#' Slack ID (\code{C}/\code{G} followed by capitals and digits) is warned about
#' but kept.
#'
#' @param path Character. Path to the CSV. Defaults to
#'   \code{"data/input/slack_channels.csv"} (relative to the project root).
#'
#' @return A data frame with columns \code{Account Name}, \code{Account ID 18},
#'   \code{slack_channel}, \code{slack_channel_id}; zero rows if the file is
#'   missing or unreadable (with a warning).
#' @export
op_list_slack_channels <- function(path = "data/input/slack_channels.csv") {
  cols  <- c("Account Name", "Account ID 18", "slack_channel", "slack_channel_id")
  empty <- stats::setNames(
    data.frame(matrix(character(0), ncol = 4), stringsAsFactors = FALSE), cols)

  if (!file.exists(path)) {
    warning("Slack channel file not found: ", path, call. = FALSE)
    return(empty)
  }
  sc <- tryCatch(
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                    na.strings = c("", "NA"), strip.white = TRUE,
                    encoding = "UTF-8"),
    error = function(e) {
      warning("Slack channel file unreadable: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(sc)) return(empty)

  missing_cols <- setdiff(cols, names(sc))
  if (length(missing_cols)) {
    warning("Slack channel file is missing column(s): ",
            paste(missing_cols, collapse = ", "), call. = FALSE)
    return(empty)
  }
  sc <- sc[, cols, drop = FALSE]

  # Slack channel names are always lowercase; a capitalised name would fail to post.
  sc$slack_channel    <- tolower(sub("^#", "", trimws(sc$slack_channel)))
  sc$slack_channel_id <- trimws(sc$slack_channel_id)
  sc[!is.na(sc$slack_channel)    & sc$slack_channel    == "", "slack_channel"]    <- NA
  sc[!is.na(sc$slack_channel_id) & sc$slack_channel_id == "", "slack_channel_id"] <- NA

  sc <- sc[!is.na(sc$slack_channel) | !is.na(sc$slack_channel_id), , drop = FALSE]
  if (nrow(sc) == 0) return(empty)

  # One row per account: the ID where there is one, else the name.
  key <- ifelse(is.na(sc$`Account ID 18`) | sc$`Account ID 18` == "",
                paste0("name:", tolower(trimws(sc$`Account Name`))),
                paste0("id:", sc$`Account ID 18`))
  dup <- duplicated(key)
  if (any(dup)) {
    warning("Slack channel file lists these accounts more than once (first row kept): ",
            paste(unique(sc$`Account Name`[dup]), collapse = ", "), call. = FALSE)
    sc <- sc[!dup, , drop = FALSE]
  }

  bad_id <- !is.na(sc$slack_channel_id) & !grepl("^[CG][A-Z0-9]{6,}$", sc$slack_channel_id)
  if (any(bad_id)) {
    warning("These slack_channel_id values don't look like Slack channel IDs: ",
            paste0(sc$`Account Name`[bad_id], " (", sc$slack_channel_id[bad_id], ")",
                   collapse = ", "), call. = FALSE)
  }

  rownames(sc) <- NULL
  sc
}

# Join the channel dictionary onto accounts as `slack_channel` and
# `slack_channel_id`. Matches on Account ID 18 first, then — for CSV rows with
# no ID — on account name (case-insensitive). CSV rows that match no account
# are reported, since they are usually a typo or a churned customer.
.accounts_add_slack_channels <- function(accounts, channels) {
  accounts <- accounts[, setdiff(names(accounts), c("slack_channel", "slack_channel_id")),
                       drop = FALSE]
  accounts$slack_channel    <- NA_character_
  accounts$slack_channel_id <- NA_character_
  if (is.null(channels) || nrow(channels) == 0) return(accounts)

  acct_id   <- if ("Account ID 18" %in% names(accounts)) as.character(accounts$`Account ID 18`) else
                 rep(NA_character_, nrow(accounts))
  acct_name <- tolower(trimws(as.character(accounts$`Account Name`)))

  has_id <- !is.na(channels$`Account ID 18`) & channels$`Account ID 18` != ""
  idx <- ifelse(has_id,
                match(channels$`Account ID 18`, acct_id),
                match(tolower(trimws(channels$`Account Name`)), acct_name))

  unmatched <- is.na(idx)
  if (any(unmatched)) {
    warning("Slack channel rows matching no account (check the name / Account ID 18): ",
            paste(channels$`Account Name`[unmatched], collapse = ", "), call. = FALSE)
  }

  hit <- !unmatched
  accounts$slack_channel[idx[hit]]    <- channels$slack_channel[hit]
  accounts$slack_channel_id[idx[hit]] <- channels$slack_channel_id[hit]

  message(".. slack channels: ", sum(!is.na(accounts$slack_channel) |
                                     !is.na(accounts$slack_channel_id)),
          " of ", nrow(accounts), " accounts assigned")
  accounts
}
