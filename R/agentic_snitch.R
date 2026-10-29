#' Agentic snitch: blank agentic profile fields, by CSM
#'
#' Runs the Salesforce agentic profile report (default
#' \code{00OVV000007LHjN2AW}) and flags every account with a blank field.
#' Every column that is not an identity column (account name/ID, CSM, owner,
#' region, links) is checked, unless \code{fields} names the ones to check.
#' A field counts as blank when it is \code{NA}, empty, \code{"-"},
#' an em-dash or \code{"NA"}, or starts with \code{"TBD"}; such fields are annotated
#' \code{"Incomplete"}.
#'
#' If the report returns several rows for one account, a field is only
#' incomplete when it is blank on every row.
#'
#' The output is grouped by CSM, each CSM shown with the number of updates
#' (blank fields) they own, e.g. \code{"Jane Doe (7)"}. Accounts with no CSM
#' are grouped under \code{"No CSM assigned"}.
#'
#' @param sf_report Character. Salesforce report ID.
#' @param fields Optional character vector of report columns to check.
#'   \code{NULL} (default) checks every non-identity column.
#' @param regions Optional character vector; keep only accounts in these
#'   regions (needs a \code{Region} column). \code{NULL} keeps all.
#' @param output_path Markdown file to write (CSM \eqn{\rightarrow} table of
#'   accounts and fields). \code{NULL} skips the file.
#' @param report Optional data frame to use instead of running the report
#'   (e.g. a cached pull).
#'
#' @return Invisibly, a list:
#'   \describe{
#'     \item{\code{by_csm}}{One row per CSM: \code{CSM}, \code{Label}
#'       (\code{"Name (n)"}), \code{Accounts}, \code{Updates}; most updates
#'       first.}
#'     \item{\code{accounts}}{One row per flagged account: \code{CSM},
#'       \code{Account Name}, \code{Account ID}, \code{Updates},
#'       \code{Fields to Update} (comma-separated), \code{account_url}.}
#'     \item{\code{detail}}{One row per incomplete field: \code{CSM},
#'       \code{Account Name}, \code{Field}, \code{Status} (\code{"Incomplete"}).}
#'     \item{\code{annotated}}{The report, one row per account, with every
#'       blank checked field replaced by \code{"Incomplete"}.}
#'     \item{\code{fields_checked}}{The columns that were checked.}
#'   }
#'
#' @examples
#' \dontrun{
#'   salesforcer::sf_auth()
#'   snitch <- agentic_snitch()
#'   snitch$by_csm
#' }
#' @export
agentic_snitch <- function(sf_report   = "00OVV000007LHjN2AW",
                           fields      = NULL,
                           regions     = NULL,
                           output_path = "output/agentic_snitch.md",
                           report      = NULL) {

  rpt <- if (is.null(report)) .sf_run_report(sf_report) else report
  rpt <- as.data.frame(rpt, check.names = FALSE, stringsAsFactors = FALSE)
  names(rpt) <- trimws(gsub("\u2191|\u2193", "", names(rpt)))
  if (nrow(rpt) == 0) stop("Report ", sf_report, " returned no rows.", call. = FALSE)

  # ── Identity columns ──────────────────────────────────────────────────────
  pick <- function(cands) {
    hit <- names(rpt)[tolower(names(rpt)) %in% tolower(cands)]
    if (length(hit)) hit[1] else NA_character_
  }
  name_col <- pick(c("Account Name", "Account: Account Name", "Account"))
  id_col   <- pick(c("Account ID", "Account ID 18", "Account: Account ID", "Account 18 Digit ID"))
  csm_col  <- pick(c("Customer Success Manager", "CSM", "Account: Customer Success Manager",
                     "Customer Success Manager: Full Name"))
  reg_col  <- pick(c("Region", "Account: Region"))
  if (is.na(name_col)) {
    stop("Report ", sf_report, " has no Account Name column. Columns: ",
         paste(names(rpt), collapse = ", "), call. = FALSE)
  }
  if (is.na(csm_col)) {
    warning("Report ", sf_report, " has no Customer Success Manager column; ",
            "everything is grouped under 'No CSM assigned'.", call. = FALSE)
  }

  if (!is.null(regions) && !is.na(reg_col)) {
    rpt <- rpt[trimws(as.character(rpt[[reg_col]])) %in% regions, , drop = FALSE]
  }

  # Columns that describe the account rather than the agentic profile.
  identity <- c(name_col, id_col, csm_col, reg_col)
  identity <- c(identity[!is.na(identity)],
                grep("(^|: )(account (name|id|owner|18)|opportunity|owner|region|url|link|type|record type|created|last modified)",
                     names(rpt), ignore.case = TRUE, value = TRUE))
  check <- if (is.null(fields)) setdiff(names(rpt), identity) else intersect(fields, names(rpt))
  if (!is.null(fields) && length(setdiff(fields, names(rpt)))) {
    warning("Not in report, skipped: ", paste(setdiff(fields, names(rpt)), collapse = ", "), call. = FALSE)
  }
  if (!length(check)) stop("No fields to check in report ", sf_report, ".", call. = FALSE)

  # ── One row per account, blank = blank on every row ───────────────────────
  is_blank <- function(x) {
    v <- trimws(as.character(x))
    is.na(x) | v %in% c("", "-", "\u2014", "\u2013", "NA", "N/A")
  }
  # A "TBD..." placeholder is not an answer: checked fields that start with
  # TBD are incomplete too.
  is_tbd <- function(x) !is_blank(x) & grepl("^\\s*tbd", as.character(x), ignore.case = TRUE)
  is_incomplete <- function(x) is_blank(x) | is_tbd(x)
  key <- if (!is.na(id_col)) {
    ifelse(is_blank(rpt[[id_col]]), rpt[[name_col]], substr(trimws(rpt[[id_col]]), 1, 15))
  } else tolower(trimws(rpt[[name_col]]))
  rpt$.key <- key

  first_filled <- function(x) {
    x <- x[!is_blank(x)]
    x <- x[order(is_tbd(x))]           # a real value beats a TBD on another row
    if (length(x)) as.character(x[1]) else NA_character_
  }
  acct <- do.call(rbind, lapply(split(rpt, rpt$.key), function(g) {
    out <- lapply(c(identity[identity %in% names(g)], check), function(f) first_filled(g[[f]]))
    names(out) <- c(identity[identity %in% names(g)], check)
    as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  }))
  rownames(acct) <- NULL

  csm <- if (is.na(csm_col)) rep(NA_character_, nrow(acct)) else acct[[csm_col]]
  csm[is_blank(csm)] <- "No CSM assigned"
  acct$CSM <- csm

  blank_mat <- vapply(check, function(f) is_incomplete(acct[[f]]), logical(nrow(acct)))
  if (!is.matrix(blank_mat)) blank_mat <- matrix(blank_mat, nrow = nrow(acct), dimnames = list(NULL, check))

  annotated <- acct
  for (f in check) annotated[[f]][blank_mat[, f]] <- "Incomplete"

  # ── Flagged accounts and the per-field detail ─────────────────────────────
  n_upd <- rowSums(blank_mat)
  flag  <- n_upd > 0
  acc_id <- if (!is.na(id_col)) acct[[id_col]] else rep(NA_character_, nrow(acct))
  accounts <- data.frame(
    CSM                = acct$CSM,
    `Account Name`     = acct[[name_col]],
    `Account ID`       = acc_id,
    Updates            = n_upd,
    `Fields to Update` = apply(blank_mat, 1, function(r) paste(check[r], collapse = ", ")),
    account_url        = ifelse(is.na(acc_id), NA_character_,
                                paste0("https://immuta.lightning.force.com/lightning/r/Account/", acc_id, "/view")),
    check.names = FALSE, stringsAsFactors = FALSE
  )[flag, , drop = FALSE]

  csm_tot  <- tapply(accounts$Updates, accounts$CSM, sum)
  accounts <- accounts[order(-csm_tot[accounts$CSM], accounts$CSM, -accounts$Updates,
                             accounts$`Account Name`), , drop = FALSE]
  rownames(accounts) <- NULL

  detail <- if (any(flag)) {
    w <- which(blank_mat & flag, arr.ind = TRUE)
    d <- data.frame(CSM = acct$CSM[w[, 1]], `Account Name` = acct[[name_col]][w[, 1]],
                    Field = check[w[, 2]], Status = "Incomplete",
                    check.names = FALSE, stringsAsFactors = FALSE)
    d[order(match(d$CSM, unique(accounts$CSM)), d$`Account Name`, match(d$Field, check)), , drop = FALSE]
  } else data.frame(CSM = character(0), `Account Name` = character(0), Field = character(0),
                    Status = character(0), check.names = FALSE)
  rownames(detail) <- NULL

  by_csm <- if (nrow(accounts)) {
    b <- data.frame(CSM = names(csm_tot), Updates = as.integer(csm_tot),
                    Accounts = as.integer(table(accounts$CSM)[names(csm_tot)]),
                    stringsAsFactors = FALSE)
    b <- b[order(-b$Updates, b$CSM), , drop = FALSE]
    b$Label <- paste0(b$CSM, " (", b$Updates, ")")
    rownames(b) <- NULL
    b[, c("CSM", "Label", "Accounts", "Updates")]
  } else data.frame(CSM = character(0), Label = character(0), Accounts = integer(0), Updates = integer(0))

  # ── Console + markdown ────────────────────────────────────────────────────
  message("agentic snitch: ", nrow(acct), " account(s) checked on ", length(check), " field(s); ",
          sum(accounts$Updates), " update(s) needed across ", nrow(accounts), " account(s).")
  for (i in seq_len(nrow(by_csm))) message("  ", by_csm$Label[i])

  if (!is.null(output_path)) {
    md <- c(
      paste0("# Agentic snitch (as of ", format(Sys.Date()), ")"),
      "",
      paste0("Salesforce report `", sf_report, "`. Checked ", nrow(acct), " accounts on ",
             length(check), " fields: ", paste(check, collapse = ", "), "."),
      paste0("**", sum(accounts$Updates), " updates needed across ", nrow(accounts), " accounts.**"),
      ""
    )
    for (i in seq_len(nrow(by_csm))) {
      a <- accounts[accounts$CSM == by_csm$CSM[i], , drop = FALSE]
      link <- ifelse(is.na(a$account_url), .snitch_md_escape(a$`Account Name`),
                     paste0("[", .snitch_md_escape(a$`Account Name`), "](", a$account_url, ")"))
      md <- c(md,
              paste0("## ", by_csm$Label[i]),
              "",
              "| Account | Updates | Incomplete fields |",
              "|---|---|---|",
              paste0("| ", link, " | ", a$Updates, " | ", .snitch_md_escape(a$`Fields to Update`), " |"),
              "")
    }
    if (!nrow(by_csm)) md <- c(md, "Every account is complete.")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(md, output_path, useBytes = TRUE)
    message("  -> ", output_path)
  }

  invisible(list(by_csm = by_csm, accounts = accounts, detail = detail,
                 annotated = annotated[, setdiff(names(annotated), ".key"), drop = FALSE],
                 fields_checked = check))
}

# Pipes and newlines would break a markdown table cell.
.snitch_md_escape <- function(x) {
  x <- gsub("|", "\\|", as.character(x), fixed = TRUE)
  gsub("[\r\n]+", " ", x)
}
