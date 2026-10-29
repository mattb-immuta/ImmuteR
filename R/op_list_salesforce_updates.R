# Metrics-based adoption ratings replace the hand-maintained Salesforce
# picklists (PBAC / Request / Comply Adoption) in every output. The originals
# are kept as `<field> (SFDC)` so op_list_salesforce_updates() can list where
# Salesforce disagrees with the metrics and needs updating.

# The adoption fields that metrics replace, and the accounts column each
# metric rating comes from. PBAC is joined from consumption$pbac_adoption.
.METRIC_ADOPTION_FIELDS <- c(
  "PBAC Adoption"    = "pbac_adoption",
  "Request Adoption" = "request_adoption_looker",
  "Comply Adoption"  = "comply_adoption_looker"
)

# Exact Salesforce picklist values, by colour — what the CSM should set the
# field to. The metric labels differ from the picklist sentences, so the
# update sheet names the picklist value to choose, not the metric label.
.SFDC_PICKLISTS <- list(
  "PBAC Adoption" = c(
    green = paste0("Green (any 2): 1. Multi-platform consumption at greater than 75% of ",
                   "contracted users. 2. Customer has established domains with stewards ",
                   "and decentralizes policy development. 3. SDD incorporated into ",
                   "tagging strategy & production policies."),
    amber = "Amber: Production consumption at greater than 75% of contracted users.",
    red   = "Red: Less than 75% consumption of contracted users.",
    black = "Black: Less than 25% consumption of contracted users"
  ),
  "Request Adoption" = c(
    green = "Green: Customer ‘retained’ for 3 of the last 4 weeks.",
    amber = paste0("Amber: Use case in production, enabling data determinations, ",
                   "driving adoption towards “retained status”."),
    red   = "Red: POC complete and active use case planning, testing, and deployment.",
    black = "Black: No active customer data request workflow development"
  ),
  "Comply Adoption" = c(
    amber = "Amber - Feature enabled and customer consuming access summary exports",
    red   = "Red - Feature not enabled",
    black = "Black - Feature blocked"
  )
)

.sfdc_col_name <- function(field) paste0(field, " (SFDC)")

# Metric rating per account, per field, aligned to `accounts` rows. NA where
# the metric is unavailable (no Looker run, dashboard failure, no PBAC row).
.metric_adoption_values <- function(accounts, pbac_adoption = NULL) {
  nm <- trimws(as.character(accounts$`Account Name`))
  out <- lapply(names(.METRIC_ADOPTION_FIELDS), function(field) {
    src <- .METRIC_ADOPTION_FIELDS[[field]]
    v <- if (field == "PBAC Adoption") {
      if (is.data.frame(pbac_adoption) && "pbac_adoption" %in% names(pbac_adoption)) {
        pbac_adoption$pbac_adoption[match(nm, trimws(as.character(pbac_adoption$`Account Name`)))]
      } else rep(NA_character_, length(nm))
    } else if (src %in% names(accounts)) {
      accounts[[src]]
    } else rep(NA_character_, length(nm))
    v <- as.character(v)
    v[!is.na(v) & !nzchar(trimws(v))] <- NA
    v
  })
  names(out) <- names(.METRIC_ADOPTION_FIELDS)
  out
}

#' Replace Salesforce adoption picklists with metrics-based ratings
#'
#' Copies each of \code{PBAC Adoption}, \code{Request Adoption} and
#' \code{Comply Adoption} to \code{<field> (SFDC)}, then overwrites the
#' field with the metrics rating wherever one exists (PBAC from
#' \code{pbac_adoption}, Request from \code{request_adoption_looker}, Comply
#' from \code{comply_adoption_looker}). Accounts with no metric keep their
#' Salesforce value, so an SFDC-only run still renders.
#'
#' @param accounts Data frame. \code{op_list$accounts}.
#' @param pbac_adoption Data frame. \code{op_list$consumption$pbac_adoption}.
#' @return \code{accounts} with the three fields replaced and the
#'   \code{(SFDC)} originals added.
#' @export
apply_metric_adoption <- function(accounts, pbac_adoption = NULL) {
  if (is.null(accounts) || nrow(accounts) == 0) return(accounts)
  metric <- .metric_adoption_values(accounts, pbac_adoption)

  for (field in names(metric)) {
    sfdc_col <- .sfdc_col_name(field)
    # Keep the first copy: re-applying must not overwrite the SFDC original
    # with an already-replaced value.
    if (!(sfdc_col %in% names(accounts))) {
      accounts[[sfdc_col]] <- if (field %in% names(accounts))
        as.character(accounts[[field]]) else NA_character_
    }
    m <- metric[[field]]
    accounts[[field]] <- ifelse(is.na(m), accounts[[sfdc_col]], m)
  }
  accounts
}

#' Salesforce adoption fields that need updating to match the metrics
#'
#' Compares each account's Salesforce \code{PBAC Adoption},
#' \code{Request Adoption} and \code{Comply Adoption} (the \code{(SFDC)}
#' originals kept by \code{\link{apply_metric_adoption}}) against the
#' metrics rating, by colour, and returns one row per field that differs.
#'
#' @param accounts Data frame. \code{op_list$accounts} after
#'   \code{apply_metric_adoption()}.
#' @param pbac_adoption Data frame. \code{op_list$consumption$pbac_adoption},
#'   for the PBAC reason text.
#'
#' @return A tibble: \code{Account Name}, \code{Account ID},
#'   \code{Customer Success Manager}, \code{Field}, \code{Salesforce Value},
#'   \code{Metrics Rating}, \code{Set Salesforce To} (the picklist value of the
#'   metric colour) and \code{Reason}. Zero rows when nothing differs.
#' @export
op_list_salesforce_updates <- function(accounts, pbac_adoption = NULL) {
  empty <- tibble::tibble(`Account Name` = character(), `Account ID` = character(),
                          `Customer Success Manager` = character(), Field = character(),
                          `Salesforce Value` = character(), `Metrics Rating` = character(),
                          `Set Salesforce To` = character(), Reason = character())
  if (is.null(accounts) || nrow(accounts) == 0) return(empty)

  nm     <- trimws(as.character(accounts$`Account Name`))
  metric <- .metric_adoption_values(accounts, pbac_adoption)
  col_or_na <- function(df, col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))

  pa <- if (is.data.frame(pbac_adoption)) {
    pbac_adoption[match(nm, trimws(as.character(pbac_adoption$`Account Name`))), , drop = FALSE]
  } else NULL

  reasons <- list(
    "PBAC Adoption" = if (is.null(pa)) rep(NA_character_, length(nm)) else
      paste0("Complexity score ", col_or_na(pa, "complexity_score"),
             ", consumption ratio ",
             ifelse(is.na(suppressWarnings(as.numeric(col_or_na(pa, "consumption_ratio")))), "n/a",
                    paste0(round(100 * suppressWarnings(as.numeric(col_or_na(pa, "consumption_ratio")))), "%"))),
    "Request Adoption" = paste0("Looker dashboard 1009 stage: ",
                                col_or_na(accounts, "request_adoption_looker_stage")),
    "Comply Adoption" = paste0(
      "access_summary_export ", ifelse(col_or_na(accounts, "comply_export_enabled") %in% TRUE, "on", "off"),
      ", comply_enabled ",      ifelse(col_or_na(accounts, "comply_feature_enabled") %in% TRUE, "on", "off"))
  )

  rows <- lapply(names(metric), function(field) {
    m        <- metric[[field]]
    sfdc     <- as.character(col_or_na(accounts, .sfdc_col_name(field)))
    m_col    <- .ra_colour(m)
    s_col    <- .ra_colour(sfdc)
    differs  <- !is.na(m_col) & (is.na(s_col) | s_col != m_col)
    if (!any(differs)) return(NULL)
    tibble::tibble(
      `Account Name`             = as.character(accounts$`Account Name`)[differs],
      `Account ID`               = as.character(col_or_na(accounts, "Account ID"))[differs],
      `Customer Success Manager` = as.character(col_or_na(accounts, "Customer Success Manager"))[differs],
      Field                      = field,
      `Salesforce Value`         = sfdc[differs],
      `Metrics Rating`           = m[differs],
      `Set Salesforce To`        = unname(.SFDC_PICKLISTS[[field]][m_col[differs]]),
      Reason                     = reasons[[field]][differs]
    )
  })

  out <- dplyr::bind_rows(c(list(empty), rows))
  out <- out[order(out$`Customer Success Manager`, out$`Account Name`, out$Field), , drop = FALSE]
  message(".. salesforce updates: ", nrow(out), " field(s) on ",
          length(unique(out$`Account Name`)), " account(s) differ from metrics")
  out
}
