#' Pull and enrich opportunities, accounts, and supplementary tables from Salesforce.
#'
#' Fetches opportunity, account, active order, CSM assignment, and SaaS pipeline
#' reports from Salesforce, enriches renewals and expansions with account context,
#' and packages everything into a named list for downstream reporting.
#' Also builds \code{op_list$consumption} from the orders report and — optionally
#' — live Looker data.
#'
#' @param sf_opportunity_report Character. Salesforce report ID for the opportunity report.
#' @param sf_account_report Character. Salesforce report ID for the account report.
#'   Expected to carry the QBR/EBR date and note fields, which
#'   \code{\link{normalize_qbr_fields}} copies to \code{Date Last QBR},
#'   \code{Date Next QBR}, and \code{QBR Note}.
#' @param sf_active_order_report Character. Salesforce report ID for active orders.
#' @param sf_saas_migration_report Character. Salesforce report ID for the SaaS migration pipeline.
#' @param sf_csm_summary_report Character. Salesforce report ID for CSM account assignments.
#' @param sf_orders_report Character. Salesforce report ID for the orders report.
#' @param sf_forecast_report Character. Salesforce report ID for the renewal
#'   churn forecast report (00OVV000006hrNN2AY), pulled on every run into
#'   \code{op_list$forecast} via \code{\link{op_list_forecast}}. Set to
#'   \code{NULL} to skip the pull.
#' @param sf_contacts_report Character. Salesforce report ID for the contacts
#'   report (executive buyer, champion and incident response contacts per
#'   customer), pulled into \code{op_list$contacts} — one row per contact,
#'   with \code{is_incident_response} etc. flags for mail merges — via
#'   \code{\link{op_list_contacts}}. Executive buyer and champion names are
#'   also appended to \code{op_list$accounts} as \code{executive_buyer} and
#'   \code{champion}. Set to \code{NULL} to skip the pull (both columns are
#'   then \code{NA}).
#' @param looker Logical. If \code{TRUE}, pulls the six Looker looks that back the
#'   CPO consumption spreadsheet and joins live metrics (users, data sources, queries,
#'   policies, support cases) onto \code{op_list$consumption$renewals}. Requires
#'   \code{LOOKER_CLIENT_ID} and \code{LOOKER_CLIENT_SECRET} environment variables.
#'   Also derives \code{request_adoption_looker} (plus
#'   \code{request_adoption_looker_stage} and \code{request_adoption_match}) on
#'   \code{op_list$accounts} from the Request metrics dashboard (see
#'   \code{\link{op_list_request_adoption}}); the hand-maintained Salesforce
#'   \code{Request Adoption} picklist is left untouched alongside it.
#'   Defaults to \code{FALSE} — SFDC-only stats are always computed.
#' @param low_consumption_threshold Numeric. Fraction below which a customer's
#'   registered/licensed user ratio flags them as low consumption in
#'   \code{op_list$consumption$low_consumption$`Low Consumption`} — a customer
#'   is also flagged when their Production instance has zero active policies of
#'   any kind. Defaults to \code{0.25} (25\%). Passed through to
#'   \code{\link{op_list_consumption}}.
#' @param productboard Logical. If \code{TRUE}, pulls Productboard notes and
#'   features and builds \code{op_list$product_board} (feedback + feature
#'   requests summarized per account). Requires a \code{PRODUCTBOARD_API_TOKEN}
#'   environment variable (see \code{?productboard}). This step makes one API
#'   call per note, so it's noticeably slower than the other enrich_*() steps
#'   — hence opt-in. Defaults to \code{FALSE}.
#' @param fiscal.period Character vector of fiscal periods to retain in expansions,
#'   e.g. \code{c("Q1-2027", "Q2-2027")}.
#'
#' @return A named list with elements: \code{renewals}, \code{expansions},
#'   \code{accounts}, \code{active_orders}, \code{csm_table}, \code{saas_pipeline},
#'   \code{orders}, \code{contacts} (see \code{\link{op_list_contacts}}),
#'   \code{consumption}, \code{forecast} (see
#'   \code{\link{op_list_forecast}}), \code{support} (from BigQuery — see
#'   \code{\link{op_list_support}}), and (if \code{productboard = TRUE})
#'   \code{product_board}.
#' @export
enrich_opportunities <- function(sf_opportunity_report    = "00OVH000005p6A92AI",
                                 sf_account_report        = "00OVH0000069G8Y2AU",
                                 sf_active_order_report   = "00OVV000006AOnB2AW",
                                 sf_saas_migration_report = "00OVV000006FbWr2AK",
                                 sf_csm_summary_report    = "00OVV000006FbNB2A0",
                                 sf_orders_report         = "00OVV000005RTxV2AW",
                                 sf_forecast_report       = "00OVV000006hrNN2AY",
                                 sf_contacts_report       = "00OVV000006TwOX2A0",
                                 looker                   = FALSE,
                                 low_consumption_threshold = 0.25,
                                 productboard             = FALSE,
                                 fiscal.period) {

  message(".. enrich_opportunities initiated")

  # create necessary hyperlinks
  opportunity.table<-sf_run_report(sf_opportunity_report)
    opportunity.table$`Fiscal Period`<-factor(opportunity.table$`Fiscal Period`,levels=fiscal.period)
    opportunity.table$opportunity_url=paste("https://immuta.lightning.force.com/lightning/r/Opportunity/",as.character(opportunity.table$`Opportunity ID`),"/view",sep="")

  account.table<- sf_run_report(sf_account_report)
    account.table$account_url=paste("https://immuta.lightning.force.com/lightning/r/Account/",account.table$`Account ID`,"/view",sep="")
    account.table$consumption_url=paste("https://immuta.cloud.looker.com/dashboards/993?Name=",gsub(" ","+",account.table$`Account Name`),"&Account+ID=&Report+Month=6+month&Instance+Name=&Instance+Type=Production",sep="")
    account.table$customer_central_url=paste("https://customer-central.immuta.support/customers/",account.table$`Account ID 18`,sep="")
    account.table$`Contract ARR Rollup`=extract_usd_amount(account.table$`Contract ARR Rollup`)
    # account.table$retool_url=paste("https://retool-nonprod.ic-ops.net/app/customer-central/customerDetail?customerName=",gsub(" ","%2O",account.table$`Account Name`),sep="")

  # The account report carries the QBR/EBR fields directly; this just adds
  # stably-named `Date Last QBR` / `Date Next QBR` / `QBR Note` copies, since
  # Salesforce has renamed the underlying fields more than once.
  account.table <- normalize_qbr_fields(account.table)


  # split into expansions and renewals
  renewals<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Renewal'),]
  expansions<-opportunity.table[as.logical(opportunity.table$`Opportunity Record Type` =='Expansion'),]

  message("opportunity data collected...enrich opportunity data")


   renewals=enrich_renewals(renewal.table=renewals,
                           account.table=account.table,
                           fiscal.period=NULL)

  expansions=enrich_expansions(expansion.table=expansions,
                               account.table=account.table,
                               fiscal.period=fiscal.period)

  # package enriched data tables into a list object
  output_list=list()
  output_list[["renewals"]]=renewals
  output_list[["expansions"]]=expansions
  output_list[["accounts"]]=account.table
  output_list[["active_orders"]]<-sf_run_report(sf_active_order_report)
  output_list[["csm_table"]]<-sf_run_report(sf_csm_summary_report)
  output_list[["saas_pipeline"]]<-sf_run_report(sf_saas_migration_report)
  output_list[["orders"]] <- sf_run_report(sf_orders_report)

  # ── contacts child table ────────────────────────────────────────────────────
  # One row per contact (kept that way for incident-response mail merges).
  # Executive buyer / champion names are rolled up onto accounts; the
  # Relationship / Influence ratings already arrive on the account report
  # itself. Warns rather than aborting — the columns are NA (em-dash
  # downstream) if the report can't be read.
  contacts <- NULL
  if (!is.null(sf_contacts_report) && nzchar(sf_contacts_report)) {
    message(".. building op_list$contacts")
    contacts <- tryCatch(
      op_list_contacts(sf_contacts_report = sf_contacts_report),
      error = function(e) {
        warning("Contacts report skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    output_list[["contacts"]] <- contacts
  }
  output_list[["accounts"]] <- .accounts_add_contact_roles(
    output_list[["accounts"]], contacts)

  # ── forecast child table ────────────────────────────────────────────────────
  # Renewal churn forecast, grouped by churn quarter. Diffed against the prior
  # snapshot in enrich_list_object() -> op_list$changes_detail$forecast.
  # Warns rather than aborting so a report permission change doesn't kill a run
  # that has already pulled everything else.
  if (!is.null(sf_forecast_report) && nzchar(sf_forecast_report)) {
    message(".. building op_list$forecast")
    output_list[["forecast"]] <- tryCatch(
      op_list_forecast(sf_forecast_report = sf_forecast_report,
                       opportunity_table  = opportunity.table),
      error = function(e) {
        warning("Forecast report skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  # ── consumption child list ──────────────────────────────────────────────────
  message(".. building op_list$consumption")
  # One login reused by every Looker call in this run (the looks below and the
  # Request adoption dashboard further down).
  looker_token <- if (looker) looker_auth() else NULL

  raw <- if (looker) {
    load_op_list_raw(token = looker_token)
  } else {
    NULL
  }
  # Accounts using domains, from the retained-accounts tile of the Domains
  # dashboard — one of the PBAC complexity signals. Warns rather than aborting:
  # the component scores 0 for everyone if the tile can't be read.
  domain_accounts <- if (looker) {
    tryCatch(
      looker_tile_accounts(1053, "retained accounts", token = looker_token),
      error = function(e) {
        warning("Domain accounts skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  } else {
    NULL
  }

  output_list[["consumption"]] <- op_list_consumption(
    op_list = output_list,
    raw     = raw,
    low_consumption_threshold = low_consumption_threshold,
    domain_accounts = domain_accounts
  )

  # Propagate multiplatform flag from low_consumption onto accounts so it is
  # available for downstream functions (HTML report, slide headers, etc.).
  if (!is.null(output_list[["consumption"]][["low_consumption"]]) &&
      "multiplatform" %in% names(output_list[["consumption"]][["low_consumption"]])) {
    mp_tbl <- output_list[["consumption"]][["low_consumption"]] |>
      dplyr::select(`Account Name`, multiplatform)
    output_list[["accounts"]] <- output_list[["accounts"]] |>
      dplyr::left_join(mp_tbl, by = "Account Name") |>
      dplyr::mutate(multiplatform = dplyr::coalesce(multiplatform, FALSE))
  }

  # ── derived Request adoption tier ───────────────────────────────────────────
  # The Salesforce `Request Adoption` picklist is maintained by hand; this
  # derives the same four tiers from the stage tiles of the Request metrics
  # dashboard (Looker 1009) and attaches them ALONGSIDE the SFDC value rather
  # than replacing it, so the two can be compared before the automation is
  # trusted. Needs Looker, and warns rather than aborting — downstream renders
  # an em-dash when absent.
  if (looker) {
    message(".. deriving Request Adoption from Looker dashboard 1009")
    ra <- tryCatch(
      op_list_request_adoption(accounts = output_list[["accounts"]],
                               token    = looker_token),
      error = function(e) {
        warning("Request Adoption derivation skipped: ", conditionMessage(e),
                call. = FALSE)
        NULL
      }
    )

    if (!is.null(ra)) {
      output_list[["accounts"]] <- output_list[["accounts"]] |>
        dplyr::left_join(
          # One row per account name: `ra` comes back one row per row of
          # accounts, so a duplicated name in the SFDC report would otherwise
          # fan this join out.
          ra |> dplyr::distinct(`Account Name`, .keep_all = TRUE),
          by = "Account Name"
        )
    }
  }

  # ── support child list ──────────────────────────────────────────────────────
  # Pulled from BigQuery (retool-457517), not Looker — see ?op_list_support.
  # Requires bigrquery::bq_auth() to have run at least once on this machine.
  # Warns rather than aborting: everything downstream renders an em-dash for
  # missing support figures.
  message(".. building op_list$support")
  output_list[["support"]] <- tryCatch(
    op_list_support(accounts = output_list[["accounts"]]),
    error = function(e) {
      warning("Support rollup skipped: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  # ── escalations child list ──────────────────────────────────────────────────
  # Open CUSTX Jira issues — the detail behind support$escalations_open, which
  # is only a count. Needs JIRA_EMAIL / JIRA_TOKEN in .Renviron; warns rather
  # than aborting, and the Escalations tab renders an empty state without it.
  message(".. building op_list$escalations")
  output_list[["escalations"]] <- tryCatch(
    op_list_escalations(accounts = output_list[["accounts"]]),
    error = function(e) {
      warning("Escalations skipped: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  # ── product_board child list ────────────────────────────────────────────────
  if (productboard) {
    message(".. building op_list$product_board")
    output_list[["product_board"]] <- op_list_product_board(
      accounts = output_list[["accounts"]]
    )
  }

  # ── build timestamp ─────────────────────────────────────────────────────────
  # When this op_list was assembled, stamped here rather than read off the
  # snapshot filename or Sys.time() at render time. main.R saves the .RData
  # AFTER generating the report, so the newest file on disk is the previous
  # run's — and re-rendering a loaded snapshot should date the data, not the
  # render. Consumers read it through .op_list_updated().
  output_list[["updated"]] <- Sys.time()

  message(".. enrich_opportunities complete")
  return(output_list)
}
