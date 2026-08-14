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
#' @param idex Logical. Whether to refresh idex parquet exports before reading. Defaults to \code{FALSE}.
#' @param idex_path Character. Path to idex parquet directory. Defaults to \code{"data/idex/"}.
#' @param looker Logical. If \code{TRUE}, pulls the six Looker looks that back the
#'   CPO consumption spreadsheet and joins live metrics (users, data sources, queries,
#'   policies, support cases) onto \code{op_list$consumption$renewals}. Requires
#'   \code{LOOKER_CLIENT_ID} and \code{LOOKER_CLIENT_SECRET} environment variables.
#'   Defaults to \code{FALSE} — SFDC-only stats are always computed.
#' @param looker_cache_path Character or NULL. Directory for Looker parquet caches.
#'   Defaults to \code{"data/looker/"}. Pass \code{NULL} to disable caching.
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
#'   \code{orders}, \code{consumption}, \code{support} (from BigQuery — see
#'   \code{\link{op_list_support}}), and (if \code{productboard = TRUE})
#'   \code{product_board}.
#' @export
enrich_opportunities <- function(sf_opportunity_report    = "00OVH000005p6A92AI",
                                 sf_account_report        = "00OVH0000069G8Y2AU",
                                 sf_active_order_report   = "00OVV000006AOnB2AW",
                                 sf_saas_migration_report = "00OVV000006FbWr2AK",
                                 sf_csm_summary_report    = "00OVV000006FbNB2A0",
                                 sf_orders_report         = "00OVV000005RTxV2AW",
                                 idex                     = FALSE,
                                 idex_path                = "data/idex/",
                                 looker                   = FALSE,
                                 looker_cache_path        = "data/looker/",
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

  # ── consumption child list ──────────────────────────────────────────────────
  message(".. building op_list$consumption")
  raw <- if (looker) {
    load_op_list_raw(cache_path = looker_cache_path)
  } else {
    NULL
  }
  output_list[["consumption"]] <- op_list_consumption(
    op_list = output_list,
    raw     = raw,
    low_consumption_threshold = low_consumption_threshold
  )

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

  # ── product_board child list ────────────────────────────────────────────────
  if (productboard) {
    message(".. building op_list$product_board")
    output_list[["product_board"]] <- op_list_product_board(
      accounts = output_list[["accounts"]]
    )
  }

  message(".. enrich_opportunities complete")
  return(output_list)
}
