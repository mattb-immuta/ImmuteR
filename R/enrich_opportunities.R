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
#' @param low_consumption_threshold Numeric. Fraction below which a customer is
#'   flagged as low consumption in \code{op_list$consumption$low_consumption}.
#'   Defaults to \code{0.20} (20\%).
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
#'   \code{orders}, \code{consumption}, and (if \code{productboard = TRUE})
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
                                 low_consumption_threshold = 0.20,
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
    raw     = raw
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
