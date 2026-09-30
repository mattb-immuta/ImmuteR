# Look IDs backing the CPO op_list consumption spreadsheet
# (All Customers + Active SKUs – https://docs.google.com/spreadsheets/d/1SAvZo1bUpKM3OlGz4a-wK8M5Nhfni6kbMOI6z8JIW0g)
.OP_LIST_LOOKS <- list(
  data_sources  = 1744L,   # RAW DATA - DATA SOURCES
  queries       = 1745L,   # RAW DATA - QUERIES
  users         = 1746L,   # RAW DATA - USERS
  policies      = 1747L,   # RAW DATA - POLICIES
  sdd_active    = 1748L,   # RAW DATA - SDD ACTIVE
  support_cases = 1776L    # RAW DATA - SUPPORT CASES
)

# Short display names mapped to their Look 1744 column names.
.PLATFORM_COLS <- c(
  "Redshift"       = "[Usage Metrics] Data Sources Count: Amazon Redshift",
  "S3"             = "[Usage Metrics] Data Sources Count: Amazon S3",
  "Synapse"        = "[Usage Metrics] Data Sources Count: Azure Synapse",
  "BigQuery"       = "[Usage Metrics] Data Sources Count: BigQuery",
  "Databricks"     = "[Usage Metrics] Data Sources Count: Databricks",
  "Lake Formation" = "[Usage Metrics] Data Sources Count: Lake Formation",
  "Snowflake"      = "[Usage Metrics] Data Sources Count: Snowflake",
  "Trino"          = "[Usage Metrics] Data Sources Count: Trino",
  "Other"          = "Data Sources: Other"
)

# Build per-account platform HTML strings and multiplatform flag from the raw
# data_sources look (1744). Production platforms render in normal weight;
# platforms present only in Dev/Test render in grey. Returns a data frame with
# columns: Account Name, platform_html, multiplatform.
.build_platform_data <- function(data_sources) {
  col_map  <- .PLATFORM_COLS[.PLATFORM_COLS %in% names(data_sources)]
  short_nm <- names(col_map)
  raw_cols <- unname(col_map)

  if (length(col_map) == 0) {
    return(data.frame(`Account Name` = character(0),
                      platform_html  = character(0),
                      multiplatform  = logical(0),
                      check.names    = FALSE))
  }

  agg <- data_sources |>
    dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
    dplyr::select(
      `Account Name`,
      `Instance Type` = `Immuta Instances Instance Type`,
      dplyr::all_of(setNames(raw_cols, short_nm))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(short_nm),
                                ~ suppressWarnings(as.numeric(.)))) |>
    dplyr::group_by(`Account Name`, `Instance Type`) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(short_nm),
                                   ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop")

  prod_agg <- agg |> dplyr::filter(`Instance Type` == "Production")
  dev_agg  <- agg |>
    dplyr::filter(`Instance Type` != "Production") |>
    dplyr::group_by(`Account Name`) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(short_nm),
                                   ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop")

  rows <- lapply(unique(agg$`Account Name`), function(acct) {
    pr <- prod_agg[prod_agg$`Account Name` == acct, , drop = FALSE]
    dv <- dev_agg[dev_agg$`Account Name`   == acct, , drop = FALSE]

    prod_active <- if (nrow(pr) > 0) {
      short_nm[vapply(short_nm,
                      function(p) { v <- pr[[p]]; !is.na(v) && v > 0 },
                      logical(1))]
    } else character(0)

    dev_only <- if (nrow(dv) > 0) {
      short_nm[vapply(short_nm,
                      function(p) {
                        v <- dv[[p]]
                        !is.na(v) && v > 0 && !(p %in% prod_active)
                      }, logical(1))]
    } else character(0)

    parts <- c(
      prod_active,
      if (length(dev_only) > 0)
        paste0('<span style="color:#aaa">', dev_only, '</span>')
      else character(0)
    )

    data.frame(
      `Account Name` = acct,
      platform_html  = if (length(parts) > 0) paste(parts, collapse = " · ") else "—",
      platform_prod  = if (length(prod_active) > 0) paste(prod_active, collapse = " · ") else "—",
      platform_dev   = if (length(dev_only)    > 0) paste(dev_only,    collapse = " · ") else NA_character_,
      multiplatform  = length(prod_active) > 1,
      check.names    = FALSE
    )
  })

  do.call(rbind, rows)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

.parse_currency <- function(x) {
  as.numeric(gsub(",", "", gsub("[$]", "", x)))
}

.parse_pct <- function(x) {
  # "190%" -> 1.90 ;  "10%" -> 0.10
  as.numeric(gsub("%", "", x)) / 100
}

# Sum ignoring NAs, but return NA (not 0) when every value is missing — the
# difference between "this account has zero users" and "we have no data".
.sum_or_na <- function(x) {
  v <- suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
}

# First non-missing value. Used for contract-level figures that Looker repeats
# identically on every instance row, where summing would multiply the value by
# the number of instances.
.first_or_na <- function(x) {
  v <- suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else v[1]
}

# ── Public API ────────────────────────────────────────────────────────────────

#' Load raw op_list data from Looker
#'
#' Fetches each of the six Looker looks that back the CPO op_list consumption
#' spreadsheet directly from Looker on every call.
#'
#' @param token Character. Bearer token from \code{looker_auth()}.
#'   Defaults to calling \code{looker_auth()} automatically.
#'
#' @return Named list of tibbles: \code{users}, \code{data_sources},
#'   \code{queries}, \code{policies}, \code{sdd_active}, \code{support_cases}.
#' @export
load_op_list_raw <- function(token = NULL) {
  if (is.null(token)) token <- looker_auth()

  raw <- lapply(names(.OP_LIST_LOOKS), function(name) {
    look_id <- .OP_LIST_LOOKS[[name]]
    message(glue::glue(".. {name}: fetching look {look_id} from Looker"))
    run_looker_look(look_id, token)
  })

  names(raw) <- names(.OP_LIST_LOOKS)
  raw
}

#' Build the consumption child list for op_list
#'
#' Reproduces the \strong{Stats}, \strong{Renewals}, and \strong{Low
#' Consumption Customers} worksheets from the CPO op_list consumption
#' spreadsheet as R data frames, and returns them as a named list suitable
#' for storing as \code{op_list$consumption}.
#'
#' Called automatically by \code{\link{enrich_opportunities}} — pass
#' \code{looker = TRUE} there to include live Looker metrics. Can also be
#' called directly to refresh consumption data without re-running the full
#' SFDC pull.
#'
#' SFDC data comes from \code{op_list$orders} (report \code{00OVV000005RTxV2AW}),
#' the same source that backs the \emph{Data} sheet in the CPO spreadsheet.
#'
#' All Looker-derived metrics (\code{Registered Users}, \code{Data Sources
#' Count}, \code{Active Policies (Total)}, etc.) are scoped to each account's
#' \strong{Production} instances only — Development/Test instances are never
#' counted. Where an account runs more than one Production instance, user and
#' policy counts are \strong{summed across all of them}; \code{Licensed Users}
#' is a contract-level figure Looker repeats per instance row, so it is taken
#' once rather than summed. When Looker data is supplied (\code{raw} not \code{NULL}), each
#' customer's \code{low_consumption} row is also annotated with a \code{Low
#' Consumption} flag. A customer is flagged when \emph{any} of:
#' \itemize{
#'   \item they have \strong{no Production instance at all} (automatic flag,
#'     regardless of the checks below),
#'   \item \code{Registered Users / Licensed Users < low_consumption_threshold}, or
#'   \item their Production instance has zero active policies of any kind
#'     (\code{Active Policies (Total) == 0}).
#' }
#' Accounts with a Production instance but incomplete Looker data on the other
#' checks keep \code{Low Consumption = NA} (unknown, not "not low").
#'
#' @param op_list Named list from \code{\link{enrich_opportunities}}.
#'   Must contain \code{$orders}.
#' @param raw Named list from \code{\link{load_op_list_raw}}, or \code{NULL}
#'   (default). When \code{NULL}, SFDC-derived stats and the contract base are
#'   still built; Looker columns (\code{Users}, \code{Data Sources},
#'   \code{License Consumption}, etc.) will be \code{NA}.
#' @param low_consumption_threshold Numeric. Fraction below which a customer's
#'   registered/licensed user ratio flags them as low consumption. Default
#'   \code{0.25} (25\%).
#' @param domain_accounts Character vector or \code{NULL}. Accounts using
#'   domains, passed through to \code{\link{op_list_pbac_adoption}}.
#'   \code{enrich_opportunities()} supplies this from the Domains dashboard
#'   (Looker 1053) on a \code{looker = TRUE} run; \code{NULL} scores the
#'   \code{domains} component \code{0} for everyone.
#'
#' @return Named list stored as \code{op_list$consumption}, with elements:
#' \describe{
#'   \item{\code{stats}}{Nested list: \code{by_product_code}, \code{by_mapping},
#'     \code{connectors}, \code{deployment}, \code{top_customers}.}
#'   \item{\code{renewals}}{One row per contract with SFDC fields and, when
#'     \code{raw} is supplied, live Looker metrics.}
#'   \item{\code{low_consumption}}{One row per customer: account base fields
#'     (including \code{Lifecycle Stage}), Production Looker metrics, and the
#'     derived \code{Registered Users}, \code{Licensed Users}, \code{Consumption
#'     Ratio}, \code{Data Sources Count}, \code{Data Policies},
#'     \code{Subscription Policies}, \code{Guardrail Policies},
#'     \code{Active Policies (Total)}, \code{Prod Instance Count},
#'     \code{Immuta Version} (software version string for self-managed
#'     accounts, literal \code{"saas"} for SaaS-hosted accounts, \code{NA}
#'     when not captured by the SDD-active look), and \code{Low Consumption}
#'     columns described above.}
#'   \item{\code{metrics}}{One row per account with the numeric inputs for
#'     adoption scoring — currently \code{platform_count}. See
#'     \code{\link{op_list_metrics}}. Zero rows when \code{raw} is
#'     \code{NULL}.}
#'   \item{\code{pbac_adoption}}{One row per account with the PBAC complexity
#'     signals, their weighted \code{complexity_score}, the production
#'     \code{consumption_ratio}, and the \code{pbac_adoption} stoplight rating
#'     derived from those two. See \code{\link{op_list_pbac_adoption}}.
#'     Components with no Looker data behind them score \code{0}.}
#' }
#' @export
op_list_consumption <- function(op_list,
                                raw = NULL,
                                low_consumption_threshold = 0.25,
                                domain_accounts = NULL) {
  if (is.null(op_list$orders)) {
    stop("`op_list$orders` is NULL. Run enrich_opportunities() first.")
  }

  has_looker <- !is.null(raw)
  message(".. op_list_consumption: building data frames",
          if (!has_looker) " (SFDC-only; pass raw = load_op_list_raw() for Looker metrics)")


  # ── Raw Looker data frames — stored as named child objects ───────────────────
  users           <- if (has_looker) raw$users        else NULL  # look 1746
  policy_metrics  <- if (has_looker) raw$policies     else NULL  # look 1747
  sdd_consumption <- if (has_looker) raw$sdd_active   else NULL  # look 1748
  data_sources    <- if (has_looker) raw$data_sources else NULL  # look 1744

  # ── low_consumption ───────────────────────────────────────────────────────────
  # One row per customer. Base from op_list$accounts; Looker metrics joined when
  # available. Users and policy_metrics are filtered to Production instances;
  # where a customer has multiple Production instances the row with the highest
  # seat consumption is used for users, and the highest total policies for metrics.
  account_base <- op_list$accounts |>
    dplyr::select(
      `Account ID`,
      `Account Name`,
      `Contract ARR Rollup`,
      `Is SaaS Customer`,
      `Customer on Unsupported Version`,
      `Lifecycle Stage`
    )

  if (has_looker) {
    # Columns beyond Instance Type shared across all three Looker joins below —
    # dropped from each so a plain left_join() doesn't silently suffix them
    # (.x/.y) when the same column name shows up in more than one join.
    .shared_join_cols <- c("Immuta Instances Instance Type",
                           "Immuta Instances Deployment Platform")

    # ── Production users ────────────────────────────────────────────────────
    # Registered Users is the SUM of `[Usage Metrics] Users in System` across
    # every Production instance an account runs — several accounts (Cigna,
    # Activision Blizzard, Booz Allen Hamilton, Takeda Oncology) operate both an
    # Immuta Managed Cloud and an On Premises Production instance, and taking
    # only one of them undercounts by roughly half.
    #
    # Licensed Users is `Salesforce Contracts All Contracted Users`, a
    # contract-level figure that Looker repeats identically on every instance
    # row for an account (verified: it does not vary across Production rows for
    # any multi-instance account). It is therefore taken ONCE, not summed —
    # summing would multiply the denominator by the instance count.
    #
    # `Users in System` is the right numerator because it counts the same
    # population the denominator contracts for. The earlier
    # `Full User Pricing Count + Policy Owner Count` excluded data consumers
    # while `All Contracted Users` includes them, which systematically deflated
    # the ratio — median 0.27 against a 0.25 flag threshold, so most of the book
    # sat on the line.
    users_agg <- users |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::summarise(
        `Registered Users`    = .sum_or_na(`[Usage Metrics] Users in System`),
        `Licensed Users`      = .first_or_na(`Salesforce Contracts All Contracted Users`),
        `Prod Instance Count` = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`)

    # Representative Production row, carrying the remaining raw user columns
    # (consumer counts, contract flags) for anything downstream that wants them.
    # Ordered by Users in System so the "main" instance is the largest one.
    users_prod <- users |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::mutate(.uis = suppressWarnings(as.numeric(`[Usage Metrics] Users in System`))) |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::slice_max(order_by = .uis, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
      dplyr::select(-dplyr::any_of(.shared_join_cols), -.uis)

    # ── Production policies ─────────────────────────────────────────────────
    # Summed across every Production instance, for the same reason as users:
    # Booz Allen Hamilton runs 457 policies on prem and 2 in managed cloud, and
    # reporting only one of those is wrong either way round.
    #
    # `Active Global Policies: Total` == Data + Subscription exactly on all 73
    # Production rows in the cached look — Guardrail policies are NOT in the
    # total (3 of 73 rows have one). So Data and Subscription fully decompose
    # the total, and the three columns stay internally consistent.
    policy_agg <- policy_metrics |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::summarise(
        `Data Policies`           = .sum_or_na(`[Usage Metrics] Active Global Policies: Data`),
        `Subscription Policies`   = .sum_or_na(`Active Global Policies: Subscription`),
        `Guardrail Policies`      = .sum_or_na(`[Usage Metrics] Active Global Policies: Guardrail`),
        `Active Policies (Total)` = .sum_or_na(`Active Global Policies: Total`),
        .groups = "drop"
      ) |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`)

    # Representative Production row, for the remaining raw policy columns.
    policy_prod <- policy_metrics |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::mutate(.tot = suppressWarnings(as.numeric(`Active Global Policies: Total`))) |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::slice_max(order_by = .tot, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
      dplyr::select(-dplyr::any_of(.shared_join_cols), -.tot)

    # Production data_sources — one row per customer (highest data source count instance)
    datasources_prod <- data_sources |>
      dplyr::filter(`Immuta Instances Instance Type` == "Production") |>
      dplyr::mutate(.dsc = suppressWarnings(as.numeric(`[Usage Metrics] Data Sources Count`))) |>
      dplyr::group_by(`Salesforce Accounts Account Name`) |>
      dplyr::slice_max(order_by = .dsc, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
      dplyr::select(-dplyr::any_of(.shared_join_cols), -.dsc)

    # Immuta software version — from the SDD-active look (1748), the only one of
    # the six op_list looks that carries `Immuta Instances Immuta Version`. That
    # look has no `Instance Type` column (it's already account-grain), so it's
    # joined directly rather than filtered/deduped like the three joins above.
    # Value is the literal string "saas" for SaaS-hosted accounts, or a version
    # string (e.g. "2024.2.15") for self-managed accounts; accounts absent from
    # this look (no SDD activity captured) join to NA and render as an em-dash
    # downstream.
    version_prod <- if (!is.null(sdd_consumption) &&
                        "Immuta Instances Immuta Version" %in% names(sdd_consumption)) {
      sdd_consumption |>
        dplyr::rename(`Account Name` = `Salesforce Accounts Account Name`) |>
        dplyr::distinct(`Account Name`, .keep_all = TRUE) |>
        dplyr::select(`Account Name`, `Immuta Instances Immuta Version`) |>
        dplyr::rename(`Immuta Version` = `Immuta Instances Immuta Version`)
    } else {
      NULL
    }

    low_consumption <- account_base |>
      dplyr::left_join(users_agg,        by = "Account Name") |>
      dplyr::left_join(users_prod,       by = "Account Name") |>
      dplyr::left_join(policy_agg,       by = "Account Name") |>
      dplyr::left_join(policy_prod,      by = "Account Name") |>
      dplyr::left_join(datasources_prod, by = "Account Name")

    if (!is.null(version_prod)) {
      low_consumption <- low_consumption |>
        dplyr::left_join(version_prod, by = "Account Name")
    } else {
      low_consumption$`Immuta Version` <- NA_character_
    }

    # Does this account have a Production instance at all? (present in at
    # least one of the three Production-filtered Looker joins above) —
    # accounts with none are automatically flagged as low consumption below,
    # regardless of what the ratio/policy checks would otherwise say.
    has_prod_instance <- low_consumption$`Account Name` %in%
      Reduce(union, list(users_prod$`Account Name`,
                        policy_prod$`Account Name`,
                        datasources_prod$`Account Name`))

    # ── Derived consumption metrics ────────────────────────────────────────────
    # Read the AGGREGATED columns from users_agg / policy_agg, not the raw
    # Looker columns.
    #
    # `[Usage Metrics] Users in System`, `Salesforce Contracts All Contracted
    # Users` and the raw policy columns are all still present on
    # `low_consumption`, joined in from users_prod / policy_prod — but those
    # hold a SINGLE Production instance's values. Reading them here silently
    # undoes the cross-instance summing and undercounts multi-instance accounts
    # (Cigna 2,317 -> 1,276 users; Booz Allen 459 -> 457 policies).
    registered_users <- suppressWarnings(as.numeric(low_consumption$`Registered Users`))
    licensed_users   <- suppressWarnings(as.numeric(low_consumption$`Licensed Users`))
    data_policies    <- suppressWarnings(as.numeric(low_consumption$`Data Policies`))
    sub_policies     <- suppressWarnings(as.numeric(low_consumption$`Subscription Policies`))
    active_policies  <- suppressWarnings(as.numeric(low_consumption$`Active Policies (Total)`))
    data_source_ct   <- suppressWarnings(as.numeric(low_consumption$`[Usage Metrics] Data Sources Count`))

    # Licensed Users of 0 means an ELA (unlimited seats), not a bad denominator
    # — consumption_ratio() in utils_internal.R scores those 1 rather than NA.
    # Named `ratio` rather than `consumption_ratio` so it doesn't shadow that
    # helper for the rest of the function.
    ratio <- consumption_ratio(registered_users, licensed_users)

    low_ratio   <- !is.na(ratio) & ratio < low_consumption_threshold
    no_policies <- !is.na(active_policies) & active_policies == 0
    has_signal  <- !is.na(ratio) | !is.na(active_policies)

    low_consumption$`Registered Users`         <- registered_users
    low_consumption$`Licensed Users`           <- licensed_users
    low_consumption$`Consumption Ratio`        <- ratio
    low_consumption$`Data Sources Count`       <- data_source_ct
    low_consumption$`Data Policies`            <- data_policies
    low_consumption$`Subscription Policies`    <- sub_policies
    low_consumption$`Active Policies (Total)`  <- active_policies
    low_consumption$`Low Consumption`          <- ifelse(
      !has_prod_instance, TRUE,                        # no Production instance at all
      ifelse(has_signal, low_ratio | no_policies, NA)   # otherwise: ratio or zero-policy check
    )

    # ── Platform breakdown ───────────────────────────────────────────────────────
    # Build per-account platform HTML strings and multiplatform flag, then join
    # onto low_consumption so downstream reports and the HTML dashboard can use them.
    platform_data <- .build_platform_data(data_sources)
    low_consumption <- low_consumption |>
      dplyr::left_join(
        platform_data |> dplyr::select(`Account Name`, platform_html,
                                       platform_prod, platform_dev, multiplatform),
        by = "Account Name"
      )

  } else {
    low_consumption <- account_base
  }

  # ── Connections adoption ────────────────────────────────────────────────────
  # From dashboard 995, not a saved look, so it is fetched here rather than in
  # load_op_list_raw(). Guarded: a renamed tile or a Looker outage should cost
  # one column on the card, not the whole consumption build.
  connections <- if (has_looker) {
    tryCatch(op_list_connections(), error = function(e) {
      warning("Connections adoption skipped: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  } else NULL

  if (!is.null(connections) && nrow(connections) > 0 && !is.null(low_consumption)) {
    # distinct() guards the join the same way the multiplatform join below
    # does: a duplicated account name would fan low_consumption out and break
    # its one-row-per-customer contract.
    low_consumption <- low_consumption |>
      dplyr::left_join(
        connections |> dplyr::distinct(`Account Name`, .keep_all = TRUE),
        by = "Account Name"
      )
  }

  # ── PBAC adoption ───────────────────────────────────────────────────────────
  # multiplatform lives on low_consumption at this point, not on accounts:
  # enrich_opportunities() copies it across only AFTER this function returns.
  # Any stale copy on accounts (from calling this on an already-enriched
  # op_list) is dropped first, so the join can't produce .x/.y suffixes and
  # silently zero the component.
  accounts_mp <- op_list$accounts
  if (!is.null(low_consumption) && "multiplatform" %in% names(low_consumption)) {
    # Dropped only when there's a replacement to join: an SFDC-only re-run
    # against an already-enriched op_list keeps the multiplatform values
    # enrich_opportunities() put on accounts rather than zeroing the component.
    accounts_mp$multiplatform <- NULL
    accounts_mp <- accounts_mp |>
      dplyr::left_join(
        low_consumption |>
          dplyr::select(`Account Name`, multiplatform) |>
          # One row per name: a duplicated account would fan the join out and
          # break row alignment with op_list$accounts.
          dplyr::distinct(`Account Name`, .keep_all = TRUE),
        by = "Account Name"
      )
  }

  pbac_adoption <- op_list_pbac_adoption(
    accounts        = accounts_mp,
    low_consumption = low_consumption,
    sdd_consumption = sdd_consumption,
    policy_metrics  = policy_metrics,
    domain_accounts = domain_accounts
  )

  message(".. op_list_consumption: complete")

  list(
    # ── Raw Looker data frames ──────────────────────────────────────────────────
    users           = users,           # look 1746 — all instances, all columns
    policy_metrics  = policy_metrics,  # look 1747 — all instances, all columns
    sdd_consumption = sdd_consumption, # look 1748 — all instances, all columns
    data_sources    = data_sources,    # look 1744 — all instances, all columns

    # ── Derived ─────────────────────────────────────────────────────────────────
    low_consumption = low_consumption,  # one row per customer, accounts + Looker
    metrics         = op_list_metrics(data_sources),  # one row per account, scoring inputs
    pbac_adoption   = pbac_adoption,  # one row per account, PBAC rating + inputs
    connections     = connections     # one row per account, dashboard 995 tile
  )
}
