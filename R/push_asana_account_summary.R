#' Push an account summary to an Asana task.
#'
#' Converts an account summary to Asana-compatible HTML and either updates an
#' existing task (matched by account name) or creates a new one in the specified
#' project. Uses the Asana REST API v1.
#'
#' @param account_summary Named list returned by \code{output_account_summary()}.
#' @param project_gid Character. The Asana project GID. Find it in your project
#'   URL: \code{https://app.asana.com/0/\{project_gid\}/...}
#' @param asana_token Character. Your Asana Personal Access Token. Generate one
#'   at \url{https://app.asana.com/0/my-apps}. Store in \code{.Renviron} as
#'   \code{ASANA_TOKEN} and retrieve with \code{Sys.getenv("ASANA_TOKEN")}.
#'
#' @return Invisibly returns the Asana task GID that was created or updated.
#'
#' @examples
#' \dontrun{
#'   summary <- output_account_summary("Cigna", op_list)
#'   push_asana_account_summary(
#'     account_summary = summary,
#'     project_gid     = "1234567890",
#'     asana_token     = Sys.getenv("ASANA_TOKEN")
#'   )
#' }
#' @export
push_asana_account_summary <- function(account_summary,
                                       project_gid,
                                       asana_token) {

  account_name <- account_summary$account$account_name
  html_notes   <- build_asana_html(account_summary)

  base_req <- httr2::request("https://app.asana.com/api/1.0") %>%
    httr2::req_auth_bearer_token(asana_token) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_error(body = function(resp) {
      httr2::resp_body_json(resp)$errors[[1]]$message
    })

  # Find existing task in the project matching the account name
  existing_gid <- asana_find_task(base_req, project_gid, account_name)

  if (!is.null(existing_gid)) {
    # Update existing task
    httr2::request("https://app.asana.com/api/1.0") %>%
      httr2::req_auth_bearer_token(asana_token) %>%
      httr2::req_headers("Accept" = "application/json") %>%
      httr2::req_error(body = function(resp) {
        httr2::resp_body_json(resp)$errors[[1]]$message
      }) %>%
      httr2::req_url_path_append("tasks", existing_gid) %>%
      httr2::req_method("PUT") %>%
      httr2::req_body_json(list(data = list(html_notes = html_notes))) %>%
      httr2::req_perform()

    message(".. updated Asana task for: ", account_name)
    invisible(existing_gid)

  } else {
    # Create new task
    resp <- httr2::request("https://app.asana.com/api/1.0") %>%
      httr2::req_auth_bearer_token(asana_token) %>%
      httr2::req_headers("Accept" = "application/json") %>%
      httr2::req_error(body = function(resp) {
        httr2::resp_body_json(resp)$errors[[1]]$message
      }) %>%
      httr2::req_url_path_append("tasks") %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(list(data = list(
        name       = account_name,
        projects   = list(project_gid),
        html_notes = html_notes
      ))) %>%
      httr2::req_perform()

    new_gid <- httr2::resp_body_json(resp)$data$gid
    message(".. created Asana task for: ", account_name)
    invisible(new_gid)
  }
}


# --- Internal: find a task by name in a project --------------------------------

asana_find_task <- function(base_req, project_gid, account_name) {
  resp <- base_req %>%
    httr2::req_url_path_append("projects", project_gid, "tasks") %>%
    httr2::req_url_query(opt_fields = "name,gid", limit = 100) %>%
    httr2::req_perform()

  tasks <- httr2::resp_body_json(resp)$data
  if (length(tasks) == 0) return(NULL)

  names_vec <- vapply(tasks, `[[`, character(1), "name")
  gids_vec  <- vapply(tasks, `[[`, character(1), "gid")
  match_idx <- which(trimws(names_vec) == trimws(account_name))

  if (length(match_idx) == 0) return(NULL)
  gids_vec[[match_idx[1]]]
}


# --- Internal: build Asana-compatible HTML from account_summary ---------------

build_asana_html <- function(account_summary) {

  acct <- account_summary$account
  opps <- account_summary$opportunities
  arch <- account_summary$architecture

  h <- function(...) paste0(...)

  # ── Account Overview --------------------------------------------------------
  overview_items <- paste0(collapse = "",
    asana_li("Contract ARR",  fmt_usd(acct$contract_arr)),
    asana_li("Region",        acct$region),
    asana_li("Account Owner", acct$account_owner),
    asana_li("Field CTO",     acct$field_cto),
    asana_li("CSM",           acct$csm)
  )

  # ── Architecture ------------------------------------------------------------
  is_saas <- isTRUE(arch$is_saas)
  deploy_item <- if (is_saas) {
    asana_li("Deployment", "\u2705 SaaS")
  } else {
    unsupported <- isTRUE(arch$unsupported_version %in% c(TRUE, "Yes", "TRUE", "true", "yes"))
    paste0(
      asana_li("Deployment", "Self-Hosted"),
      asana_li("Unsupported Version",
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
        asana_li("Amount",            fmt_usd(r[["Amount"]])),
        asana_li("Contract End Date", fmt_date(r[["Current Contract End Date"]])),
        asana_li("Churn Quarter",     safe_val(r[["Churn Quarter"]])),
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
        asana_li("Owner",         safe_val(r[["Opportunity Owner"]])),
        asana_li("Amount",        fmt_usd(r[["Amount"]])),
        asana_li("Fiscal Period", safe_val(r[["Fiscal Period"]])),
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


# Micro helpers (safe_val, fmt_usd, fmt_date, asana_li, html_escape)
# are defined in R/utils_internal.R
