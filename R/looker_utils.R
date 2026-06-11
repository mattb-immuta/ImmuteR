#' Authenticate with the Looker API
#'
#' Reads credentials from environment variables \code{LOOKER_CLIENT_ID} and
#' \code{LOOKER_CLIENT_SECRET}. The base URL defaults to
#' \code{LOOKER_BASE_URL} or \code{"https://immuta.cloud.looker.com"}.
#'
#' @return Character. Bearer token for use in subsequent API calls.
#' @export
looker_auth <- function() {
  base_url      <- Sys.getenv("LOOKER_BASE_URL", "https://immuta.cloud.looker.com")
  client_id     <- Sys.getenv("LOOKER_CLIENT_ID")
  client_secret <- Sys.getenv("LOOKER_CLIENT_SECRET")

  if (client_id == "" || client_secret == "") {
    stop(
      "Looker credentials not found. ",
      "Set LOOKER_CLIENT_ID and LOOKER_CLIENT_SECRET environment variables."
    )
  }

  resp <- httr2::request(paste0(base_url, "/api/4.0/login")) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(client_id = client_id, client_secret = client_secret) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$access_token
}

#' Run a saved Looker look and return a data frame
#'
#' Calls \code{GET /api/4.0/looks/{look_id}/run/csv} on the Looker instance
#' and parses the response into a tibble.
#'
#' @param look_id       Integer or character. The numeric look ID to run.
#' @param token         Character. Bearer token from \code{looker_auth()}.
#' @param result_format Character. \code{"csv"} (default) or \code{"json"}.
#' @param base_url      Character. Looker instance base URL.
#'
#' @return A tibble with the look's result rows.
#' @export
run_looker_look <- function(look_id,
                             token,
                             result_format = "csv",
                             base_url = Sys.getenv("LOOKER_BASE_URL",
                                                    "https://immuta.cloud.looker.com")) {
  url <- paste0(base_url, "/api/4.0/looks/", look_id, "/run/", result_format)

  resp <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("token", token)) |>
    httr2::req_perform()

  if (result_format == "csv") {
    readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
  } else {
    httr2::resp_body_json(resp, simplifyVector = TRUE) |> tibble::as_tibble()
  }
}
