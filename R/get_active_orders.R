#' Fetch all active Salesforce orders for a given account.
#'
#' Queries the Salesforce \code{Order} object for records with \code{Status = "Active"}
#' matching the supplied account name.
#'
#' @param account_name Character. The Salesforce account name to query.
#'
#' @return A data frame of active orders with columns \code{Id},
#'   \code{OrderNumber}, \code{EffectiveDate}, \code{Status}, \code{TotalAmount},
#'   and \code{Account.Name}, or \code{NULL} if no active orders exist.
#' @export
get_active_orders <- function(account_name) {
  clean_name <- gsub("'", "\\\\'", account_name)
  query <- sprintf("
    SELECT Id, OrderNumber, EffectiveDate, Status, TotalAmount, Account.Name
    FROM Order
    WHERE Account.Name = '%s'
    AND Status = 'Active'
  ", clean_name)

  results <- salesforcer::sf_query(query)

  if (nrow(results) == 0) {
    message(paste("No active orders found for account:", account_name))
    return(NULL)
  }
  return(results)
}


#' Fetch all line items for a given Salesforce order number.
#'
#' Queries \code{OrderItem} records associated with the supplied order number,
#' returning product name, quantity, pricing, and service date.
#'
#' @param order_num Character. The Salesforce \code{OrderNumber} to query.
#'
#' @return A data frame of order line items, or \code{NULL} if none exist.
#' @export
get_products_by_order_number <- function(order_num) {
  query <- sprintf("
    SELECT
      Product2.Name,
      Quantity,
      UnitPrice,
      TotalPrice,
      ServiceDate,
      Description
    FROM OrderItem
    WHERE Order.OrderNumber = '%s'
  ", order_num)

  results <- salesforcer::sf_query(query)

  if (nrow(results) == 0) {
    message(paste("No products found for Order Number:", order_num))
    return(NULL)
  }
  return(results)
}


#' Fetch the child object relationships for the Salesforce Order object.
#'
#' Useful for exploring what related objects (e.g. OrderItems, Amendments) are
#' accessible from an Order record via SOQL.
#'
#' @return A data frame with columns \code{childSObject}, \code{relationshipName},
#'   and \code{field}.
#' @export
get_order_child_relationships <- function() {
  order_metadata <- salesforcer::sf_describe_object_fields("Order")
  order_metadata$childRelationships %>%
    dplyr::bind_rows() %>%
    dplyr::select(childSObject, relationshipName, field)
}
