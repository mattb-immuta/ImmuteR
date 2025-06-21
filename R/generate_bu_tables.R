#' generate customer operations tracker data from renewals and expansions.
#'
#' @param op_list the updated list enriched list object.
#' @param old_list the enriched list object to compare and identify changes
#' @return a customer operations tracker data table with expansion context.
#' @examples
#' generate_bu_tables(op_list)
#' @export
generate_bu_tables<-function(op.table){
  #op.table=op_list
  temp.expansions=op.table$expansions[,c("Account ID","Amount")]
  temp.expansions=temp.expansions %>%
    group_by(`Account ID`) %>%
    summarise(total_amount = sum(Amount),active_expansions = n())

  customer_ops_tracker=merge(op.table$renewals[,c("Account ID","account_url","opportunity_url","Amount","Close Date",
                                                  "Fiscal Period","Region",
                                                  "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                                  "consumption_url",
                                                  "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                                  "re_hyperlink")],
                             temp.expansions,by="Account ID",all.x=TRUE)

  customer_ops_tracker=customer_ops_tracker[,c("account_url","opportunity_url","Amount","Close Date","Fiscal Period","Region",

                                               #account health
                                               "Adoption Risk","Stage","registered_users","licensed_users","utilization_risk",
                                               "consumption_url","active_expansions","total_amount",

                                               #account team engagement
                                               "Next Step","RD Comment","Date of last QBR/EBR","Date of next QBR/EBR",
                                               "re_hyperlink")]
  op.table[["customer_ops_tracker"]]=customer_ops_tracker
  return(op.table)
}
