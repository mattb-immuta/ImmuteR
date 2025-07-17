
#' Load prioritization and forecast constants.
#'
#' This function takes two numeric inputs and returns their sum.
#'
#' @return A list with a probability matrix, phat, a risk index vector, and arr index.
#' @export
#'
load_constants<-function(risk.vec=c('3. Low Risk','2. Moderate Risk','1. High Risk','Planned Lost','Closed Lost','0. Will Lose','0. Will Lose (Formally Notified)','Planned Won',"Closed Won")){
  c=list()
  c$phat_adoption <- data.table(worst_case = c(.9,.75,.5,0,0,0,0,1,1),
                                expected = c(.95,.85,.675,0,0,0,0,1,1),
                                best_case = c(1,.9,.75,0,0,0,0,1,1),
                                risk.vec)


  c$risk_index = risk.vec
  c$arr_index = c(0,150000,
                  500000)
  return(c)
}
