#DiDiSTATIS_Summary_Stats_Table.R
## For instructions, see https://davidgohel.github.io/ReporteRs/articles/FlexTable.html#table-model

#' Print a table of summary stats
#'
#' @param res_DiDiSTATIS DiDiSTATIS output with all inference
#' @param main Table title
#' @return A table of summary stats
#' @export

DiDiSTATIS_Summary_Stats_Table <- function(res_DiDiSTATIS, main = NULL){

  Eff <- res_DiDiSTATIS$res_BaryGrand$EffectSize
  Pred_Fix <- res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows
  Pred_LOO <- res_DiDiSTATIS$LOO_Rows
  Pred_SH <- res_DiDiSTATIS$SH_Rows


  ROWNAMES <- c("SS_Plain", "SS_Disc", "SS_B", "SS_A(B)",
                "r2_Plain_Disc", "r2_Plain_B", "r2_Categories",
                "Fixed Pred Accur", "LOO Pred Accur", "SH Pred Accur")
  Grand    <- c(Eff$SS_plain.., Eff$SS_ab.., Eff$SS_.b.., Eff$SS_aINb..,
                Eff$r2_Plain_Disc_.., Eff$r2_Plain_b_.., Eff$r2_Categories..,
                Pred_Fix$Class_accuracy, Pred_LOO$Grand$Class_accuracy, Pred_SH$Grand$Class_accuracy)
  # Group    <- c(Eff$SS_plain.D[d], Eff$SS_ab.D[d], Eff$SS_.b.D[d], Eff$SS_aINb.D[d],
  #               Eff$r2_Plain_Disc_.D[d], Eff$r2_Plain_b_.D[d], Eff$r2_Categories.D[d],
  #               Pred_Fix$Class_accuracy.d[d], Pred_LOO$Group$Class_accuracy_D[d], Pred_SH$Group$Class_accuracy_D[d])




  TheTable <- data.frame(ROWNAMES, Grand, row.names = 1)

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    GetGroupEffects <- paste0("Group_d", d, "<- c(Eff$SS_plain.D[d], Eff$SS_ab.D[d], Eff$SS_.b.D[d], Eff$SS_aINb.D[d],
                             Eff$r2_Plain_Disc_.D[d], Eff$r2_Plain_b_.D[d], Eff$r2_Categories.D[d],
                             Pred_Fix$Class_accuracy.d[d], Pred_LOO$Group$Class_accuracy_D[d], Pred_SH$Group$Class_accuracy_D[d])")
    eval(parse(text = GetGroupEffects))

    add_column <- paste0("TheTable <- cbind(TheTable, Group_d", d,")")
    eval(parse(text = add_column))
  }

  TheColumnNames <- c("Grand", paste0(res_DiDiSTATIS$input$DESIGN_tables$labels))
  colnames(TheTable) <- TheColumnNames

  return(TheTable)
}




