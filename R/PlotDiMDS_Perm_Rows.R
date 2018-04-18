#'Plot results from test of Null Hypothesis on Rows
#'
#'@param res_DiMDS Output from DiMDS with Perm_rows==TRUE
#'@return Histograms of the r2's
#'@export


PlotDiMDS_Perm_Rows <- function(res_DiMDS){

  hist(res_DiMDS$Perm_Rows$r2total.b_perm, breaks=50, main="DiMDS: r2total.b_perm")
  abline(v = quantile(res_DiMDS$Perm_Rows$r2total.b_perm, .95), col="red", lwd=4)
  abline(v = res_DiMDS$input$r2total.b_Full, col='blue')


  hist(res_DiMDS$Perm_Rows$r2disc_perm.b_perm, breaks=50, main="DiMDS: r2disc_perm.b_perm")
  abline(v = quantile(res_DiMDS$Perm_Rows$r2disc_perm.b_perm, .95), col="red", lwd=4)
  abline(v = res_DiMDS$res_Disc_Full$proj2Bary$r2disc.B, col='blue')

}
