#'Plot results from omnibus test of Null Hypothesis
#'
#'@param res_DiDiSTATIS Output from DiDiSTATIS with Perm_omnibus==TRUE
#'@return Histograms of the Dev2's
#'@export


PlotDiDiSTATIS_Perm_Omni <- function(res_DiDiSTATIS){

  hist(res_DiDiSTATIS$Perm_Omnibus$r2$disc.cd_B.D.perm, breaks=50)
  abline(v = quantile(res_DiDiSTATIS$Perm_Omnibus$r2$disc.cd_B.D.perm, .95), col="red")
  abline(v = res_DiDiSTATIS$res_BaryGrand$r2$disc.cd_B.D, col="blue")


  hist(res_DiDiSTATIS$Perm_Omnibus$r2$Plain_disc.perm, breaks=50)
  abline(v = quantile(res_DiDiSTATIS$Perm_Omnibus$r2$Plain_disc.perm, .95), col="red")
  abline(v = res_DiDiSTATIS$res_BaryGrand$r2$Plain_disc, col="blue")

}
