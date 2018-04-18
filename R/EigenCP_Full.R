#EigenCP_Full.R

#' Eigen-decompose a CP matrix.
#'
#' @param CP A cross-product matrix
#' @param DESIGN_rows List of DESIGN info for the rows
#' @param n2k Number of components 2 keep
#' @return Basic output and a PrettyPlot
#' @export

EigenCP_Full <- function(CP, DESIGN_rows = DESIGN_rows, n2k=NULL){

  res_CP <- list()
  res_CP$input$CP <- CP

  #eigen-decompose
  res <- eigen(CP)

  #prepare to remove empty (or unwanted, see n2k) components
  precisionLimit <- 2 * .Machine$double.eps
  indToKeep <- which(res$values > precisionLimit)
  indToKeep <- 1:min(n2k, length(indToKeep))

  #Define output (U, Lambda_vec, Lambda, ProjMat, t, F, and Ctrb)

  #U_Full
  res_CP$eig$U_Full <- res$vectors[,indToKeep]
  dimnames(res_CP$eig$U_Full) <- list(rownames(CP), paste('Comp ', indToKeep, sep=""))

  #U_Cond
  res_CP$eig$U_Cond <- DESIGN_rows$Pb_Cond %*% res_CP$eig$U_Full

  #U_Cond_Sqrt
  res_CP$eig$U_Cond_Sqrt <- DESIGN_rows$Pb_Cond_Sqrt %*% res_CP$eig$U_Full

  #Lambda_vec
  res_CP$eig$Lambda_vec <- res$values[indToKeep]

  #Lambda
  res_CP$eig$Lambda <- diag(res_CP$eig$Lambda_vec)
  dimnames(res_CP$eig$Lambda) <- list(paste('Comp ', indToKeep, sep=""), paste('Comp ', indToKeep, sep=""))

  #ProjMat_Full
  res_CP$eig$ProjMat_Full <- res_CP$eig$U_Full %*% solve(res_CP$eig$Lambda^(1/2))
  res_CP$eig$ProjMat_Cond <- res_CP$eig$U_Cond %*% solve(res_CP$eig$Lambda^(1/2))
  res_CP$eig$ProjMat_Cond_Sqrt <- res_CP$eig$U_Cond_Sqrt %*% solve(res_CP$eig$Lambda^(1/2))

  #tau (cumulative percent variance)
  res_CP$eig$t <- 100 * res_CP$eig$Lambda_vec / sum(res_CP$eig$Lambda_vec)

  #Compute F
  #and if sum(F[,1]) < 0, then F[,1] * -1
  # res_CP$eig$F_Full <- FlipCheck(CP %*% res_CP$eig$ProjMat_Full)
  res_CP$eig$F_Full <- CP %*% res_CP$eig$ProjMat_Full
  
  #F_Cond (does't work the 2nd way... when condense columns, need BtB)
  res_CP$eig$F_Cond <- DESIGN_rows$Pb_Cond %*% res_CP$eig$F_Full
  # res_CP$eig$F_Cond <- DESIGN_rows$Pb_Cond %*% CP %*% t(DESIGN_rows$Pb_Cond) %*% res_CP$eig$ProjMat_Cond
  
  # res_CP$eig$F_Cond_Sqrt <- DESIGN_rows$Pb_Cond_Sqrt %*% res_CP$eig$F_Full
  res_CP$eig$F_Cond_Sqrt <- DESIGN_rows$Pb_Cond %*% CP %*% t(DESIGN_rows$Pb_Cond) %*% res_CP$eig$ProjMat_Cond_Sqrt

  #Contributions
  res_CP$eig$Ctrb_Full <- (res_CP$eig$F_Full^2) / repmat(t(res_CP$eig$Lambda_vec),nrow(res_CP$eig$U_Full),1)
  res_CP$eig$Ctrb_Cond <- t(DESIGN_rows$mat) %*% res_CP$eig$Ctrb_Full


  return(res_CP)
}
