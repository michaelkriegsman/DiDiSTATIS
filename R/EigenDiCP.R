#EigenDiCP.R

#' Discriminant Eigen-decomposition for a CP matrix.
#'
#' @param CP A cross-product matrix
#' @param DESIGN_rows DESIGN matrix for the rows
#' @param n2k Number of components 2 keep
#' @return Computational output and PrettyPlots
#' @export

EigenDiCP <- function(CP, DESIGN_rows = NULL, n2k=NULL){

  res_DiCP <- list()
  res_DiCP$input$CP <- CP
  res_DiCP$input$DESIGN$rows <- DESIGN_rows

  #Define the projector matrices
  Pb  <-  res_DiCP$input$Pb  <-  Bary_Projector(DESIGN_rows)
  Pab <-  res_DiCP$input$Pab <-  Ortho_Projector(Pb)

  #Project into the barycentric and orthogonal space
  Pb_CP_Pb   <- Pb %*%  CP %*% Pb
  Pab_CP_Pab <- Pab %*% CP %*% Pab

  #eigen-decompose
  res <- eigen(Pb_CP_Pb)

  #prepare to remove empty (or unwanted, see n2k) components
  precisionLimit <- 2 * .Machine$double.eps
  indToKeep <- which(res$values > precisionLimit)
  indToKeep <- 1:min(n2k, length(indToKeep))


  #Define output (U, Lambda_vec, Lambda, ProjMat, t, F, and Ctrb)

  #Ub
  res_DiCP$eigPbCPPb$Ub <- as.matrix(res$vectors[,indToKeep])
  rownames(res_DiCP$eigPbCPPb$Ub) <- rownames(CP)
  colnames(res_DiCP$eigPbCPPb$Ub) <- paste('Comp ', indToKeep, sep="")

  #Lambdab_vec
  res_DiCP$eigPbCPPb$Lambdab_vec <- res$values[indToKeep]
  if(any(res_DiCP$eigPbCPPb$Lambdab_vec[1] < (sum(res_DiCP$eigPbCPPb$Lambdab_vec) / 10000))){
    print('Warning: At least 1 very small eigen-value. Could give funky factor scores. Consider manually removing.')
  }

  #Lambdab
  res_DiCP$eigPbCPPb$Lambdab <- diag(res_DiCP$eigPbCPPb$Lambdab_vec)
  if(length(indToKeep)!=1){
    rownames(res_DiCP$eigPbCPPb$Lambdab) <- paste('Comp ', indToKeep, sep="")
    colnames(res_DiCP$eigPbCPPb$Lambdab) <- paste('Comp ', indToKeep, sep="")
  }

  #ProjMatb
  res_DiCP$eigPbCPPb$ProjMatb <- res_DiCP$eigPbCPPb$Ub %*% solve(res_DiCP$eigPbCPPb$Lambdab^(1/2))

  #tau (cumulative percent variance)
  res_DiCP$eigPbCPPb$tb <- 100 * res_DiCP$eigPbCPPb$Lambdab_vec / sum(res_DiCP$eigPbCPPb$Lambdab_vec)

  #Fb (from the eigen-decomposition)
  res_DiCP$eigPbCPPb$Fb <- res_DiCP$eigPbCPPb$Ub %*% res_DiCP$eigPbCPPb$Lambdab^(1/2)

  #Compute F
  #and if sum(F[,1]) < 0, then F[,1] * -1
  res_DiCP$ProjCP$F   <- FlipCheck(CP       %*% res_DiCP$eigPbCPPb$ProjMatb)
  res_DiCP$ProjCP$Fb  <- FlipCheck(Pb_CP_Pb %*% res_DiCP$eigPbCPPb$ProjMatb)
  res_DiCP$ProjCP$Fa_in_b <- res_DiCP$ProjCP$F - res_DiCP$ProjCP$Fb
    round(res_DiCP$ProjCP$F, 2)
    round(res_DiCP$ProjCP$Fb, 2)
    round(res_DiCP$ProjCP$Fa_in_b, 2)
  # res_DICP$ProjCP$Fe

  #Contributions (of barycenters to barycentric space)
  res_DiCP$eigPbCPPb$Ctrb <- (res_DiCP$eigPbCPPb$Fb^2) / repmat(t(res_DiCP$eigPbCPPb$Lambdab_vec),nrow(res_DiCP$eigPbCPPb$Ub),1)


  #Describe Spaces
  SStotaltotal <- res_DiCP$SS$totaltotal <- sum(diag(CP))                  # SS for the original data
  SStotal      <- res_DiCP$SS$total      <- sum(res_DiCP$ProjCP$F^2)       # SS of original data projected into barycentric space
  SSb          <- res_DiCP$SS$b          <- sum(res_DiCP$ProjCP$Fb^2)      # SS_between in barycentric space     # == sum((Pb %*% res_DiCP$ProjCP$F)^2)
  SSa_in_b     <- res_DiCP$SS$a_in_b     <- sum(res_DiCP$ProjCP$Fa_in_b^2) # SS_within in barycentric space      # == SStotal - SSb
  SSe          <- res_DiCP$SS$e          <- SStotaltotal - SStotal         # SS that failed to project

  #r2
  #barycentric potato
  r2total.b         <- res_DiCP$r2$total.b       <- SSb / SStotal
  r2total.a_in_b    <- res_DiCP$r2$total.a_in_b  <- SSa_in_b / SStotal
  #r2total.b + r2total.a_in_b == 1
  paste0('Total Barycentric = Between (',round(r2total.b,2),') + Within (',round(r2total.a_in_b,2),')')

  #original potato
  r2totaltotal.b      <- res_DiCP$r2$totaltotal.b       <- SSb / SStotaltotal
  r2totaltotal.a_in_b <- res_DiCP$r2$totaltotal.a_in_b  <- SSa_in_b / SStotaltotal
  r2totaltotal.e      <- res_DiCP$r2$totaltotal.e       <- SSe / SStotaltotal
  #r2totaltotal.b + r2totaltotal.a_in_b + r2totaltotal.e == 1
  paste0('TotalTotal = Between (',round(r2totaltotal.b,2),') + Within (',round(r2totaltotal.a_in_b,2),') + Error of Projection (',round(r2totaltotal.e, 2),')')

  #Component 1 res (not currently needed... the purpose woudl be to compare the effect of B on Comp 1 in MDS vs DiMDS)


  # #Graphs
  # if(graphs){
  #   if(length(indToKeep)==1){
  #     print("Warning: I don't currently support 1-D plotting")
  #   }else{
  #     #PlotDiCP(res_DiCP, axes = c(1,2), main=main)
  #     DiMDS_Plot_Fb()
  #   }
  # }
  return(res_DiCP)
}
