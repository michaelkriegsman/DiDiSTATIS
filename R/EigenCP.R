#EigenCP.R

#' Eigen-decompose a CP matrix.
#'
#' @param CP A cross-product matrix
# #'@param DESIGN List of DESIGN info
#' @param n2k Number of components 2 keep
#' @return Basic output and a PrettyPlot
#' @export

EigenCP <- function(CP, n2k=NULL){

  res_CP <- list()
  res_CP$input$CP <- CP
  # res_CP$input$DESIGN_rows_mat <- DESIGN$rows$mat
  # res_CP$input$DESIGN_rows_cols_B <- DESIGN$rows$colors_B
  # res_CP$input$DESIGN_rows_cols_AB <- DESIGN$rows$colors_AB
  # res_CP$input$DESIGN <- DESIGN

  #eigen-decompose
  res <- eigen(CP)

  #prepare to remove empty (or unwanted, see n2k) components
  precisionLimit <- 10 * .Machine$double.eps #3.6.2018 changed multiplier from 2 to 10
  indToKeep <- which(res$values > precisionLimit)
  indToKeep <- 1:min(n2k, length(indToKeep))

  #Define output (U, Lambda_vec, Lambda, ProjMat, t, F, and Ctrb)

  #U
  res_CP$eig$U <- FlipCheck(res$vectors[,indToKeep])
  ### dimnames(res_CP$eig$U) <- list(rownames(CP), paste('Comp ', indToKeep, sep=""))

  #Lambda_vec
  res_CP$eig$Lambda_vec <- res$values[indToKeep]

  #Lambda
  res_CP$eig$Lambda <- diag(as.matrix(res_CP$eig$Lambda_vec))
  ### dimnames(res_CP$eig$Lambda) <- list(paste('Comp ', indToKeep, sep=""), paste('Comp ', indToKeep, sep=""))

  #PojMat
  res_CP$eig$ProjMat <- res_CP$eig$U %*% solve(res_CP$eig$Lambda^(1/2))

  #tau (cumulative percent variance)
  res_CP$eig$t <- 100 * res_CP$eig$Lambda_vec / sum(res_CP$eig$Lambda_vec)

  #Compute F
  #and if sum(F[,1]) < 0, then F[,1] * -1
  res_CP$eig$F <- #res_CP$eig$U %*% res_CP$eig$Lambda^(1/2)
                   CP %*% res_CP$eig$ProjMat

  #Contributions
  res_CP$eig$Ctrb <- (res_CP$eig$F^2) / repmat(t(res_CP$eig$Lambda_vec),nrow(res_CP$eig$U),1)

  return(res_CP)
}
