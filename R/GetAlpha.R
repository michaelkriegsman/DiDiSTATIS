#'Obtain a vector of alpha-weights from an inner product matrix (RV or C)
#'
#'@param C The inner product (RV or C) matrix
#'@return a vector of alpha-weights
#'@export



#GetAlpha.R

GetAlpha <- function(C){

  returnME <- list()
  returnME$res_Rv <- EigenCP(C)

  abs_u1 <- abs(returnME$res_Rv$eig$U[, 1])
  returnME$alpha <- abs_u1 / sum(abs_u1)

  return(returnME)

}
