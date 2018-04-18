# MFAnormCPFinder_1.R

#'Identify the coefficient to MFA normalize a matrix
#'
#'@param Y A cross-product matrix
#'@return The first eigen-value of Y
#'@export


#create the internal function
MFAnormCPFinder_1 <- function(Y) {
  ev = eigen(Y, symmetric = T, only.values = T)[1]
  e1 = solve(ev$values[1])
  return(e1)
}
