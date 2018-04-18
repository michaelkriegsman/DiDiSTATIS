#'Compute SS from factor scores
#'
#'@param F Factor scores
#'@return Sum of Squares
#'@export

SS_from_F <- function(F){

  CP <- t(F) %*% F
  SS <- sum(diag(CP))

  return(SS)
}
