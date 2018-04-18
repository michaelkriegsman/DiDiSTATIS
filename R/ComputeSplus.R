#ComputeSplus.R

#' Compute a compromise, as defined in DiSTATIS
#'
#' @param CubeCP Array of (double-centered) cross-product matrices
#' @param alpha Vector of alpha-weights
#' @return The compromise
#' @export


ComputeSplus <- function(CubeCP, alpha){
  nJ = dim(CubeCP)[3]
  nI = dim(CubeCP)[1]
  Splus = matrix(0, nI, nI)

  Splus <- apply(apply(CubeCP, c(1, 2), "*", t(alpha)), c(2, 3), sum)
  return(Splus)
}

