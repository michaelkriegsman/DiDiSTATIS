#' Compute an inner-product matrix (RV or C) from an array of CP matrices
#'
#' @param CubeCP an array of CP matrices
#' @param RV flag to normalize. TRUE gives RV. FALSE gives standard inner product, C.
#' @return Inner product matrix
#' @export

#adapted from DistatisR

GetCmat <- function(CubeCP, RV = FALSE) { ######### function #########
  nI = dim(CubeCP)[1]
  nJ = dim(CubeCP)[3]
  CP2 = array(CubeCP, dim = c(nI * nI, nJ))
  C = t(CP2) %*% CP2
  if (RV==TRUE) {
    laNorm = sqrt(apply(CP2^2, 2, sum))
    C = C/(t(t(laNorm)) %*% laNorm)
  }
  dimnames(C) <- list(dimnames(CubeCP)[[3]],dimnames(CubeCP)[[3]])
  return(C)
}
