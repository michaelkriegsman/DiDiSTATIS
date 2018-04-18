#DblCenterD2.R
#adapted from DistatisR

#' Double Center a squared distance matrix
#'
#' @param D2 a squared distance matrix
#' @return a cross product matrix
#' @export


DblCenterD2 <- function(D2) {
  nI = nrow(D2)
  CentMat = diag(nI) - (1/nI) * matrix(1, nI, nI)
  S = -(1/2) * (CentMat %*% D2 %*% CentMat)
  return(S)
}
