#Dist2CP.R
#copied from DistatisR

#' Apply DblCenterDist to an array of squared distance matrices
#'
#' @param D3 An array of squared distance matrices
#' @return An array of CP matrices
#' @export



Dist2CP <- function(D3) {
  CP3 <- (array(apply(D3, 3, DblCenterD2), dim = c(dim(D3)[1],
                                                     dim(D3)[2], dim(D3)[3])))
  dimnames(CP3) <- dimnames(D3)
  return(CP3)
}
