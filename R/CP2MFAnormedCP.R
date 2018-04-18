#CP2MFAnormedCP.R
#copied from DistatisR

#'MFA normalize an array of cross-product matrices
#'
#'@param CP3 An array of cross-product matrices
#'@return An array of MFA-normalized cross-product matrices
#'@export

CP2MFAnormedCP <- function(CP3) {
  CP3normed <- array(apply(CP3, 3, MFAnormCP), dim = dim(CP3))
  dimnames(CP3normed) <- dimnames(CP3)
  return(CP3normed)
}
