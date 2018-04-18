#MFAnormCP.R
#copied from DistatisR

#'MFA normalize a psd matrix
#'
#'@param psd A psd (such as cross-product) matrix
#'@return An MFA-normalized psd matrix
#'@export

MFAnormCP <- function(psd) {
  ev = eigen(psd, symmetric = T, only.values = T)[1]
  e1 = ev$values[1]
  psd_normed = psd/e1
  return(psd_normed)
}
