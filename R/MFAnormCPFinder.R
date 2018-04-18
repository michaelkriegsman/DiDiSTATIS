# MFAnormCPFinder.R

#'Identify coefficients to MFA normalize an array
#'
#'@param CP_array An array of cross-product matrices
#'@return a vector of MFAnorm coefficients
#'@export


MFAnormCPFinder <- function(CP_array) {
  MFAnorm_coefficients <- array(apply(CP_array, 3, MFAnormCPFinder_1), dim = dim(CP_array)[3])
  return(MFAnorm_coefficients)
}

