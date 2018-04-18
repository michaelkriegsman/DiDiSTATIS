#' Transform Sorting data to a Distance Matrix
#'
#' @param X A rectangular matrix of sorting data (nominal; stimuli by participants)
#' @return An array of (sorting) distance matrices
#' @export

#DistanceFromSort.R
#copied from DistatisR

DistanceFromSort <- function (X){

  Sort2Dist <- function(LeSort) {
    nObj = length(LeSort)
    truc = matrix(as.matrix(LeSort), nrow = nObj, ncol = nObj)
    DistMat = 1 - (truc == t(truc))
    return(DistMat)
  }

  nI = nrow(X)
  nJ = ncol(X)
  LeCube2Distance = array(0, c(nI, nI, nJ))
  for (j in 1:nJ) {
    LeCube2Distance[, , j] <- Sort2Dist(X[, j])
  }
  dimnames(LeCube2Distance) <- list(rownames(X), rownames(X),
                                    colnames(X))
  return(LeCube2Distance)
}
