#SortFromSortDist.R

#'Transform from an array of Sorting Distance to a Sorting matrix
#'
#'@param SortDistArray Array of sorting distance matrices
#'@return The sorting matrix
#'@export


SortFromSortDist <- function(SortDistArray){

  nI <- dim(SortDistArray)[1]
  nJ <- dim(SortDistArray)[3]
  Sort <- matrix(NA, nI, nJ, dimnames = dimnames(SortDistArray)[c(1,3)])
  for(j in 1:nJ){
    Sort[,j] <- SortDist2Sort(SortDistArray[,,j])
  }
  return(Sort)
}

