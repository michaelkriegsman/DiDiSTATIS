#SortDist2Sort.R

#'Transform a Sorting Distance matrix to a column of the Sorting matrix
#'
#'@param SortDist A given sorting distance matrix
#'@return A column of the sorting matrix
#'@export


SortDist2Sort <- function(SortDist){
  count <- 1
  Sort_vec <- matrix(0, nrow(SortDist), 1)
  for(i in 1:nrow(SortDist)){
    next_blank_in_Sort_vec <- which(Sort_vec==0)[1]
    if(is.na(next_blank_in_Sort_vec)) {return(Sort_vec)}
    Sort_vec[which(1-SortDist[next_blank_in_Sort_vec,]==1)] <- count
    count <- count + 1
  }
}
