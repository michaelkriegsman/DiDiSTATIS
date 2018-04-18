#Dev2


#'Compute the squared distances between rows of two matrices 
#'
#'@param from The first matrix
#'@param to The second matrix
#'@return The squared distances
#'@export


Dev2 <- function(from, to){
  
  Dev2_from_to <- matrix(NA, nrow(from), nrow(to), dimnames = list(rownames(from), rownames(to)))
  
  for(i in 1:nrow(from)){
    for(j in 1:nrow(to)){
      Dev2_from_to[i,j] <- sum((from[i,] - to[j,])^2)
    }
  }
  
  return(Dev2_from_to)
  
}