#
#'Flip_mat is a matrix to manually flip components to match results across methods
#'
#'@param Flip_axis1 Boolean to Flip Factor Scores of axis1
#'@param Flip_axis2 Boolean to Flip Factor Scores of axis2
#'@return Flip_mat
#'@export


makeFlip_mat <- function(Flip_axis1, Flip_axis2){

  if(Flip_axis1){
    Flip_1 <- -1
  }else{
    Flip_1 <- 1
  }

  if(Flip_axis2){
    Flip_2 <- -1
  }else{
    Flip_2 <- 1
  }

  Flip_mat <- diag(c(Flip_1, Flip_2))

  return(Flip_mat)

}

