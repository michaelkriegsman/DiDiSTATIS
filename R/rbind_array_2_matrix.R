#The DiDiSTATIS function within DiDiSTATIS
#
#'For permutation of tables of DiDiSTATIS, Plot each stimulus in the Grand Compromise
#'
#'@param the_array a 3-d or 4-d data array
#'@return the matrix
#'@export

rbind_array_2_matrix <- function(the_array){

  if(length(dim(the_array))==3){

    #example
    #the_array <- array(1:18, dim=c(3,2,3))
    #the_array
    #matrix(apply(the_array, c(3,2), c) , dim(the_array)[1]*dim(the_array)[3], dim(the_array)[2])

    the_matrix <- matrix(apply(the_array, c(3,2), c) , dim(the_array)[1]*dim(the_array)[3], dim(the_array)[2])
  }


  if(length(dim(the_array))==4){

    #example 1
    #the_array <- array(1:36, dim=c(3,2,3,2))
    #matrix(apply(the_array, c(4,2), c) , dim(the_array)[1]*dim(the_array)[3]*dim(the_array)[4], dim(the_array)[2])

    #example 2
    #the_array <- array(1:108, dim=c(4,3,3,3))
    the_matrix <- matrix(apply(the_array, c(4,2), c) , dim(the_array)[1]*dim(the_array)[3]*dim(the_array)[4], dim(the_array)[2])
  }

  return(the_matrix)

}

