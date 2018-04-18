#Pick rows to split... for a SH on the rows
#
#'Identify a matrix of row indices for SH_rows
#'
#'@param DESIGN_rows DESIGN matrix for rows (stimuli nested in categories)
#'@param niter Number of iterations
#'@return A matrix of row indices for each iteration of the SH
#'@export

Pick_rows_to_split <- function(DESIGN_rows = DESIGN_rows, niter = niter){

  #Remove half of the row stimuli from each Category.
    #How many to remove from each category?
    how_many_to_remove_from_each_category <- floor(colSums(DESIGN_rows$mat)/2)
    total_rows_to_leave_out <- sum(how_many_to_remove_from_each_category)

    #Initialize the matrix to store left out row indices
    Leave_out_these_rows <- matrix(NA, total_rows_to_leave_out, niter)

    #Identify the specific stimuli to remove from each category
    for(i in 1:niter){

      leave_out <- vector()
      for(b in 1:DESIGN_rows$B){
        leave_out <- c(leave_out,
                       sort(sample(which(DESIGN_rows$mat[,b]==1), how_many_to_remove_from_each_category[b]))
        )
      }

      Leave_out_these_rows[,i] <- leave_out

    }

  return(Leave_out_these_rows)

}
