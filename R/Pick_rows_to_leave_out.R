#Pick rows to leave out... for a LOO on the rows
#
#'Identify a matrix of row indices for LOO_rows
#'
#'@param DESIGN_rows DESIGN matrix for rows (stimuli nested in categories)
#'@param multiplier parameter to increase the number of iterations
#'@return A matrix of row indices for each iteration of the LOO
#'@export

Pick_rows_to_leave_out <- function(DESIGN_rows = DESIGN_rows, multiplier = 1){

  #Define the total number of iterations.
  #For each iteration, we'll predict the positions of B randomly-selected stimuli.
  #For now, let's choose to iterate AB*B times. This will ensure that each row is Predicted at least B times.
  #Can also boost that number of iterations by multiplier
  total_iter <- DESIGN_rows$AB * DESIGN_rows$B * multiplier
  Leave_out_these_rows <- matrix(NA, DESIGN_rows$B, total_iter)

  counter <- 0
  Left_out_rows <- matrix(NA, nrow(Leave_out_these_rows), 1)
  for(b in 1:(DESIGN_rows$B * multiplier)){
    for(ab in 1:DESIGN_rows$AB){

      counter <- counter + 1

      #identify the category B of stimulus ab
      Category_of_primary_Left_out <- which(DESIGN_rows$mat[ab,]==1)

      #Assign that left out stimulus (row number) to its category (to keep the order of Left_out consistent)
      Left_out_rows[Category_of_primary_Left_out] <- ab

      #sample 1 stimulus from each of the other Bs
      The_other_categories <- c(1:ncol(DESIGN_rows$mat))[-Category_of_primary_Left_out]
      for(j in The_other_categories){
        Left_out_rows[j] <- sample(which(DESIGN_rows$mat[,j]==1),1)
      }

      #and store as columns of eave_out_these_rows
      Leave_out_these_rows[,counter] <- Left_out_rows
    }
  }

  return(Leave_out_these_rows)
}
