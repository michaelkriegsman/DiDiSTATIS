#Pick tables to leave out... for a LOO on the tables of HiDiSTATIS
#
#'Identify a matrix of indices for LOO_tables
#'
#'@param DESIGN_tables DESIGN info for tables (participants nested in groups)
#'@param multiplier parameter to increase the number of iterations
#'@return A matrix of table indices for each iteration of the LOO
#'@export

Pick_tables_to_leave_out <- function(DESIGN_tables = DESIGN_tables, multiplier = 1){

  #Define the total number of iterations.
  #For each iteration, we'll predict the positions of all AB stimuli from the perspectives of D randomly-selected participants.
  #For now, let's choose to iterate CD*D times. This will ensure that each CD table is Predicted at least D times.
  #Can also boost that number of iterations by multiplier
  total_iter <- DESIGN_tables$CD * DESIGN_tables$D * multiplier
  Leave_out_these_tables <- matrix(NA, DESIGN_tables$D, total_iter)

  counter <- 0
  Left_out_tables <- matrix(NA, nrow(Leave_out_these_tables), 1)
  for(d in 1:(DESIGN_tables$D * multiplier)){
    for(cd in 1:DESIGN_tables$CD){

      counter <- counter + 1

      #identify the category B of stimulus ab
      Group_of_primary_Left_out <- which(DESIGN_tables$mat[cd,]==1)

      #Assign that left out table to its group
      Left_out_tables[Group_of_primary_Left_out] <- cd

      #sample 1 table from each of the other D groups
      The_other_groups <- c(1:ncol(DESIGN_tables$mat))[-Group_of_primary_Left_out]
      for(j in The_other_groups){
        Left_out_tables[j] <- sample(which(DESIGN_tables$mat[,j]==1),1)
      }

      #and store as columns of eave_out_these_rows
      Leave_out_these_tables[,counter] <- Left_out_tables
    }
  }

  return(Leave_out_these_tables)
}
