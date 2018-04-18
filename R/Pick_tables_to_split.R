#Pick tables to split... for a SH on the tables
#
#'Identify a matrix of indices for SH_talbes
#'The rows of the matrix produced correspond to the tables to leave out in each iteration
#'
#'@param DESIGN_tables DESIGN matrix for tables (participants nested in groups)
#'@param niter Number of iterations
#'@return A matrix of tables indices to leave out for each iteration of the SH
#'@export

Pick_tables_to_split <- function(DESIGN_tables = DESIGN_tables, niter = niter){

  #Remove half of the tables from each Group.
  #How many to remove from each Group?
  how_many_to_remove_from_each_Group <- floor(colSums(DESIGN_tables$mat)/2)
      #use the following to create colors also: floor(colSums(DESIGN_tables$mat)/2)
      #rep(res_HiDiSTATIS$input$DESIGN_tables$colors_D, floor(colSums(res_HiDiSTATIS$input$DESIGN_tables$mat)/2))
  total_tables_to_leave_out <- sum(how_many_to_remove_from_each_Group)

  #Initialize the matrix to store left out table indices
  Leave_out_these_tables <- matrix(NA, total_tables_to_leave_out, niter)

  #Identify the specific stimuli to remove from each category
  for(i in 1:niter){

    leave_out <- vector()
    for(d in 1:DESIGN_tables$D){
      leave_out <- c(leave_out,
                     sort(sample(which(DESIGN_tables$mat[,d]==1), how_many_to_remove_from_each_Group[d]))
      )
    }

    Leave_out_these_tables[,i] <- leave_out

  }

  return(Leave_out_these_tables)

}
