#'Condence the rows of a matrix according to DESIGN$rows
#'
#'@param X the data matrix
#'@param DESIGN_rows_mat Column vector(s) to discriminate rows (nominal or colors or DESIGN)
#'@return The row-averaged version of X
#'@export

Condense_Rows <- function(X, DESIGN_rows_mat){

  #ensure that DESIGN_rows_rows is a DESIGN_rows matrix
  if(is.vector(DESIGN_rows_mat)){
    DESIGN_rows_mat <- makeNominalData(as.matrix(DESIGN_rows_mat))
  }

  row_averager <- solve(t(DESIGN_rows_mat) %*% DESIGN_rows_mat) %*% t(DESIGN_rows_mat)

  #This is close to Takane's projection equation.
  #Takane's PbX gives A(B) rows.
  #Here, I omit the left-most matrix, to give only B rows.
  X_bary <- row_averager %*% X

  return(X_bary)
}
