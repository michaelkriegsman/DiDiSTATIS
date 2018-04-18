#' Compute condensed Projector matrix from DESIGN_rows
#'
#' @param DESIGN_rows Design matrix for the rows
#' @return The condensed projector matrix for the barycentric space
#' @export

Bary_Projector_Cond_Sqrt <- function(DESIGN_rows_mat){
  # Pb <- DESIGN_rows %*% solve(t(DESIGN_rows) %*% DESIGN_rows) %*% t(DESIGN_rows)
  # Pb <-                 solve(t(DESIGN_rows) %*% DESIGN_rows) %*% t(DESIGN_rows)
  Pb <-                 sqrt(solve(t(DESIGN_rows_mat) %*% DESIGN_rows_mat)) %*% t(DESIGN_rows_mat)
  return(Pb)
}
