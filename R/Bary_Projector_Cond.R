#' Compute condenses Projector matrix from DESIGN_rows
#'
#' @param DESIGN_rows Design matrix for the rows
#' @return The condensed projector matrix for the barycentric space
#' @export

Bary_Projector_Cond <- function(DESIGN_rows){
  # Pb <- DESIGN_rows %*% solve(t(DESIGN_rows) %*% DESIGN_rows) %*% t(DESIGN_rows)
    Pb <-                 solve(t(DESIGN_rows) %*% DESIGN_rows) %*% t(DESIGN_rows)
  return(Pb)
}