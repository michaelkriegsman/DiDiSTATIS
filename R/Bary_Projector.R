#' Compute Projector matrix from DESIGN_rows
#'
#' @param DESIGN_rows Design matrix for the rows
#' @return The projector matrix for the barycentric space
#' @export

Bary_Projector <- function(DESIGN_rows){
  Pb <- DESIGN_rows %*% solve(t(DESIGN_rows) %*% DESIGN_rows) %*% t(DESIGN_rows)
  return(Pb)
}
