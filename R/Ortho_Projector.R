#' Compute the Orthogonal Projector matrix from the Barycentric Projector Matrix
#'
#' @param Pb Barycentric Projector Matrix
#' @return The projector matrix for the orthogonal space
#' @export

Ortho_Projector <- function(Pb){
  Pab <- diag(rep(1,nrow(Pb))) - Pb
  return(Pab)
}
