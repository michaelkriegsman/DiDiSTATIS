#FlipCheck

#' If Component 1 factor Scores are all on the left side, flip them to the right side
#'
#' @param F Factor scores
#' @return Flip-checked factor scores
#' @export

FlipCheck <- function(F){
  F <- as.matrix(F)

  if(round(sum(F[,1]),10)<0){
    F[,1] <- F[,1] * -1
  }

  return(F)
}
