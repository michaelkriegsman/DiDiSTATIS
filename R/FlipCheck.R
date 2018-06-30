#FlipCheck

#' If Component 1 factor Scores are all on the left side, flip them to the right side
#'
#' @param F Factor scores
#' @return Flip-checked factor scores
#' @export

FlipCheck <- function(F){
  if(round(sum(as.matrix(F)[,1]),10)<0){
    as.matrix(F)[,1] <- as.matrix(F)[,1] * -1
  }

  return(F)
}
