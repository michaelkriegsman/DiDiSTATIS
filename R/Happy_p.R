# Happy_p.R
#
#' Remove the leading zero before the p-value
#'
#' @param Distribution The distribution of values
#' @param Stat The statistic to compare to the distribution
#' @return APA format
#' @export

Happy_p <- function(Distribution, Stat) {

  Larger_than_score <- sum(Distribution > Stat)
  if(Larger_than_score == 0) Larger_than_score <- 1

  p_val <- Larger_than_score / length(Distribution)

  if(length(Distribution) >= 11     &     length(Distribution) <= 100){
    Happy_val <- sub("^(-?)0.", "\\1.", sprintf("%.2f", p_val))
  }
  if(length(Distribution) >= 101     &     length(Distribution) <= 1000){
    Happy_val <- sub("^(-?)0.", "\\1.", sprintf("%.3f", p_val))
  }
  if(length(Distribution) >= 1001     &     length(Distribution) <= 10000){
    Happy_val <- sub("^(-?)0.", "\\1.", sprintf("%.4f", p_val))
  }
  if(length(Distribution) >= 10001     &     length(Distribution) <= 100000){
    Happy_val <- sub("^(-?)0.", "\\1.", sprintf("%.5f", p_val))
  }

  return(Happy_val)

}
