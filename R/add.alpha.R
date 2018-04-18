#'Add transparency to a color
#'
#'@param col a vector of colors
#'@param alpha the level of transparency
#'@return the vector of transparent colors
#'@export


add.alpha <- function(col, alpha = 1) {
  apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1],
                                                     x[2], x[3], alpha = alpha))
}
