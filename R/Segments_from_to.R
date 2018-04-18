#Segments_from_to.R
#
#'Shortcut to draw line segments between points
#'
#'@param From the first object
#'@param To the second object #"origin"
#'@param XandY the 2 axes to plot, concatenated
#'@param col line color
#'@param lwd line width
#'@return Draw segments
#'@export


Segments_from_to <- function(From, To, XandY = c(1,2), col="black", lwd=1){

  if(is.character(To)){
    if(To=="origin"){ To <- t(c(0,0))}
  }else{To <- as.matrix(To)}

  segments(x0 = c(From[,XandY[1]]), y0 = c(From[,XandY[2]]), x1 = c(To[,XandY[1]]), y1 = c(To[,XandY[2]]), col = col, lwd = lwd)

}
