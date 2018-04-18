#
#'Plot a confusion matrix
#'
#'@param Confusion_mat Confusion matrix
#'@return A pretty confusion matrix
#'@export

PlotConfusion <- function(Confusion_mat = NULL){

  if(max(Confusion_mat)<1){
    scale_max <- 1
  }else{
    scale_max <- ceiling(max(Confusion_mat))
  }

  corrplot(Confusion_mat,
           method = "square",
           col=viridis(40, direction=1, option="plasma",
                       begin = 0, end=.94, alpha = 1),
           cl.lim = c(0,scale_max), cl.length=11,
           is.corr = F)
}

