#'Plot results from Bootstrap on the Rows of DiMDS
#'
#'@param res_DiMDS Output from DiMDS
#'@return 95% Bootstrap confidence intervals
#'@export


PlotDiMDS_Boot_Rows <- function(res_DiMDS){

  XandY <- c(1,2)
  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond,
             display_points=TRUE,
             display_names=TRUE,
             main="95% Bootstrap Confidence Intervals",
             x_axis=XandY[1], y_axis=XandY[2],
             xlab=paste('Component ',XandY[1], sep=""),
             ylab=paste('Component ',XandY[2], sep=""),
             col=res_DiMDS$input$DESIGN_rows$colors_B,
             constraints=minmaxHelper(rbind_array_2_matrix(res_DiMDS$Boot_Rows$Fb.boot)),
             pch=22, cex=2.75)
  for(b in 1:nrow(res_DiMDS$Boot_Rows$Fb.boot)){
    hull.matrix <- t(res_DiMDS$Boot_Rows$Fb.boot[b,,])
    peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
    peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_DiMDS$input$DESIGN_rows$colors_B[b])
  }

  # prettyPlot(rbind_array_2_matrix(res_DiMDS$Boot_Rows$Fb.boot),
  #            dev.new = F, new.plot = F)


}
