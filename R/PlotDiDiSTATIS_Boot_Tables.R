#'Plot results from bootstrap resampling of Tables for DiDiSTATIS
#'
#'@param res_DiDiSTATIS Output from DiDiSTATIS with Boot_tables==TRUE
#'@return Output
#'@export


PlotDiDiSTATIS_Boot_Tables <- function(res_DiDiSTATIS){

  #Plot the bootstrapped category barycenters, from the groups' perspectives
  XandY <- c(1,2)
  prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b..boot_Cond), col=rep(res_DiDiSTATIS$input$DESIGN_rows$colors_B, 100),
             pch=22,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d)),
             main = "Bootstrap F_B_.(.) and F_B_.(D)")
  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d[,,d,]),
               pch = 16, col=add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha = .3),
               new.plot = F, dev.new = F)
    for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
      hull.matrix <- t(res_DiDiSTATIS$Boot_Tables$F.b_boot_d[b,,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }
  }
  # prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b..boot), col=rep(res_DiDiSTATIS$input$DESIGN_rows$colors_B, 100),
  #            main = "Bootstrap F_B_.(.) and F_B_.(D)",
  #            pch=22, dev.new = F, new.plot = F)
  # prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = input$DESIGN_rows$colors_B,
  #            dev.new = F, new.plot = F,
  #            cex = 3)






  #PLot corrected
  prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b..boot_Cond), col=rep(res_DiDiSTATIS$input$DESIGN_rows$colors_B, res_DiDiSTATIS$Boot_Tables$Boot_tables_niter),
             pch=22,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected)),
             main = "Bootstrap F_B_.(.) and F_B_.(D)_corrected")

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,,d,]),
               pch = 16, col=add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha = .3),
               new.plot = F, dev.new = F)
    for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
      hull.matrix <- t(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }
  }

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             dev.new = F, new.plot = F,
             pch = 22, cex = 2.5, display_names = F)



}
