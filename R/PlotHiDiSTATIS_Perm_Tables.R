#'Plot results from test of Null Hypothesis on Tables
#'
#'@param res_HiDiSTATIS Output from HiDiSTATIS with Perm_tables==TRUE
#'@return Histograms of the Dev2's
#'@export


PlotHiDiSTATIS_Perm_Tables <- function(res_HiDiSTATIS){

  #Plot the histogram of the total squared distances from each group to the average,
  #aggregated over all stimuli
  hist(res_HiDiSTATIS$Perm_Tables$Dev2_F.d.perm_2_F.perm, breaks=50, main="HiDiSTATIS: Dev2 F.d to F..")
  abline(v = quantile(res_HiDiSTATIS$Perm_Tables$Dev2_F.d.perm_2_F.perm, .95), col="black", lwd=4)
  abline(v = res_HiDiSTATIS$Perm_Tables$Dev2_F.d_2_F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D, lwd=2)


  #one for each stimulus
  for(ab in 1:nrow(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab)){

    #Plot the histogram of the total squared distances from each group to the average,
    hist(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], breaks=50, main=paste0("HiDiSTATIS: Dev2 Fab.d to Fab.. (",rownames(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab)[ab],")"))
    abline(v = quantile(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], .95), col="black", lwd=4)
    abline(v = res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab[ab,], col=res_HiDiSTATIS$input$DESIGN_tables$colors_D, lwd=2)



    # #Visualize this deviation in the factor space
    # ### First, prepare the "circle" around the special stimulus
    # radius <- sqrt(quantile(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], .95))
    # theta <- seq(0, 2 * pi, length = 200)
    #
    # ### Second, check if the radius needs to be scaled to be above 1
    # if(quantile(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], .95) < 1){}
    # ########################3 Oh wait, this whole idea is broken...
    # ######################## how do I scale Dev2 in a non-arbitrary way?
    #
    #    ### Plot the ab factor scores, with emphasis on the "special" stimulus
    #    XandY <- c(1,2)
    #    cex.special <- matrix(1, res_HiDiSTATIS$input$DESIGN_rows$AB,1)
    #    cex.special[ab] <- 2.5
    #    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,XandY], main = paste0("HiDiSTATIS: Permutation Test (",rownames(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab)[ab],")"),
    #               col = res_HiDiSTATIS$input$DESIGN_rows$colors_AB,
    #               display_names = F, cex=cex.special)
    #    ### Plot the d Group Partial Factor Scores for a given ab stimulus
    #    prettyPlot(t(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY,]),
    #               dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D,
    #               cex = 1.5, pch = 24, display_names = F)
    #    ### Draw the circle around
    #    # draw the circle
    #    lines(x = (radius * cos(theta)) + res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[1]],
    #          y = (radius * sin(theta)) + res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[2]])

  }

}
