#'Plot results from LOO cross-validation of Tables for HiDiSTATIS
#'
#'@param res_HiDiSTATIS Output from HiDiSTATIS with LOO_tables==TRUE
#'@return Output
#'@export


PlotHiDiSTATIS_LOO_Tables <- function(res_HiDiSTATIS){

  PlotConfusion(res_HiDiSTATIS$LOO_Tables$Confusion_rand_norm)
  # corrplot(res_HiDiSTATIS$LOO_Tables$Confusion_rand_norm,
  #          col=viridis(100, direction=1, option="magma"),
  #          is.corr = F)
  #


  #plot individual participants, for a given LOO iteration (i)
  i <- 1
  XandY <- c(1,2)
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F)
  prettyPlot(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[1,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])
  segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[1,i],i], y1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[1,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])
  prettyPlot(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[2,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[2])
  segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[2,i],i], y1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[2,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[2])
  prettyPlot(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[3,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[3])
  segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[3,i],i], y1 = res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$LOO_Tables$Leave_out_these_tables[3,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[3])




  #plot the prediction intervals around each stimulus
  XandY <- c(1,2)
  for(ab in 1:nrow(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables_Cond)){
    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,XandY],
               col = "gray", display_names = F,
               constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables_Cond)),
               main = paste0("HiDiSTATIS LOO: All LeftOut tables for ", rownames(res_HiDiSTATIS$input$DESIGN_rows$mat)[ab]))

    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(t(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,d,]),
                 dev.new = F, new.plot = F,
                 pch=19,
                 col = add.alpha(res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], .3))
      hull.matrix <- t(res_HiDiSTATIS$LOO_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      # pause()
    }

    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),XandY],
               dev.new = F, new.plot = F,
               display_names = F, cex = 2.5)

  }






  #plot the prediction intervals around each category of stimuli
  XandY <- c(1,2)

  for(b in 1:nrow(res_HiDiSTATIS$LOO_Tables$Pb_Fhat_LeftOut_Tables_Cond)){
    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,XandY], col = "gray")
    prettyPlot((res_HiDiSTATIS$input$DESIGN_rows$Pb %*% res_HiDiSTATIS$res_GrandComp$eig$F)[c(b,b),XandY], dev.new = F, new.plot = F)

    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(t(res_HiDiSTATIS$LOO_Tables$Pb_Fhat_LeftOut_Tables_Cond[b,XandY,d,]), dev.new = F, new.plot = F, col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      hull.matrix <- t(res_HiDiSTATIS$LOO_Tables$Pb_Fhat_LeftOut_Tables_Cond[b,XandY,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      # pause()
    }

  }






  #plot the prediction intervals around each category of stimuli
  XandY <- c(1,2)

  prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat),
             constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$LOO_Tables$Pb_Fhat_LeftOut_Tables_Cond[,XandY,,])),
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_B, display_names = F,
             main=paste0("HiDiSTATIS LOO: Prediction intervals for Row Categories"))

  for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,XandY,d], res_HiDiSTATIS$input$DESIGN_rows$mat),
               col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d],
               display_names = F,
               dev.new = F, new.plot = F)

    for(b in 1:res_HiDiSTATIS$input$DESIGN_rows$B){
      hull.matrix <- t(res_HiDiSTATIS$LOO_Tables$Pb_Fhat_LeftOut_Tables_Cond[b,XandY,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }

  }

  prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat),
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_B, pch = 22, cex=2,
             dev.new = F, new.plot = F)


}
