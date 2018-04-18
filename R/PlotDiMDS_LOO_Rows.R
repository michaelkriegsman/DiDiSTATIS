#'Plot results from LOO on the Rows of DiMDS
#'
#'@param res_DiMDS Output from DiMDS
#'@return LOO results
#'@export


PlotDiMDS_LOO_Rows <- function(res_DiMDS){


  # heatmap(res_DiMDS$LOO_Rows$Prediction_mat_raw, Rowv = NA, Colv = NA)
  #
  # heatmap(res_DiMDS$LOO_Rows$Confusion_rand_norm, Rowv = NA, Colv = NA)
  #
  #

  PlotConfusion(res_DiMDS$LOO_Rows$Confusion_rand_norm)


  ###################### FULL
  prettyPlot(rbind_array_2_matrix(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Full),
             col = rep(res_DiMDS$input$DESIGN_rows$colors_B, ncol(res_DiMDS$LOO_Rows$Leave_out_these_rows)),
             display_names = F, main='1 iteration of LeftOutRows in Original Space',
             constraints = minmaxHelper(mat1 = res_DiMDS$res_Disc_Full$proj2Bary$Fb_condensed, mat2 = rbind_array_2_matrix(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Full)))

  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc,
             display_names = F,
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             dev.new = F, new.plot = F, pch = 24, cex=1.5)


  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fb_condensed, pch=22, cex=3, col = res_DiMDS$input$DESIGN_rows$colors_B, new.plot = F, dev.new = F)

  prettyPlot(apply(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Full, c(1,2), mean), pch=21, cex=3, col = res_DiMDS$input$DESIGN_rows$colors_B, new.plot = F, dev.new = F)




  # ###################### Cond
  # #Plot 1 iteration of LeftOutRows in Original Space
  # prettyPlot(rbind_array_2_matrix(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Cond),
  #            col = rep(res_DiMDS$input$DESIGN_rows$colors_B, ncol(res_DiMDS$LOO_Rows$Leave_out_these_rows)),
  #            display_names = F, main='1 iteration of LeftOutRows in Original Space',
  #            constraints = minmaxHelper(mat1 = res_DiMDS$res_Disc_Cond$proj2Bary$Fb_Cond, mat2 = rbind_array_2_matrix(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Cond)))
  #
  # prettyPlot(res_DiMDS$res_Disc_Cond$proj2Bary$Fdisc,
  #            display_names = F,
  #            col = res_DiMDS$input$DESIGN_rows$colors_AB,
  #            dev.new = F, new.plot = F, pch = 24, cex=1.5)
  #
  #
  # prettyPlot(res_DiMDS$res_Disc_Cond$proj2Bary$Fb_Cond, pch=22, cex=3, col = res_DiMDS$input$DESIGN_rows$colors_B, new.plot = F, dev.new = F)
  #
  # prettyPlot(apply(res_DiMDS$LOO_Rows$Fhatdisc_LeftOut_Rows_Cond, c(1,2), mean), pch=21, cex=3, col = res_DiMDS$input$DESIGN_rows$colors_B, new.plot = F, dev.new = F)


}
