#PlotDiMDS.R

#' Plot results of DiMDS
#'
#' @param res_DiMDS The output of DiMDS
#' @param axes Axes to plot, default = c(1,2)
#' @export

PlotDiMDS <- function(res_DiMDS, axes=c(1,2)){







  PlotConfusion(res_DiMDS$Predict_Fixed_Rows$Confusion_mat)
  PlotConfusion(res_DiMDS$Predict_Fixed_Rows$Prediction_mat)









  #Fb
  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             #dev.new=FALSE, new.plot=TRUE,
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_B,
             pch=22, cex = 2,
             main = paste0('Fb'))









  #Fdisc
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             pch=21,
             display_names = F,
             main = paste0('Fdisc'))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 2, pch=22,
             col = res_DiMDS$input$DESIGN_rows$colors_B)









  #Fdisc: SSdisc
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             pch=21,
             display_names = F,
             main = paste0('Fdisc: SSdisc'))

  # prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
  #            dev.new = F, new.plot = F,
  #            cex = 2, pch=22,
  #            display_names = F,
  #            col = res_DiMDS$input$DESIGN_rows$colors_B)

  segments(x0 = 0,
           y0 = 0,
           x1 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[2]],
           col = res_DiMDS$input$DESIGN_rows$colors_AB)








  #Fdisc: SSb
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             pch=21,
             display_names = F,
             main = paste0('Fdisc: SSb'))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 2, pch=22,
             display_names = F,
             col = res_DiMDS$input$DESIGN_rows$colors_B)

  segments(x0 = 0,
           y0 = 0,
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[2]],
           col = res_DiMDS$input$DESIGN_rows$colors_B)









  #Fdisc: SSab
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             pch=21,
             display_names = F,
             main = paste0('Fdisc: SSab'))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 2, pch=22,
             display_names = F,
             col = res_DiMDS$input$DESIGN_rows$colors_B)

  segments(x0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[1]],
           y0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[2]],
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[2]],
           col = res_DiMDS$input$DESIGN_rows$colors_AB)








  #Fdisc: Summary
  prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes],
             xlab = paste0("Component ", axes[1]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][1],3), "% of Barycentric variance"),
             ylab = paste0("Component ", axes[2]," = ", round(res_DiMDS$res_Disc_Full$eig$t[axes][2],3), "% of Barycentric variance"),
             col = res_DiMDS$input$DESIGN_rows$colors_AB,
             pch=21,
             display_names = F,
             main = paste0('Fdisc: Between and Within'))

  prettyPlot(res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes],
             dev.new = F, new.plot = F,
             cex = 2, pch=22,
             display_names = F,
             col = res_DiMDS$input$DESIGN_rows$colors_B)

  segments(x0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[1]],
           y0 = res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[,axes[2]],
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Full[,axes[2]],
           col = res_DiMDS$input$DESIGN_rows$colors_AB)

  segments(x0 = 0,
           y0 = 0,
           x1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[1]],
           y1 = res_DiMDS$res_Disc_Full$eig$Fb_Cond[,axes[2]],
           col = res_DiMDS$input$DESIGN_rows$colors_B, lwd = 2)

}
