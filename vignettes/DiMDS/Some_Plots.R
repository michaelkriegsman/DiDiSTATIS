
i <- 1
###
yup_these <- Leave_out_these_rows[,i]
res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[yup_these,]



#Highlight the left out rows in their original space
prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc,
           display_names = F,
           col = res_DiMDS$input$DESIGN_rows$colors_AB,
           main="The original barycentric space")

prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc[yup_these,],
           col = res_DiMDS$input$DESIGN_rows$colors_B,
           cex = 3, text.cex = 2,
           dev.new = F, new.plot = F)



#Leave out those rows, compute the solution, and project them in.
prettyPlot(res_LeftIn_Rows_Disc_Full$proj2Bary$Fdisc,
           display_names = F,
           col = DESIGN_Rows_LeftIn$colors_AB_LeftIn,
           main="The LeftIn barycentric space")

prettyPlot(Fdisc_LeftOut_Rows[,,i],
           col = res_DiMDS$input$DESIGN_rows$colors_B,
           cex = 3, text.cex = 2,
           dev.new = F, new.plot = F)


#Then project those left out rows into the original space (and do 1 more LOO iteration)
prettyPlot(res_DiMDS$res_Disc_Full$proj2Bary$Fdisc,
           display_names = F,
           col = res_DiMDS$input$DESIGN_rows$colors_AB,
           main="The original barycentric space")

prettyPlot(Fhat_disc_1,
           col = res_DiMDS$input$DESIGN_rows$colors_B[1],
           cex = 3, text.cex = 2,
           dev.new = F, new.plot = F)
