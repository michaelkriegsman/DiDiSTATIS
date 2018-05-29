#Print_DiDiSTATIS_Example2_AV_to_pptx.R
#for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software


library( ReporteRs )
# Create a PowerPoint document
options("ReporteRs-default-font" = "Times New Roman")
doc = pptx( )

#options for regularly producing figures...
Slide_type <- "Content with Caption"

Flip_axis1 <- FALSE
Flip_axis2 <- TRUE



# Slide 1 : Title slide
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc, paste0("DiDiSTATIS Results: Example 2 (AV) \n MFA2 = ", res_DiDiSTATIS$input$MFA2_Flag))
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")



# Add a slide to print the calls that were called... DATA, data_are, MFA1, RV1, MFA2, RV2

# Slide 2 : Add plot: F.B..
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F_B..")
plot_F.B.. <- function(){ Plot_DiDiSTATIS_F.B..(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_F.B.., vector.graphic = TRUE)



# Slide 3 : Add plot: F.B.D
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F.B.D")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)



# Slide 4 : Add plot: FAB..
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FAB..")
plot_FAB.. <- function(){ Plot_DiDiSTATIS_FAB..(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_FAB.., vector.graphic = TRUE)



# Slide 5 : Add plot: FaB.D
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FaB.D")
plot_FaB.D <- function(){ Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "ab", connect = c(8, 14, 31), quiet_B.. = TRUE,
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_FaB.D, vector.graphic = TRUE)



# Slide 5 1/2 : Add plot: FaB.D
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FabCd")
plot_FabCd <- function(){ Plot_DiDiSTATIS_FabCd(res_DiDiSTATIS, ab = 1, d = 1, quiet_B.. = TRUE,
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_FabCd, vector.graphic = TRUE)




##
##
## Add a slide printing the SS_summary, and the r^2s
##
##



# Slide 6 : Add plot: Confusion_Fixed_Grand
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Confusion_Fixed_Grand")
plot_Confusion_Fixed_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows, scale_max_100 = F, dev_new = F) }
doc <- addPlot(doc, plot_Confusion_Fixed_Grand, vector.graphic = TRUE)
plot_Confusion_Fixed_Grand_norm <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows_norm, scale_max_100 = T, dev_new = F) }
doc <- addPlot(doc, plot_Confusion_Fixed_Grand_norm, vector.graphic = TRUE)



for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
  # Slide 7 : Add plot: Confusion_Fixed_Group_d
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, paste0("Confusion_Fixed_Group, d = ", d))
  plot_Confusion_Fixed_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_norm.d[,,d], scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_Confusion_Fixed_Group_d, vector.graphic = TRUE)
  Plot_F.B.d <- function(){ Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = d, quiet_B.. = F, axes = c(1,2),
                                                  Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, Plot_F.B.d, vector.graphic = TRUE)
}


if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
  # Slide 12 : Add plot: Perm_r2_Categories
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Perm_r2_Categories..")
  plot_Perm_r2_Categories <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories..(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Categories, vector.graphic = TRUE)



  # Slide 12 : Add plot: Perm_r2_Categories
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Perm_r2_Categories.D")
  plot_Perm_r2_Categories <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories.D(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Categories, vector.graphic = TRUE)




  # Slide 13 : Add plot: Perm_r2_Groups
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Perm_r2_Groups")
  plot_Perm_r2_Groups <- function(){ Plot_DiDiSTATIS_Perm_r2_Groups_b(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Groups, vector.graphic = TRUE)

}




if("Boot_Tables" %in% names(res_DiDiSTATIS)){
  # Slide 14 : Add plot: Boot_CIs
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Boot_CIs")
  plot_Boot_CIs <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS, dev.new = F) }
  doc <- addPlot(doc, plot_Boot_CIs, vector.graphic = TRUE)

}




if("LOO_Rows" %in% names(res_DiDiSTATIS)){
  # Slide 15 : Add plot: LOO_Grand
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "LOO_Grand")
  plot_LOO_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)
  plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)




  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Slide 16 : Add plot: LOO_Group_d
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, paste0("LOO_Group, d = ", d))
    plot_LOO_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Group_d, vector.graphic = TRUE)
    plot_LOO_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Group_d_Signed_Ctrb, vector.graphic = TRUE)
  }
}



if("SH_Rows" %in% names(res_DiDiSTATIS)){
  # Slide 19 : Add plot: SH_Grand
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "SH_Grand")
  plot_SH_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand, vector.graphic = TRUE)
  plot_SH_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand_Signed_Ctrb, vector.graphic = TRUE)

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Slide 20 : Add plot: SH_Group_d
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, paste0("SH_Group, d = ", d))
    plot_SH_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Group_d, vector.graphic = TRUE)
    plot_SH_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Group_d_Signed_Ctrb, vector.graphic = TRUE)
  }
}





pptFileName <- paste0(Sys.Date(), " DiDiSTATIS Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/", pptFileName) )


