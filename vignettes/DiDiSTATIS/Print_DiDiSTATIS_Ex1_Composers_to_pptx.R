#Print_DiDiSTATIS_Ex1_Composers_to_pptx.R
#for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software

Flip_axis1 <- FALSE
Flip_axis2 <- FALSE

library( ReporteRs )
# Create a PowerPoint document
options("ReporteRs-default-font" = "Times New Roman")
doc = pptx( )

#Figures
#for 1 plot:  "Content with Caption"
#for 2 plots: "Two Content"








# Slide 0 : Title slide
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc, paste0("DiDiSTATIS Results: Ex1 Composers \n MFA2 = ", res_DiDiSTATIS$input$MFA2_Flag))
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")



# Add a slide to print the calls that were called... DATA, data_are, MFA1, RV1, MFA2, RV2

# Slide 1 : Add plot: F.B..
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "F_B..")
plot_F.B.. <- function(){ Plot_DiDiSTATIS_F.B..(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_F.B.., vector.graphic = TRUE)
stat <- pot(paste0("r2_Plain_Disc^.D_summed = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_Disc_.D_summed, 2)))
explain <- pot(paste0("The portion of the group-level between-stimulus-variability that projects into the barycentric discriminant sub-space"))
interpretation <- set_of_paragraphs(stat, explain)
doc <- addParagraph(doc, interpretation)



# Slide 2 : Add plot: FabCd
#+++++++++++++++++++++++
doc <- addSlide(doc, "Content with Caption")
ab <- 1
d  <- 1
doc <- addTitle(doc, paste0("FabCd, ab = ", ab, ", d = ", d))
plot_FabCd <- function(){ Plot_DiDiSTATIS_FabCd(res_DiDiSTATIS, ab = ab, d = d, quiet_B.. = TRUE,
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_FabCd, vector.graphic = TRUE)
theSpace <- pot(paste0("everything projected into the space is interpreted like a contrast, relative to the barycenters"))
Hier <- pot(paste0("and everything is hierarchically barycentric. Means of means of means."))
together <- set_of_paragraphs(theSpace, Hier)
doc <- addParagraph(doc, together)




##
##
## Add a slide printing the SS_summary, and the r^2s
##
##




# Slide Break : Grand Results
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc, paste0("Grand Results"))




# Slide 3 : Add plot: FAB.. & Confusion_Grand_Fixed
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "F_Disc^.. & Confusion_Grand_Fixed")
#
plot_FAB.. <- function(){ Plot_DiDiSTATIS_FAB..(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_FAB.., vector.graphic = TRUE)
#
plot_Confusion_Fixed_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows, scale_max_100 = F, dev_new = F) }
doc <- addPlot(doc, plot_Confusion_Fixed_Grand, vector.graphic = TRUE)
doc <- addFooter(doc, paste0("r2_Categories^.. = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Categories.., 2)))



if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
  # Slide 4 : Add plot: Perm_r2_Categories
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "Perm_r2_Categories^..")
  plot_Perm_r2_Categories <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories..(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Categories, vector.graphic = TRUE)
}



if("LOO_Rows" %in% names(res_DiDiSTATIS) & "SH_Rows" %in% names(res_DiDiSTATIS)){
  # Slide 5 : Add plot: LOO_Grand & SH_Grand
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "SH_Grand & LOO_Grand")
  plot_SH_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand, vector.graphic = TRUE)
  plot_LOO_Grand <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)



  # Slide 5.5 : Add plot: SH_Grand
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "SH_Grand_Signed_Ctrb & LOO_Grand_Signed_Ctrb")
  plot_SH_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand_Signed_Ctrb, vector.graphic = TRUE)
  plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)
}



# Slide 5.55 : Add plot: Perfect Confusion Results would look like...
Perfect_Conf_norm <- diag(rep(100, res_DiDiSTATIS$input$DESIGN_rows$B))
dimnames(Perfect_Conf_norm) <- dimnames(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows)
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Perfect Confusion Example")
plot_LOO_Grand <- function(){ PlotConfusion(Perfect_Conf_norm, scale_max_100 = T, dev_new = F) }
doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)
plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(Perfect_Conf_norm, DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)



# Slide Break : Group Results
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc, paste0("Group Results"))




# Slide 6 : Add plot: F.B.D
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "F.B.D")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS, axes = c(1,2),
                                                Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)
doc <- addParagraph(doc, paste0("r2_Group_b = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Groups_b,2)))




if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
  # Slide 7 : Add plot: Perm_r2_Groups
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "Perm_r2_Groups_b")
  plot_Perm_r2_Groups <- function(){ Plot_DiDiSTATIS_Perm_r2_Groups_b(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Groups, vector.graphic = TRUE)
  doc <- addParagraph(doc, paste0("r2_Groups_b = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Groups_b, 2)))
}



for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
  # Slides 8-10 : Add plot: Confusion_Fixed_Group_d & FABd
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, paste0("Group: ", res_DiDiSTATIS$input$DESIGN_tables$labels[d], "\n Confusion_Fixed & F_AB^.d"))
  Plot_FAB.d <- function(){ Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = d, quiet_B.. = T, axes = c(1,2),
                                                  Flip_axis1 = Flip_axis1, Flip_axis2 = Flip_axis2, dev.new = F) }
  doc <- addPlot(doc, Plot_FAB.d, vector.graphic = TRUE)
  plot_Confusion_Fixed_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion.d[,,d], scale_max_100 = F, dev_new = F) }
  doc <- addPlot(doc, plot_Confusion_Fixed_Group_d, vector.graphic = TRUE)
  doc <- addFooter(doc, paste0("SS_B^.d = ",             round(res_DiDiSTATIS$res_BaryGrand$EffectSize$SS_.b.D[d], 2),
                               "\n SS_A(B)^.d = ",       round(res_DiDiSTATIS$res_BaryGrand$EffectSize$SS_aINb.D[d], 2),
                               "\n r2_Categories^.d = ", round(res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Categories.D[d], 2)))

}



if("Perm_Omnibus" %in% names(res_DiDiSTATIS)){
  # Slide 11 : Add plot: Perm_r2_Categories
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "Perm_r2_Categories.D")
  plot_Perm_r2_Categories <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories.D(res_DiDiSTATIS, dev_new = F) }
  doc <- addPlot(doc, plot_Perm_r2_Categories, vector.graphic = TRUE)
}




if("Boot_Tables" %in% names(res_DiDiSTATIS)){
  # Slide 12 : Add plot: Boot_CIs
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Content with Caption")
  doc <- addTitle(doc, "Boot_CIs")
  plot_Boot_CIs <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS, dev.new = F) }
  doc <- addPlot(doc, plot_Boot_CIs, vector.graphic = TRUE)

}






if("LOO_Rows" %in% names(res_DiDiSTATIS) & "SH_Rows" %in% names(res_DiDiSTATIS)){

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Slide 16 : Add plot: LOO_Group_d
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, paste0("SH & LOO Group Confusion \n Group: ", d))
    plot_SH_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Group_d, vector.graphic = TRUE)
    plot_LOO_Group_d <- function(){ PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Group_d, vector.graphic = TRUE)


    # Slide 20 : Add plot: SH_Group_d
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Two Content")
    doc <- addTitle(doc, paste0("SH & LOO Group Signed Ctrb \n Group: ", d))
    plot_SH_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_SH_Group_d_Signed_Ctrb, vector.graphic = TRUE)
    plot_LOO_Group_d_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d], DESIGN_rows = res_DiDiSTATIS$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
    doc <- addPlot(doc, plot_LOO_Group_d_Signed_Ctrb, vector.graphic = TRUE)
  }
}




pptFileName <- paste0(Sys.Date(), " DiDiSTATIS Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/", pptFileName) )


