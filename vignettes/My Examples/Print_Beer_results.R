#Print_Beer_results.R
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
doc <- addTitle(doc, paste0("DiDiSTATIS Results: Beers"))
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")



# Add a slide to print the calls that were called... DATA, data_are, MFA1, RV1, MFA2, RV2

# Slide 1 : Colors MFA_F
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Colors MFA_F")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_Color_MFA_F, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)
plot_Boot_F.B.D <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_Color_MFA_F, dev.new = F) }
doc <- addPlot(doc, plot_Boot_F.B.D, vector.graphic = TRUE)


# Slide 1.5 : Colors MFA_F
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Colors MFA_F")
plot_r2_Cat.. <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories..(res_Color_MFA_F, dev_new = F) }
doc <- addPlot(doc, plot_r2_Cat.., vector.graphic = TRUE)
plot_r2_Cat.D <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories.D(res_Color_MFA_F, dev_new = F) }
doc <- addPlot(doc, plot_r2_Cat.D, vector.graphic = TRUE)

if("LOO_Rows" %in% names(res_Color_MFA_F) & "SH_Rows" %in% names(res_Color_MFA_F)){
  # Slide 5 : Add plot: SH_Grand & LOO_Grand
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "SH_Grand & LOO_Grand Colors_MFA_F")
  plot_SH_Grand <- function(){ PlotConfusion(res_Color_MFA_F$SH_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand, vector.graphic = TRUE)
  plot_LOO_Grand <- function(){ PlotConfusion(res_Color_MFA_F$LOO_Rows$Grand$Confusion_rand_norm, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)



  # Slide 5.5 : Add plot: Signed Ctrbs
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Two Content")
  doc <- addTitle(doc, "SH_Grand_Signed_Ctrb & LOO_Grand_Signed_Ctrb Colors_MFA_F")
  plot_SH_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(Colors_MFA_F$SH_Rows$Grand$Confusion_rand_norm, DESIGN_rows = Colors_MFA_F$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_SH_Grand_Signed_Ctrb, vector.graphic = TRUE)
  plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(Colors_MFA_F$LOO_Rows$Grand$Confusion_rand_norm, DESIGN_rows = Colors_MFA_F$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
  doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)
}



# Slide 5.55 : Add plot: Perfect Confusion Results would look like...
Perfect_Conf_norm <- diag(rep(100, Colors_MFA_F$input$DESIGN_rows$B))
dimnames(Perfect_Conf_norm) <- dimnames(Colors_MFA_F$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows)
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Perfect Confusion Example")
plot_LOO_Grand <- function(){ PlotConfusion(Perfect_Conf_norm, scale_max_100 = T, dev_new = F) }
doc <- addPlot(doc, plot_LOO_Grand, vector.graphic = TRUE)
plot_LOO_Grand_Signed_Ctrb <- function(){ PlotConfusion_norm_SignedCtrb(Perfect_Conf_norm, DESIGN_rows = Colors_MFA_F$input$DESIGN_rows, scale_max_100 = T, dev_new = F) }
doc <- addPlot(doc, plot_LOO_Grand_Signed_Ctrb, vector.graphic = TRUE)









# Slide 2 : Colors MFA_T
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Colors MFA_T")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_Color_MFA_T, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)
plot_Boot_F.B.D <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_Color_MFA_T, dev.new = F) }
doc <- addPlot(doc, plot_Boot_F.B.D, vector.graphic = TRUE)



# Slide 3 : Brewery MFA_F
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Brewery MFA_F")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_Brewery_MFA_F, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)
plot_Boot_F.B.D <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_Brewery_MFA_F, dev.new = F) }
doc <- addPlot(doc, plot_Boot_F.B.D, vector.graphic = TRUE)

# Slide 3.5 : Brwery MFA_F
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Brewery MFA_F")
plot_r2_Cat.. <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories..(res_Brewery_MFA_F, dev_new = F) }
doc <- addPlot(doc, plot_r2_Cat.., vector.graphic = TRUE)
plot_r2_Cat.D <- function(){ Plot_DiDiSTATIS_Perm_r2_Categories.D(res_Brewery_MFA_F, dev_new = F) }
doc <- addPlot(doc, plot_r2_Cat.D, vector.graphic = TRUE)


# Slide 4 : Brewery MFA_T
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "Brewery MFA_T")
plot_F.B.D <- function(){ Plot_DiDiSTATIS_F.B.D(res_Brewery_MFA_T, dev.new = F) }
doc <- addPlot(doc, plot_F.B.D, vector.graphic = TRUE)
plot_Boot_F.B.D <- function(){ Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_Brewery_MFA_T, dev.new = F) }
doc <- addPlot(doc, plot_Boot_F.B.D, vector.graphic = TRUE)



pptFileName <- paste0(Sys.Date(), "DiDiSTATIS Beer Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/", pptFileName) )


