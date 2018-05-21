#Print_HiDiSTATIS_to_pptx.R
#for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software


library( ReporteRs )
# Create a PowerPoint document
options("ReporteRs-default-font" = "Times New Roman")
doc = pptx( )

#options for regularly producing figures...
Slide_type <- "Content with Caption"


# Slide 1 : Title slide
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc,"HiDiSTATIS Results")
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")


# Slide 2 : Add plot: F
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F.(.)")
plot_F.. <- function(){ Plot_HiDiSTATIS_F..(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
doc <- addPlot(doc, plot_F.., vector.graphic = TRUE)


for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
  # Slide 3 : Add plot: F.k - s003
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, paste0("Plot_HiDiSTATIS_F.d, d = ", d))
  Plot_F.d <- function(){ Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = d, Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
  doc <- addPlot(doc, Plot_F.d, vector.graphic = TRUE)

}


# Slide 4 : Add plot: FiK; i = c(20,33)
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FiCd")
plot_FiCd <- function(){ Plot_HiDiSTATIS_FiCd(res_HiDiSTATIS, axes = c(1,2), i = 23, d=1, Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
doc <- addPlot(doc, plot_FiCd, vector.graphic = TRUE)



# Slide 5 : Add plot: FiK; i = c(20,33)
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Perm")
plot_Perm_r2_Groups <- function(){ Plot_HiDiSTATIS_Perm_r2_Groups(res_HiDiSTATIS, dev_new = F) }
doc <- addPlot(doc, plot_Perm_r2_Groups, vector.graphic = TRUE)



# Slide 6 : Add plot: FiK; i = c(20,33)
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FiCd")
plot_Boot_CIs <- function(){ Plot_HiDiSTATIS_Boot_centered_CIs(res_HiDiSTATIS, axes = c(1,2), i = 13, ellipse = TRUE, percentage = 0.95, dev.new = F) }
doc <- addPlot(doc, plot_Boot_CIs, vector.graphic = TRUE)


# Plot_HiDiSTATIS_Boot_centered_example(res_HiDiSTATIS, i = 5)
Plot_HiDiSTATIS_Boot_centered_CIs(res_HiDiSTATIS, axes = c(1,2), i = 13, ellipse = TRUE, percentage = 0.95, dev.new = TRUE)








pptFileName <- paste0(Sys.Date(), " HiDiSTATIS Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/HiDiSTATIS/", pptFileName) )


