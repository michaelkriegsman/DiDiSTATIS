#Print_DiSTATIS_to_pptx.R
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
doc <- addTitle(doc,"DiSTATIS Results")
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")


# Slide 2 : Add plot: F
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F")
plot_F <- function(){ Plot_DiSTATIS_F(res_DiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = FALSE) }
doc <- addPlot(doc, plot_F, vector.graphic = TRUE)


# Slide 3 : Add plot: FiK; i = c(20,33)
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FiK: i = c(20, 33)")
plot_FiK <- function(){ Plot_DiSTATIS_FiK(res_DiSTATIS, axes = c(1,2), i = c(20, 33), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = FALSE) }
doc <- addPlot(doc, plot_FiK, vector.graphic = TRUE)



pptFileName <- paste0(Sys.Date(), " DiSTATIS Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiSTATIS/", pptFileName) )


