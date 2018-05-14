#Print_HMFA_to_pptx.R
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
doc <- addTitle(doc,"HMFA Results")
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")


# Slide 2 : Add plot: F
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F.(.)")
plot_F.. <- function(){ Plot_HMFA_F..(res_HMFA, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
doc <- addPlot(doc, plot_F.., vector.graphic = TRUE)


for(d in 1:res_HMFA$input$DESIGN_tables$D){
  # Slide 3 : Add plot: F.k - s003
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, paste0("Plot_HMFA_F.d, d = ", d))
  Plot_F.d <- function(){ Plot_HMFA_F.d(res_HMFA, axes = c(1,2), d = d, Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
  doc <- addPlot(doc, Plot_F.d, vector.graphic = TRUE)

}


# Slide 4 : Add plot: FiK; i = c(20,33)
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "FiCd")
plot_FiCd <- function(){ Plot_HMFA_FiCd(res_HMFA, axes = c(1,2), i = 23, d=1, Flip_axis1 = TRUE, Flip_axis2 = FALSE, dev.new = F) }
doc <- addPlot(doc, plot_FiCd, vector.graphic = TRUE)



pptFileName <- paste0(Sys.Date(), " HMFA Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/HMFA/", pptFileName) )


