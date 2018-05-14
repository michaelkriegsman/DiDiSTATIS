#Print_DiMDS_to_pptx.R
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
doc <- addTitle(doc,"DiMDS Results")
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")


# Slide 2 : Add plot: F.b
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "F.b")
plot_F.b <- function(){ Plot_DiMDS_F.b(res_DiMDS) }
doc <- addPlot(doc, plot_F.b, vector.graphic = TRUE)


# Slide 3 : Add plot: Fdisc
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Fdisc")
plot_Fdisc <- function(){ Plot_DiMDS_Fdisc(res_DiMDS) }
doc <- addPlot(doc, plot_Fdisc, vector.graphic = TRUE)


# Slide 4 : Add plot: Blank
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Only")
doc <- addTitle(doc, "Create SSdisc here, from next 3 slides")


# Slide 5 : Add plot: SSdisc-Part 1/3
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "SSdisc-Part 1/3")
plot_SSdisc1 <- function(){ Plot_DiMDS_SSdisc(res_DiMDS) }
doc <- addPlot(doc, plot_SSdisc1, vector.graphic = TRUE)
doc <- addParagraph(doc, "Make all text Times New Roman. Copy and paste to create Fig/SSdisc")


# Slide 6 : Add plot: SSdisc-Part 2/3
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "SSdisc-Part 2/3")
plot_SSdisc2 <- function(){ Plot_DiMDS_SS.b(res_DiMDS) }
doc <- addPlot(doc, plot_SSdisc2, vector.graphic = TRUE)
doc <- addParagraph(doc, "Copy and paste to create Fig/SSdisc")


# Slide 7 : Add plot: SSdisc-Part 3/3
#+++++++++++++++++++++++
doc <- addSlide(doc, "Two Content")
doc <- addTitle(doc, "SSdisc-Part 3/3")
plot_SSdisc3 <- function(){ Plot_DiMDS_SSab(res_DiMDS) }
doc <- addPlot(doc, plot_SSdisc3, vector.graphic = TRUE)
doc <- addParagraph(doc, "Copy and paste to create Fig/SSdisc")


# Slide 8 : Add plot: Confusion Fixed
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Confusion Fixed")
plot_Confusion_Fixed <- function(){ PlotConfusion(res_DiMDS$Predict_Fixed_Rows$Confusion_mat) }
doc <- addPlot(doc, plot_Confusion_Fixed, vector.graphic = TRUE)


if(is.list(res_DiMDS$Perm_Rows)){
  # Slide 9 : Add plot: r2plain.b_perm
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "r2plain.b_perm")
  plot_r2plain.b_perm <- function(){ Plot_DiMDS_Perm_Rows_r2plain.b_perm(res_DiMDS) }
  doc <- addPlot(doc, plot_r2plain.b_perm, vector.graphic = TRUE)


  # Slide 10 : Add plot: r2plain.disc_perm
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "r2plain.disc_perm")
  plot_r2plain.disc_perm <- function(){ Plot_DiMDS_Perm_Rows_r2plain.disc_perm(res_DiMDS) }
  doc <- addPlot(doc, plot_r2plain.disc_perm, vector.graphic = TRUE)


  # Slide 11 : Add plot: r2disc_perm.b_perm
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "r2disc_perm.b_perm")
  plot_r2disc_perm.b_perm <- function(){ Plot_DiMDS_Perm_Rows_r2disc_perm.b_perm(res_DiMDS) }
  doc <- addPlot(doc, plot_r2disc_perm.b_perm, vector.graphic = TRUE)
}


if(is.list(res_DiMDS$LOO_Rows)){
  # Slide 12 : Add plot: Confusion LOO
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Confusion LOO")
  plot_Confusion_LOO <- function(){
    PlotConfusion(Confusion_mat = res_DiMDS$LOO_Rows$Confusion_rand_norm, is.percent = TRUE)
  }
  doc <- addPlot(doc, plot_Confusion_LOO, vector.graphic = TRUE)
}


if(is.list(res_DiMDS$SH_Rows)){
  # Slide 13 : Add plot: Confusion SH
  #+++++++++++++++++++++++
  doc <- addSlide(doc, Slide_type)
  doc <- addTitle(doc, "Confusion SH")
  plot_Confusion_SH <- function(){
    PlotConfusion(Confusion_mat = res_DiMDS$SH_Rows$Confusion_rand_norm, is.percent = TRUE)
    }
  doc <- addPlot(doc, plot_Confusion_SH, vector.graphic = TRUE)
}

pptFileName <- paste0(Sys.Date(), " DiMDS Results ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiMDS/", pptFileName) )


