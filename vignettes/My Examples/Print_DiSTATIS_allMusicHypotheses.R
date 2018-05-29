#Print_DiSTATIS_for_all_3_hypotheses.R
#for instructions, see: http://www.sthda.com/english/wiki/create-and-format-powerpoint-documents-from-r-software

rm(list=ls())
library(DiDiSTATIS)
library(PlotDiDiSTATIS)

#Composers
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')
res_DiSTATIS_Composers <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                              DESIGN_rows = DESIGN$rows$Comp,
                                              DESIGN_tables = DESIGN$tables$MusExp)


#Musicians
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_free_balanced_Pianists.R')
res_DiSTATIS_Musicians <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                                   DESIGN_rows = DESIGN$DESIGN_rows,
                                   DESIGN_tables = DESIGN$DESIGN_tables)

#Arousal-Valence
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_balanced_ArousalValence.R')
res_DiSTATIS_AV <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                            DESIGN_rows = DESIGN$DESIGN_rows,
                            DESIGN_tables = DESIGN$DESIGN_tables)


library( ReporteRs )
# Create a PowerPoint document
options("ReporteRs-default-font" = "Times New Roman")
doc = pptx( )

Slide_type <- "Content with Caption"

# Slide 1 : Title slide
#+++++++++++++++++++++++
doc <- addSlide(doc, "Title Slide")
doc <- addTitle(doc, paste0("DiSTATIS Results: \n3 Music Hypotheses"))
doc <- addDate(doc)
doc <- addFooter(doc, "Michael A. Kriegsman")



# Slide 2 : Add plot: Composer
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Composers")
plot_F <- function(){ Plot_DiSTATIS_F(res_DiSTATIS_Composers, dev.new = F) }
doc <- addPlot(doc, plot_F, vector.graphic = TRUE)



# Slide 3 : Add plot: Musician
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Musicians")
plot_F <- function(){ Plot_DiSTATIS_F(res_DiSTATIS_Musicians, dev.new = F) }
doc <- addPlot(doc, plot_F, vector.graphic = TRUE)



# Slide 4 : Add plot: Arousal-Valence
#+++++++++++++++++++++++
doc <- addSlide(doc, Slide_type)
doc <- addTitle(doc, "Arousal-Valence")
plot_F <- function(){ Plot_DiSTATIS_F(res_DiSTATIS_AV, dev.new = F) }
doc <- addPlot(doc, plot_F, vector.graphic = TRUE)



pptFileName <- paste0(Sys.Date(), " DiSTATIS - 3 hypotheses ", round(runif(1),10), ".pptx")
writeDoc(doc, paste0("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/", pptFileName) )
