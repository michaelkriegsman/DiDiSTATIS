#MDS_v2.R

rm(list=ls())
library(DiDiSTATIS)

#For this example, I adapt the data from the chapter on DiSTATIS.
#Although these data are an array of distance matrices, DiSTATIS collapses
#the tables into a single representative table, called the compromise.
#This compromise is a 36 by 36 (psd) matrix of the perceived similarity
#between 36 pieces of classical music.

#Load the compromise from DiSTATIS
load('C:/Users/Michael A. Kriegsman/Google Drive/Dissertation/DiDiSTATIS/DiDiSTATIS/vignettes/DiMDS/CompromiseForDiMDS.rda')

#And load more than the necessary DESIGN info
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

#And run MDS!
res_MDS <- MDS(DATA = S, data_are = 'CP', DESIGN_rows = DESIGN$rows$Comp)



# Plot Factor Scores
axes <- c(1,2)
prettyPlot(res_MDS$eig$F_Full,
           col = res_MDS$input$DESIGN_rows$colors_AB,
           cex = 1.75, pch=15,
           display_names = F,
           main = "MDS Factor Scores",
           xlab = paste0("Component ", axes[1]," variance = ", round(res_MDS$eig$t[axes][1],1), "%"),
           ylab = paste0("Component ", axes[2]," variance = ", round(res_MDS$eig$t[axes][2],1), "%"))
prettyPlot(res_MDS$eig$F_Cond,
           new.plot = F, dev.new = F, text.cex = 1.25,
           col = res_MDS$input$DESIGN_rows$colors_B,
           display_points = F)
#Then put it into powerpoint, right click, edit, Times New Roman, and jog Composers back to correct position.
