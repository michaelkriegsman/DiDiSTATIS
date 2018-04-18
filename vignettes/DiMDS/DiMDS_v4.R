#DiMDS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

#Load the compromise
load('C:/Users/Michael A. Kriegsman/Google Drive/Dissertation/DiDiSTATIS/DiDiSTATIS/vignettes/DiMDS/CompromiseForDiMDS.rda')

#Initialize some stuff (remove other stuff)
source('C:/Users/Michael A. Kriegsman/Google Drive/Dissertation/DiDiSTATIS/DiDiSTATIS/vignettes/Initialize_Dowling_NaturalStimuli_Example.R')

#Do it!
res_DiMDS <- DiMDS(DATA = S, data_are = 'CP', DESIGN_rows = DESIGN$rows$Comp, n2k=NULL,
                   Perm_rows = T, Perm_rows_niter = 1000,
                   Boot_rows = F, Boot_rows_niter = 1000,
                   LOO_rows = F,  LOO_rows_multiplier = 10,
                   SH_rows = F,   SH_rows_niter = 50)

PlotDiMDS(res_DiMDS)

#build in checks to see if the sub-list exists for that type of inference
PlotDiMDS_Perm_Rows(res_DiMDS)

PlotDiMDS_Boot_Rows(res_DiMDS)

PlotDiMDS_LOO_Rows(res_DiMDS)
#make prettier heatmaps
#break these figures

PlotDiMDS_SH_Rows(res_DiMDS)








