#HiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

#Load the (sorting) data
SortFree_36 <- read.csv('C:/Users/Michael A. Kriegsman/Google Drive/Projects/DiDiSTATIS_Composers/WithMoreSubj_ForDissertation/natural36_free_forR.csv', row.names=1)
Sort <- SortFree_36[-37,]

#Initialize DESIGN
source('C:/Users/Michael A. Kriegsman/Google Drive/Dissertation/DiDiSTATIS/DiDiSTATIS/vignettes/Initialize_Dowling_NaturalStimuli_Example.R')

#Do it!
res_HiDiSTATIS <- HiDiSTATIS(DATA = Sort, data_are = 'sort',
                             DESIGN_rows = DESIGN$rows$Comp,
                             DESIGN_tables = DESIGN$tables$MusExp, n2k=NULL,
                             Perm_tables = F, Perm_tables_niter = 1000,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_tables = T,  LOO_tables_multiplier = 10,
                             SH_tables = F,   SH_tables_niter = 50)


PlotHiDiSTATIS_Perm_Tables(res_HiDiSTATIS)

############################ For now, drop visualization... I'm stuck on d^2 < 1 ################
# Use Avg_Dev2_F.d.perm_2_F.perm to plot the alpha around each stimulus
# What to do about dev2 < 1 (they inflate when sqrt).
#################################################################################################

PlotHiDiSTATIS_Boot_Tables(res_HiDiSTATIS)

PlotHiDiSTATIS_LOO_Tables(res_HiDiSTATIS)

PlotHiDiSTATIS_SH_Tables(res_HiDiSTATIS)






