#MFA vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Load:
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Run MFA!
res_DiSTATIS <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                         DESIGN_rows = DESIGN$rows$Comp,
                         DESIGN_tables = DESIGN$tables$MusExp)


library(PlotDiDiSTATIS)

Plot_DiSTATIS_F(res_DiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE)
Plot_DiSTATIS_FiK(res_DiSTATIS, axes = c(1,2), i = c(20, 33), Flip_axis1 = FALSE, Flip_axis2 = FALSE)


#### to print results to a new pptx ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiSTATIS/Print_DiSTATIS_to_pptx.R')

