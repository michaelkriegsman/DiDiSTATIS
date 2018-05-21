#HMFA vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Load:
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Run HMFA!
res_HMFA <- HMFA(DATA = Sort_balanced, data_are = 'sort',
                DESIGN_rows = DESIGN$rows$Comp,
                DESIGN_tables = DESIGN$tables$MusExp)


library(PlotDiDiSTATIS)

Plot_HMFA_F..(res_HMFA, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HMFA_F.d(res_HMFA, axes = c(1,2), d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HMFA_F.d(res_HMFA, axes = c(1,2), d = 2, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HMFA_F.d(res_HMFA, axes = c(1,2), d = 3, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
# for(d in 1:res_HMFA$input$DESIGN_tables$D){
#   Plot_HMFA_F.d(res_HMFA, axes = c(1,2), d = d, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
# }

Plot_HMFA_FiCd(res_HMFA, axes = c(1,2), i = 23, d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)

#### to print results to a new pptx ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/HMFA/Print_HMFA_to_pptx.R')
