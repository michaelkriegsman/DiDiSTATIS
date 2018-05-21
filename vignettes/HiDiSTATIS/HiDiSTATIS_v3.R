#HiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Load: DATA and DESIGN info
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Run HiDiSTATIS!
res_HiDiSTATIS <- HiDiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                             DESIGN_rows = DESIGN$rows$Comp,
                             DESIGN_tables = DESIGN$tables$MusExp, n2k=NULL,
                             Perm_tables = F, Perm_tables_niter = 1000,
                             Boot_tables = T, Boot_tables_niter = 1000,
                             LOO_tables = F,  LOO_tables_multiplier = 10,
                             SH_tables = F,   SH_tables_niter = 50)

## 3. Plot Results
library(PlotDiDiSTATIS)

Plot_HiDiSTATIS_F..(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 2, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 3, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
# for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
#   Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = d, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
# }

Plot_HiDiSTATIS_FiCd(res_HiDiSTATIS, axes = c(1,2), i = 23, d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)




Plot_HiDiSTATIS_Perm_d2(res_HiDiSTATIS)








PlotHiDiSTATIS_Perm_Tables(res_HiDiSTATIS)

PlotHiDiSTATIS_Boot_Tables(res_HiDiSTATIS)

PlotHiDiSTATIS_LOO_Tables(res_HiDiSTATIS)

PlotHiDiSTATIS_SH_Tables(res_HiDiSTATIS)






