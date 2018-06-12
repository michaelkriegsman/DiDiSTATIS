#HiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Load: DATA and DESIGN info
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Run HiDiSTATIS!
res_HiDiSTATIS <- HiDiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                             DESIGN_rows = DESIGN$rows$Comp,
                             DESIGN_tables = DESIGN$tables$MusExp, n2k=NULL,
                             Perm_tables = T, Perm_tables_niter = 1000,
                             Boot_tables = F, Boot_tables_niter = 1000)

  # cbind(res_HiDiSTATIS$input$DESIGN_tables$vec,
#       res_HiDiSTATIS$res_GrandComp$ProjCP$SScd,
#       res_HiDiSTATIS$Hierarchy_of_tables$coef$alpha1 * res_HiDiSTATIS$input$DESIGN_tables$CD)


## 3. Plot Results
library(PlotDiDiSTATIS)

Plot_HiDiSTATIS_F..(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 2, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 3, Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HiDiSTATIS_FiCd(res_HiDiSTATIS, axes = c(1,2), i = 23, d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)



Plot_HiDiSTATIS_Perm_r2_Groups(res_HiDiSTATIS)

# Plot_HiDiSTATIS_Boot_centered_example(res_HiDiSTATIS, i = 5)
Plot_HiDiSTATIS_Boot_centered_CIs(res_HiDiSTATIS, axes = c(1,2), i = 13, ellipse = TRUE, percentage = 0.95)


source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/HiDiSTATIS/Print_HiDiSTATIS_to_pptx.R')










