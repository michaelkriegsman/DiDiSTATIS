#Vignette_DiDiSTATIS_Composers.R
#DiDiSTATIS vignette on Dowling's (balanced) Composers

rm(list=ls())
library(DiDiSTATIS)

## 1. Load data (called Sort) and DESIGN (called DESIGN)
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Analyze (Preprocessing is done according to what the 'data_are')
res_DiDiSTATIS <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$rows$Comp, #DESIGNS$DESIGN_rows
                             DESIGN_tables = DESIGN$tables$MusExp, #DESIGNS$DESIGN_tables
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = FALSE, RV2_Flag = TRUE, #currently, Bootstrap is breaking on MFA2_Flag==FALSE
                             Perm_omni_sort = F, Perm_omni_sort_niter=100,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = F, LOO_rows_multiplier=2,
                             SH_rows = F, SH_rows_niter=100)

## 3. Plot results
library(PlotDiDiSTATIS)

Plot_DiDiSTATIS_F.b..(res_DiDiSTATIS)
Plot_DiDiSTATIS_F.b.d(res_DiDiSTATIS)
Plot_DiDiSTATIS_Fab.d(res_DiDiSTATIS, priority = "ab", connect = 0)
Plot_DiDiSTATIS_Fab.d(res_DiDiSTATIS, priority = "ab", connect = c(3, 16, 30))
Plot_DiDiSTATIS_Fab.d(res_DiDiSTATIS, priority = "d", connect = 1)
Plot_DiDiSTATIS_Fab..(res_DiDiSTATIS)
