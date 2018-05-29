#DiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Initialize
# SortFree_36 <- read.csv('C:/Users/Michael A. Kriegsman/Google Drive/Projects/DiDiSTATIS_Composers/WithMoreSubj_ForDissertation/natural36_free_forR.csv', row.names=1)


## 1. Load data (called Sort) and DESIGN (called DESIGN)
#for Free Sort of natural stimuli (Dowling, W.J.)
# DESIGNs <- Initialize_DESIGNs(DESIGN_rows_mat = , DESIGN_tables_mat = )
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')


## 2. Analyze (Preprocessing is done according to what the 'data_are')
res_DiDiSTATIS <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$rows$Comp,
                             DESIGN_tables = DESIGN$tables$MusExp,
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = F, RV2_Flag = TRUE,
                             Perm_omni_sort = F, Perm_omni_sort_niter=1000,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = F, LOO_rows_multiplier=2,
                             SH_rows = F, SH_rows_niter=100)


num_Categories <- apply(Sort_balanced, 2, max)
num_Cats_D <- res_DiDiSTATIS$input$DESIGN_tables$Pd_Cond %*% num_Categories


library(PlotDiDiSTATIS)

#### To print the various results ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_to_pptx.R')

#Factor Scores
Plot_DiDiSTATIS_F.B..(res_DiDiSTATIS)
Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS, axes = c(1,2), Flip_axis2 = F)

Plot_DiDiSTATIS_FAB..(res_DiDiSTATIS, axes = c(3,2), Flip_axis2 = TRUE)
Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "ab", connect = c(8, 14, 31), quiet_B.. = TRUE)
Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, axes = c(3,2), priority = "d", connect = 3, quiet_B.. = TRUE, Flip_axis2 = TRUE)

Plot_DiDiSTATIS_FabCd(res_DiDiSTATIS, ab = 1, d = 1)

#Effect Sizes
res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Categories
res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Groups
res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_BD_ABCD
res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_Disc_..
#Not sure how really to interpret these guys...
# res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_Disc_.d
# res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_Disc_cd

#Quality of Prediction
PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion_Rows)

PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion.d[,,1])
Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = 1, quiet_B.. = F)

PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion.d[,,2])
Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = 2, quiet_B.. = F)

PlotConfusion(res_DiDiSTATIS$res_BaryGrand$Prediction_Fixed_Rows$Confusion.d[,,3])
Plot_DiDiSTATIS_FaB.d(res_DiDiSTATIS, priority = "d", connect = 3, quiet_B.. = F)


#Permutation
Plot_DiDiSTATIS_Perm_r2_Categories..(res_DiDiSTATIS, dev_new = T)
Plot_DiDiSTATIS_Perm_r2_Categories.D(res_DiDiSTATIS)
Plot_DiDiSTATIS_Perm_r2_Groups_b(res_DiDiSTATIS)
Plot_DiDiSTATIS_Perm_r2_Groups_Disc(res_DiDiSTATIS)

Plot_DiDiSTATIS_Perm_r2_Plain_Disc_.d(res_DiDiSTATIS)


#Bootstrap
Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS)


#LOO
PlotConfusion(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm, is.percent = T)

PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,1], is.percent = T)
PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,2], is.percent = T)
PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,3], is.percent = T)


#SH
PlotConfusion(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm, is.percent = T)

PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,1], is.percent = T)
PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,2], is.percent = T)
PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,3], is.percent = T)
