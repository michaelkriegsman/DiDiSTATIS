#DiMDS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Load:
# the compromise
load('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiMDS/CompromiseForDiMDS.rda')

# and more than the necessary DESIGN info
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')

## 2. Run DiMDS!
res_DiMDS <- DiMDS(DATA = S, data_are = 'CP', DESIGN_rows = DESIGN$rows$Comp, n2k=NULL,
                   Perm_rows = T, Perm_rows_niter = 1000,
                   LOO_rows = T,  LOO_rows_multiplier = 10,
                   SH_rows = T,   SH_rows_niter = 50)


## 3. Plot results
library(PlotDiDiSTATIS)

#### to print results to a new pptx ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiMDS/Print_DiMDS_to_pptx.R')
#To be publish-ready, need to change all font to Times New Roman

#Fixed Effects
Plot_DiMDS_F.b(res_DiMDS)
Plot_DiMDS_Fdisc(res_DiMDS)

Plot_DiMDS_SSdisc(res_DiMDS)
Plot_DiMDS_SS.b(res_DiMDS)
Plot_DiMDS_SSab(res_DiMDS)

PlotConfusion(Confusion_mat = res_DiMDS$Predict_Fixed_Rows$Confusion_mat)

#Permutation
Plot_DiMDS_Perm_Rows_r2plain.b_perm(res_DiMDS)
Plot_DiMDS_Perm_Rows_r2plain.disc_perm(res_DiMDS)
Plot_DiMDS_Perm_Rows_r2disc_perm.b_perm(res_DiMDS)


#Loo
PlotConfusion(Confusion_mat = res_DiMDS$LOO_Rows$Confusion_rand_norm, is.percent = TRUE)


#SH
PlotConfusion(Confusion_mat = res_DiMDS$SH_Rows$Confusion_rand_norm, is.percent = TRUE)
