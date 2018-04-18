#DiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)

## 1. Initialize
# SortFree_36 <- read.csv('C:/Users/Michael A. Kriegsman/Google Drive/Projects/DiDiSTATIS_Composers/WithMoreSubj_ForDissertation/natural36_free_forR.csv', row.names=1)


## 1. Load data (called Sort) and DESIGN (called DESIGN)
#for Free Sort of natural stimuli (Dowling, W.J.)
# DESIGNs <- Initialize_DESIGNs(DESIGN_rows_mat = , DESIGN_tables_mat = )
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')


## 2. Analyze
# Preprocessing  (double centering sorting data) is now done within DiDiSTATIS
res_DiDiSTATIS <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$rows$Comp, #DESIGNS$DESIGN_rows
                             DESIGN_tables = DESIGN$tables$MusExp, #DESIGNS$DESIGN_tables
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = FALSE, RV2_Flag = TRUE, #currently, Bootstrap is breaking on MFA2_Flag==FALSE
                             Perm_omni_sort = F, Perm_omni_sort_niter=100,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = F, LOO_rows_multiplier=2,
                             SH_rows = F, SH_rows_niter=100) #,

res_DiDiSTATIS$input$DESIGN_rows$colors_B   <- c('#3c81c1', '#7d4dbc', '#52af6f') #possible 4th color... brown #9d6725 or #714107
for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
  res_DiDiSTATIS$input$DESIGN_rows$colors_AB[which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)] <- res_DiDiSTATIS$input$DESIGN_rows$colors_B[b]
}



res_DiDiSTATIS$input$DESIGN_tables$colors_D <- c('#fcd305','#ed961c','#ce5050')
#e5ce37 #old yellow
#e58537 #old orange
for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
  res_DiDiSTATIS$input$DESIGN_tables$colors_CD[which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)] <- res_DiDiSTATIS$input$DESIGN_tables$colors_D[d]
}




# PlotDiDiSTATIS
PlotDiDiSTATIS(res_DiDiSTATIS)

PlotDiDiSTATIS_Perm_Omni(res_DiDiSTATIS)

PlotDiDiSTATIS_Boot_Tables(res_DiDiSTATIS)

# PlotDiDiSTATIS_LOO_Rows(res_DiDiSTATIS)
PlotConfusion(res_DiDiSTATIS$LOO_Rows$Grand$Confusion_rand_norm)
for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
  PlotConfusion(res_DiDiSTATIS$LOO_Rows$Group$Confusion_rand_D_norm[,,d])
}

# PlotDiDiSTATIS_SH_Rows(res_DiDiSTATIS)
PlotConfusion(res_DiDiSTATIS$SH_Rows$Grand$Confusion_rand_norm)
for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
  PlotConfusion(res_DiDiSTATIS$SH_Rows$Group$Confusion_rand_D_norm[,,d])
}

