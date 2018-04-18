#DiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)


## 1. Load data (called Sort) and DESIGN (called DESIGN)
#for Free Sort of natural stimuli (Dowling, W.J.)
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/DiDiSTATIS/vignettes/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')


## 2. Analyze
# Preprocessing  (double centering sorting data) is now done within DiDiSTATIS
res_DiDiSTATIS <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$rows$Comp,
                             DESIGN_tables = DESIGN$tables$MusExp,
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = FALSE, RV2_Flag = TRUE, #currently, Bootstrap is breaking on MFA2_Flag==FALSE
                             Perm_omni_sort = F, Perm_omni_sort_niter=100,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = T, LOO_rows_multiplier=1) #,


                             # LOO_rows = F,  LOO_rows_multiplier = 10,

                             # SH_rows = F,   SH_rows_niter = 50,
                             # SH_tables = F,   SH_tables_niter = 50
                             # )




# PlotDiDiSTATIS
PlotDiDiSTATIS(res_DiDiSTATIS)

PlotDiDiSTATIS_Perm_Omni(res_DiDiSTATIS)

PlotDiDiSTATIS_Boot_Tables(res_DiDiSTATIS)

PlotDiDiSTATIS_LOO_Tables(res_DiDiSTATIS)

PlotDiDiSTATIS_SH_Tables(res_DiDiSTATIS)


############ To go into LOO... ###########
res_DiDiSTATIS$LOO_rows <- DiDiSTATIS_LOO_rows(res_DiDiSTATIS)

library(RColorBrewer)
brewer.pal(9, "PuRd")

heatmap(prediction_mat,
        Rowv = NA, Colv=NA, revC = TRUE,
        col = brewer.pal(9, "PuRd"))
#######################################################



