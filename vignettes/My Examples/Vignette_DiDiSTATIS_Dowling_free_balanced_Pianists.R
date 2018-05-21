# Vignette_DiDiSTATIS_Dowling_free_balanced_Pianists

rm(list=ls())
library(DiDiSTATIS)

## 1. Initialize DATA and DESIGNs
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_free_balanced_Pianists.R')


## 2. Analyze (Preprocessing is done according to what the 'data_are')
res_DiDiSTATIS <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$DESIGN_rows,
                             DESIGN_tables = DESIGN$DESIGN_tables,
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = TRUE, RV2_Flag = TRUE, #currently, Bootstrap is breaking on MFA2_Flag==FALSE
                             Perm_omni_sort = F, Perm_omni_sort_niter=1000,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = F, LOO_rows_multiplier=2,
                             SH_rows = F, SH_rows_niter=100)

#Plot the stuff!!!
