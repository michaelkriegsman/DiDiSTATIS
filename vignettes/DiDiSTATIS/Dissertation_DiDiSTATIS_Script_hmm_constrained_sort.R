# The script to print DiDiSTATIS results as in the Dissertation



rm(list=ls())
library(DiDiSTATIS)
library(PlotDiDiSTATIS)



# Example 1: Composers (Illustrative) ####

## Example 1.1. Load data (called Sort) and DESIGN (called DESIGN)
#     Initialize_Balanced_Dowling_NaturalStimuli_Example.R (a free sort)
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Dowling_Balanced_Constrained_NaturalStimuli_Example.R')

## Example 1.2. Run DiDiSTATIS, MFA2 = TRUE, all inference = TRUE
res_DiDiSTATIS_Composers_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                              DESIGN_rows = DESIGN$rows$Comp,
                                              DESIGN_tables = DESIGN$tables$MusExp,
                                              MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                              MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                              Boot_tables = T,    Boot_tables_niter = 1000,
                                              LOO_rows = T,       LOO_rows_multiplier = 4,
                                              SH_rows = T,        SH_rows_niter = 100)


## Example 1.3. Print results

res_DiDiSTATIS <- res_DiDiSTATIS_Composers_MFA2_F
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_Ex1_Composers_to_pptx.R')







# * Begin post-hoc investigation ####
#Recall results of DiSTATIS, that Composers explained some of Component 2, but not Component 1.
res_DiSTATIS_Composers <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                                   DESIGN_rows = DESIGN$rows$Comp,
                                   DESIGN_tables = DESIGN$tables$MusExp)
Plot_DiSTATIS_F(res_DiSTATIS_Composers)

#Thus, some other effect is primary within these sorting data.
#Let's explore the effect of musician, and the effects of arousal and valence








# *** First, check DiSTATIS ####

# Musicians
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_free_balanced_Pianists.R')
res_DiSTATIS_Musicians <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                                   DESIGN_rows = DESIGN$DESIGN_rows,
                                   DESIGN_tables = DESIGN$DESIGN_tables)
Plot_DiSTATIS_F(res_DiSTATIS_Musicians)
#Appears that Richter is perceived as Beethoven-like, and Barenboim is perceived as Bach-like.
#But doesn't really appear relevant to the effect on Component 1


# Arousal-Valence
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_balanced_ArousalValence.R')
res_DiSTATIS_AV <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                            DESIGN_rows = DESIGN$DESIGN_rows,
                            DESIGN_tables = DESIGN$DESIGN_tables)
Plot_DiSTATIS_F(res_DiSTATIS_AV)
#Oh, baby. Component 1 is Arousal. Component 2 is Valence. And there is most confusion (variability) for aV pieces.











# * The Musician Effect ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_free_balanced_Pianists.R')

res_DiDiSTATIS_Musicians_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                              DESIGN_rows = DESIGN$DESIGN_rows,
                                              DESIGN_tables = DESIGN$DESIGN_tables,
                                              MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                              MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                              Boot_tables = T,    Boot_tables_niter = 1000,
                                              LOO_rows = T,       LOO_rows_multiplier = 4,
                                              SH_rows = T,        SH_rows_niter = 100)

res_DiDiSTATIS <- res_DiDiSTATIS_Musicians_MFA2_F
### Can use the Print_DiDiSTATIS_Ex1_Composers_to_pptx for the musicians also...
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_Ex1_Composers_to_pptx.R')









# * The Arousal-Valence Effect ####
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_balanced_ArousalValence.R')

res_DiDiSTATIS_AV_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                       DESIGN_rows = DESIGN$DESIGN_rows,
                                       DESIGN_tables = DESIGN$DESIGN_tables,
                                       MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                       MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                       Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                       Boot_tables = T,    Boot_tables_niter = 1000,
                                       LOO_rows = T,       LOO_rows_multiplier = 4,
                                       SH_rows = T,        SH_rows_niter = 100)

res_DiDiSTATIS <- res_DiDiSTATIS_AV_MFA2_F
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_Ex1.2_AV_to_pptx.R')

