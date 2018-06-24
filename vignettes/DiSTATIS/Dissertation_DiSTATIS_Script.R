#MFA vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)
library(PlotDiDiSTATIS)
setwd("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiSTATIS/")

## 1a. Composers
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_NaturalStimuli_Example.R')
res_DiSTATIS_Composers <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                                   DESIGN_rows = DESIGN$rows$Comp,
                                   DESIGN_tables = DESIGN$tables$MusExp)
Print_res2ppt_DiSTATIS(res_DiSTATIS = res_DiSTATIS_Composers, Fik = c(20, 33), main = "Composers")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Composers, axes = c(3,4), main = "Composers Comp_3_4")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Composers, axes = c(5,6), main = "Composers Comp_5_6")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Composers, axes = c(7,8), main = "Composers Comp_7_8")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Composers, axes = c(9,10), main = "Composers Comp_9_10")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Composers, axes = c(11,12), main = "Composers Comp_11_12")

Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(13, 14))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(15, 16))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(17, 18))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(19, 20))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(21, 22))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(23, 24))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(25, 26))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(27, 28))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(29, 30))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(31, 32))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(33, 34))
Plot_DiSTATIS_F_BD(res_DiSTATIS_Composers, axes = c(34, 35))
## 1b. Musicians
rm(list=ls())
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_free_balanced_Pianists.R')
res_DiSTATIS_Musicians <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                                   DESIGN_rows = DESIGN$DESIGN_rows,
                                   DESIGN_tables = DESIGN$DESIGN_tables)
Print_res2ppt_DiSTATIS(res_DiSTATIS_Musicians, main = "Musicians")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Musicians, axes = c(3,4), main = "Musicians Comp_3_4")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Musicians, axes = c(5,6), main = "Musicians Comp_5_6")





## 1c. Arousal-Valence
rm(list=ls())
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Balanced_Dowling_balanced_ArousalValence.R')
res_DiSTATIS_AV <- DiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                            DESIGN_rows = DESIGN$DESIGN_rows,
                            DESIGN_tables = DESIGN$DESIGN_tables)
Print_res2ppt_DiSTATIS(res_DiSTATIS_AV, main = "Arousal & Valence")
Print_res2ppt_DiSTATIS(res_DiSTATIS_AV, axes = c(3,4), main = "Arousal & Valence Comp_3_4")






#2a. Beers - Color
rm(list=ls())
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Beer_Sorting.R')
res_DiSTATIS_Beers_by_Color <- DiSTATIS(DATA = GiantBeerCube, data_are = 'd2_array',
                                        DESIGN_rows = DESIGN_rows_Beers_by_Color$DESIGN_rows,
                                        DESIGN_tables = DESIGN_tables_Beer_Sorting$DESIGN_tables)
Print_res2ppt_DiSTATIS(res_DiSTATIS_Beers_by_Color, main = "Beers by Color")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Beers_by_Color, axes = c(3,4), main = "Beers by Color Comp_3_4")


#2b. Beers - Brewery
rm(list=ls())
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/My Examples/Initialize_Beer_Sorting.R')
res_DiSTATIS_Beers_by_Brewery <- DiSTATIS(DATA = GiantBeerCube, data_are = 'd2_array',
                                          DESIGN_rows = DESIGN_rows_Beers_by_Brewery$DESIGN_rows,
                                          DESIGN_tables = DESIGN_tables_Beer_Sorting$DESIGN_tables)
Print_res2ppt_DiSTATIS(res_DiSTATIS_Beers_by_Brewery, main = "Beers by Brewery")
Print_res2ppt_DiSTATIS(res_DiSTATIS_Beers_by_Brewery, axes = c(3,4), main = "Beers by Brewery Comp_3_4")


