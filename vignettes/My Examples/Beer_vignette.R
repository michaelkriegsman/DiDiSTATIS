# Beers

rm(list=ls())
setwd("C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/")
load("C:/Users/Michael A. Kriegsman/Downloads/sunGlassesBeers.rda")

# dim(beersVisionBlind$EV$cubeOfDistance) #17
# dim(beersVisionBlind$EBr1$cubeOfDistance) #13
# dim(beersVisionBlind$NV$cubeOfDistance) #21
# dim(beersVisionBlind$NBr1$cubeOfDistance) #18
Tables_vec <- c(rep("EV", 17), rep("Ebr1", 13), rep("NV", 21), rep("Nbr1", 18))

GiantBeerCube <- array(c(beersVisionBlind$EV$cubeOfDistance,
                         beersVisionBlind$EBr1$cubeOfDistance,
                         beersVisionBlind$NV$cubeOfDistance,
                         beersVisionBlind$NBr1$cubeOfDistance),
                       dim = c(9, 9, 69),
                       dimnames = list(rownames(beersVisionBlind$EV$cubeOfDistance),
                                         colnames(beersVisionBlind$EV$cubeOfDistance),
                                         paste0(Tables_vec,".", 1:69)))

# GiantBeerCube[,,18] == beersVisionBlind$EBr1$cubeOfDistance[,,1]
# GiantBeerCube[,,31] == beersVisionBlind$NV$cubeOfDistance[,,1]

library(DiDiSTATIS)
library(PlotDiDiSTATIS)
# rownames(GiantBeerCube)
Beer_colors_vec <- rep(c("BL", "A", "BR"),3)
Beer_brewery_vec <- rep(c("PELF", "CHTI", "LEFF"), each=3)


BeerDESIGN_Color <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_colors_vec,
                                       # colors_B = c("gold", "red", "black"),
                                       colors_B = c("#F9A602", "#E26404", "#703F27"),
                                       DESIGN_tables_mat = Tables_vec, colors_D = NULL)
BeerDESIGN_Brewery <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_brewery_vec,
                                         # colors_B = c("purple", "gray", "green"),
                                         colors_B = c("#fcd305", "#ed961c", "#ce5050"),

                                         DESIGN_tables_mat = Tables_vec, colors_D = NULL)

rownames(BeerDESIGN_Color$DESIGN_rows$mat) <- rownames(GiantBeerCube)
rownames(BeerDESIGN_Color$DESIGN_tables$mat) <-  paste0(Tables_vec,".", 1:69)
rownames(BeerDESIGN_Brewery$DESIGN_rows$mat) <- rownames(GiantBeerCube)
rownames(BeerDESIGN_Brewery$DESIGN_tables$mat) <-  paste0(Tables_vec,".", 1:69)




#############################################################
res_Color_MFA_F$input$DESIGN_rows$colors_B  <- BeerDESIGN_Color$DESIGN_rows$colors_B
res_Color_MFA_F$input$DESIGN_rows$colors_AB <- BeerDESIGN_Color$DESIGN_rows$colors_AB
#############################################################

#############################################################
res_Brewery_MFA_F$input$DESIGN_rows$colors_B  <- BeerDESIGN_Brewery$DESIGN_rows$colors_B
res_Brewery_MFA_F$input$DESIGN_rows$colors_AB <- BeerDESIGN_Brewery$DESIGN_rows$colors_AB
#############################################################




res_Color_MFA_F <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "sort_dist", n2k = 0,
                        DESIGN_rows = BeerDESIGN_Color$DESIGN_rows,
                        DESIGN_tables = BeerDESIGN_Color$DESIGN_tables,
                        MFA1_Flag = T,     RV1_Flag = T,
                        MFA2_Flag = FALSE, RV2_Flag = T,
                        Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                        Boot_tables = T, Boot_tables_niter = 1000,
                        LOO_rows = T, LOO_rows_multiplier = 10,
                        SH_rows = T, SH_rows_niter = 100)

res_Color_MFA_F$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_Color_MFA_F)
vanilla.table(round(res_Color_MFA_F$TheTable, 2), add.rownames = T)

Print_res2ppt_DiDiSTATIS(res_Color_MFA_F, main = "Beer - Color Hypothesis")







res_Color_MFA_T <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "sort_dist", n2k = 0,
                              DESIGN_rows = BeerDESIGN_Color$DESIGN_rows,
                              DESIGN_tables = BeerDESIGN_Color$DESIGN_tables,
                              MFA1_Flag = T,     RV1_Flag = T,
                              MFA2_Flag = TRUE, RV2_Flag = T,
                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                              Boot_tables = T, Boot_tables_niter = 1000,
                              LOO_rows = T, LOO_rows_multiplier = 10,
                              SH_rows = T, SH_rows_niter = 100)

res_Color_MFA_T$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_Color_MFA_T)
Print_res2ppt_DiDiSTATIS(res_Color_MFA_T, main = "Beer - Color Hypothesis")








res_Brewery_MFA_F <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "sort_dist", n2k = 0,
                              DESIGN_rows = BeerDESIGN_Brewery$DESIGN_rows,
                              DESIGN_tables = BeerDESIGN_Brewery$DESIGN_tables,
                              MFA1_Flag = T,     RV1_Flag = T,
                              MFA2_Flag = FALSE, RV2_Flag = T,
                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                              Boot_tables = T, Boot_tables_niter = 1000,
                              LOO_rows = T, LOO_rows_multiplier = 10,
                              SH_rows = T, SH_rows_niter = 100)

res_Brewery_MFA_F$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_Brewery_MFA_F)
Print_res2ppt_DiDiSTATIS(res_Brewery_MFA_F, main = "Beer - Brewery Hypothesis")

Plot_DiDiSTATIS_Perm_r2_Plain_b_..(res_Brewery_MFA_F)

Plot_DiDiSTATIS_Perm_r2_Categories.D(res_Brewery_MFA_F)






res_Brewery_MFA_T <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "sort_dist", n2k = 0,
                              DESIGN_rows = BeerDESIGN_Brewery$DESIGN_rows,
                              DESIGN_tables = BeerDESIGN_Brewery$DESIGN_tables,
                              MFA1_Flag = T,     RV1_Flag = T,
                              MFA2_Flag = TRUE, RV2_Flag = T,
                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                              Boot_tables = T, Boot_tables_niter = 1000,
                              LOO_rows = T, LOO_rows_multiplier = 10,
                              SH_rows = T, SH_rows_niter = 100)

res_Brewery_MFA_T$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_Brewery_MFA_T)
Print_res2ppt_DiDiSTATIS(res_Brewery_MFA_T, main = "Beer - Brewery Hypothesis")





