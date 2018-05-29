# Beers

load("C:/Users/Michael A. Kriegsman/Downloads/sunGlassesBeers.rda")

dim(beersVisionBlind$EV$cubeOfDistance) #17
dim(beersVisionBlind$EBr1$cubeOfDistance) #13
dim(beersVisionBlind$NV$cubeOfDistance) #21
dim(beersVisionBlind$NBr1$cubeOfDistance) #18
Tables_vec <- c(rep("EV", 17), rep("Ebr1", 13), rep("NV", 21), rep("Nbr1", 18))

GiantBeerCube <- array(c(beersVisionBlind$EV$cubeOfDistance,
                         beersVisionBlind$EBr1$cubeOfDistance,
                         beersVisionBlind$NV$cubeOfDistance,
                         beersVisionBlind$NBr1$cubeOfDistance),
                       dim = c(9, 9, 69),
                       dimnames = list(rownames(beersVisionBlind$EV$cubeOfDistance),
                                         colnames(beersVisionBlind$EV$cubeOfDistance),
                                         paste0(Tables_vec,".", 1:69)))

GiantBeerCube[,,18] == beersVisionBlind$EBr1$cubeOfDistance[,,1]
GiantBeerCube[,,31] == beersVisionBlind$NV$cubeOfDistance[,,1]

library(DiDiSTATIS)
library(PlotDiDiSTATIS)
rownames(GiantBeerCube)
Beer_colors_vec <- rep(c("BL", "A", "BR"),3)
Beer_brewery_vec <- rep(c("PELF", "CHIT", "LEFF"),each=3)


BeerDESIGN_Color <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_colors_vec,
                                       colors_B = c("gold", "red", "black"),
                                       DESIGN_tables_mat = Tables_vec, colors_D = NULL)
BeerDESIGN_Brewery <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_brewery_vec,
                                         colors_B = c("purple", "gray", "green"),
                                         DESIGN_tables_mat = Tables_vec, colors_D = NULL)











res_Color <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "d2_array", n2k = 0,
                        DESIGN_rows = BeerDESIGN_Color$DESIGN_rows,
                        DESIGN_tables = BeerDESIGN_Color$DESIGN_tables,
                        MFA1_Flag = T,     RV1_Flag = T,
                        MFA2_Flag = FALSE, RV2_Flag = T,
                        Perm_omni_sort = , Perm_omni_sort_niter = ,
                        Boot_tables = , Boot_tables_niter = ,
                        LOO_rows = , LOO_rows_multiplier = ,
                        SH_rows = , SH_rows_niter = )

res_DiDiSTATIS <- res_Color
# source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_to_pptx.R')



res_Brewery <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "d2_array",
                        DESIGN_rows = BeerDESIGN_Brewery$DESIGN_rows,
                        DESIGN_tables = BeerDESIGN_Brewery$DESIGN_tables,
                        MFA1_Flag = T,     RV1_Flag = T,
                        MFA2_Flag = FALSE, RV2_Flag = T)

res_DiDiSTATIS <- res_Brewery




#Effects _ color

res_Color$res_BaryGrand$EffectSize$r2_Plain_Disc_.. #.3369
res_Color$res_BaryGrand$EffectSize$r2_Plain_Disc_.D
res_Color$res_BaryGrand$EffectSize$r2_Categories.. #.956
res_Color$res_BaryGrand$EffectSize$r2_Categories.D
res_Color$res_BaryGrand$EffectSize$r2_Groups_b #.095

Plot_DiDiSTATIS_F.B..(res_Color)
Plot_DiDiSTATIS_F.B.D(res_Color)
Plot_DiDiSTATIS_FAB..(res_Color)
Plot_DiDiSTATIS_FaB.d(res_Color, priority = "d", connect = 1, quiet_B.. = F)
Plot_DiDiSTATIS_FaB.d(res_Color, priority = "d", connect = 2, quiet_B.. = F)
Plot_DiDiSTATIS_FaB.d(res_Color, priority = "d", connect = 3, quiet_B.. = F)
Plot_DiDiSTATIS_FaB.d(res_Color, priority = "d", connect = 4, quiet_B.. = F)

Effects _ Brewery
res_Brewery$res_BaryGrand$EffectSize$r2_Plain_Disc_.. #.2983
res_Brewery$res_BaryGrand$EffectSize$r2_Plain_Disc_.D
res_Brewery$res_BaryGrand$EffectSize$r2_Categories.. #.9486
res_Brewery$res_BaryGrand$EffectSize$r2_Categories.D
res_Brewery$res_BaryGrand$EffectSize$r2_Groups_b #.0008



#Factor Scores


Plot_DiDiSTATIS_F.B..(res_DiDiSTATIS)
Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS, axes = c(1,2), Flip_axis2 = F)

Plot_DiDiSTATIS_FAB..(res_DiDiSTATIS, axes = c(1,2), Flip_axis2 = F)
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



