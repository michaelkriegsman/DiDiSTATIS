# Beers

rm(list=ls())

load("C:/Users/Michael A. Kriegsman/Downloads/sunGlassesBeers.rda")

# dim(beersVisionBlind$EV$cubeOfDistance) #17
# dim(beersVisionBlind$EBr1$cubeOfDistance) #13
dim(beersVisionBlind$NV$cubeOfDistance) #21
dim(beersVisionBlind$NBr1$cubeOfDistance) #18
Tables_vec <- c(rep("NV", 21), rep("Nbr1", 18))

GiantBeerCube <- array(c(beersVisionBlind$NV$cubeOfDistance,
                         beersVisionBlind$NBr1$cubeOfDistance),
                       dim = c(9, 9, 39),
                       dimnames = list(rownames(beersVisionBlind$EV$cubeOfDistance),
                                       colnames(beersVisionBlind$EV$cubeOfDistance),
                                       paste0(Tables_vec,".", 1:39)))

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

rownames(BeerDESIGN_Color$DESIGN_tables$mat) <-  paste0(Tables_vec,".", 1:39)
rownames(BeerDESIGN_Brewery$DESIGN_tables$mat) <-  paste0(Tables_vec,".", 1:39)







res_DiDiSTATIS_BeerByColor <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "d2_array", n2k = 0,
                                                      DESIGN_rows = BeerDESIGN_Color$DESIGN_rows,
                                                      DESIGN_tables = BeerDESIGN_Color$DESIGN_tables,
                                                      MFA1_Flag = T,     RV1_Flag = T,
                                                      MFA2_Flag = FALSE, RV2_Flag = T,
                                                      Boot_tables = T, Boot_tables_niter = 1000)

Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS_BeerByColor)
Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS_BeerByColor)







res_DiDiSTATIS_BeerByBrewery <- DiDiSTATIS(DATA = GiantBeerCube, data_are = "d2_array", n2k = 0,
                                         DESIGN_rows = BeerDESIGN_Brewery$DESIGN_rows,
                                         DESIGN_tables = BeerDESIGN_Brewery$DESIGN_tables,
                                         MFA1_Flag = T,     RV1_Flag = T,
                                         MFA2_Flag = FALSE, RV2_Flag = T,
                                         Boot_tables = T, Boot_tables_niter = 1000)

Plot_DiDiSTATIS_F.B.D(res_DiDiSTATIS_BeerByBrewery)
Plot_DiDiSTATIS_Boot_centered_CIs_BD(res_DiDiSTATIS_BeerByBrewery)
