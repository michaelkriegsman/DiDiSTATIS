# Beers

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


Beer_colors_vec <- rep(c("BL", "A", "BR"),3)
Beer_brewery_vec <- rep(c("PELF", "CHTI", "LEFF"), each=3)

DESIGN_rows_Beers_by_Color   <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_colors_vec, colors_B = c("#F9A602", "#E26404", "#703F27"))
DESIGN_rows_Beers_by_Brewery <- Initialize_DESIGNs(DESIGN_rows_mat = Beer_brewery_vec, colors_B = c("#fcd305", "#ed961c", "#ce5050"))
DESIGN_tables_Beer_Sorting   <- Initialize_DESIGNs(DESIGN_tables_mat = Tables_vec)

rownames(DESIGN_rows_Beers_by_Color$DESIGN_rows$mat)   <- rownames(GiantBeerCube)
rownames(DESIGN_rows_Beers_by_Brewery$DESIGN_rows$mat) <- rownames(GiantBeerCube)
rownames(DESIGN_tables_Beer_Sorting$DESIGN_tables$mat) <-  paste0(Tables_vec,".", 1:69)




