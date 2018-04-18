#Playing with SortingWines


rm(list=ls())
library(DiDiSTATIS)


###Initialize the data sets
load('vignettes/sortingWines.rda')

#ternarySortNovices
# sortingWines$ternarySortNovices
colnames(sortingWines$ternarySortNovices) <- paste0('Exp', 1:ncol(sortingWines$ternarySortNovices),'_Tern')

#ternarySortExperts
# sortingWines$ternarySortExperts
colnames(sortingWines$ternarySortExperts) <- paste0('Exp', 1:ncol(sortingWines$ternarySortExperts),'_Tern')

#freeSortExperts
# sortingWines$freeSortExperts
# apply(sortingWines$freeSortExperts, 2, max) #check how many categories each Expert created
colnames(sortingWines$freeSortExperts) <- paste0('Exp', 1:ncol(sortingWines$freeSortExperts),'_Free')


#bind the 3 groups, Novice, Expert3, ExpertFree
Wines <- cbind(sortingWines$ternarySortNovices, sortingWines$ternarySortExperts, sortingWines$freeSortExperts)





#Create DESIGN_rows_mat
DESIGN_rows_mat <- makeNominalData(as.matrix(sortingWines$winesDescription$Type))
colnames(DESIGN_rows_mat) <- unique(sortingWines$winesDescription$Type)
rownames(DESIGN_rows_mat) <- sortingWines$winesDescription$WinesLong

#Create DESIGN_tables_mat
DESIGN_tables_mat <- c(rep('Nov_Tern', ncol(sortingWines$ternarySortNovices)),
                       rep('Exp_Tern', ncol(sortingWines$ternarySortExperts)),
                       rep('Exp_Free', ncol(sortingWines$freeSortExperts)))


#Create all the needed DESIGN info
DESIGN <- Initialize_DESIGNs(DESIGN_rows_mat = DESIGN_rows_mat,
                             DESIGN_tables_mat = DESIGN_tables_mat)



###And, if wanted, change the colors...

#Set the B category colors
DESIGN$DESIGN_rows$labels
# DESIGN$DESIGN_rows$colors_B <- c('#3c81c1', '#7d4dbc', '#52af6f')
DESIGN$DESIGN_rows$colors_B <- unique(sortingWines$winesDescription$ColorCode)
#Set the AB stimulus colors
for(b in 1:DESIGN$DESIGN_rows$B){
  DESIGN$DESIGN_rows$colors_AB[which(DESIGN$DESIGN_rows$mat[,b]==1)] <- DESIGN$DESIGN_rows$colors_B[b]
}

#Set the D group colors
DESIGN$DESIGN_tables$colors_D <- c('#fcd305','#ed961c','#ce5050')
#Set the CD participant colors
for(d in 1:DESIGN$DESIGN_tables$D){
  DESIGN$DESIGN_tables$colors_CD[which(DESIGN$DESIGN_tables$mat[,d]==1)] <- DESIGN$DESIGN_tables$colors_D[d]
}




#And crunch the analysis
res_DiDiSTATIS <- DiDiSTATIS(DATA = Wines, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$DESIGN_rows,
                             DESIGN_tables = DESIGN$DESIGN_tables,
                             MFA1_Flag = TRUE, RV1_Flag = TRUE,
                             MFA2_Flag = FALSE, RV2_Flag = TRUE, #currently, Bootstrap is breaking on MFA2_Flag==FALSE
                             Perm_omni_sort = F, Perm_omni_sort_niter=100,
                             Boot_tables = F, Boot_tables_niter = 1000,
                             LOO_rows = F, LOO_rows_multiplier=2,
                             SH_rows = F, SH_rows_niter=100) #,

res_HiDiSTATIS <- HiDiSTATIS(DATA = Wines, data_are = 'sort', n2k = NULL,
                             DESIGN_rows = DESIGN$DESIGN_rows,
                             DESIGN_tables = DESIGN$DESIGN_tables)

