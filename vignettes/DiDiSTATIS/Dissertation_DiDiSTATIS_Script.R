# The script/vignette to print DiDiSTATIS results as in the Dissertation

rm(list=ls())
library(DiDiSTATIS)
library(PlotDiDiSTATIS)

##setwd()
setwd('C://Users/Michael A. Kriegsman/Box Sync/Dissertation/LaTeX/Figures/DiDiSTATIS/')
####

# Example 1: Music Free Sort ####
MusicFreeSort

#The first 36 rows correspond to the 36 stimuli
Sort <- MusicFreeSort[-37,]

#To balance the 3 groups, I randomly selected 7 participants to remove from the Mid Musical Experience group, as:
### drop_these_MidMusExp <- sample(which(DESIGN$tables$MusExp$mat[,2]==1), 7)
#The results were c(3, 22,  9, 21, 31, 24, 11)
#Remove these 7 to reproduce the results in the dissertation, or sample your own.
drop_these_MidMusExp <- c(3, 22,  9, 21, 31, 24, 11)
Sort_balanced <- Sort[,-drop_these_MidMusExp]

#Select the row that corresponds to the variable, "years of musical experience"
MusExp <- t(MusicFreeSort[37,]) #subjects' musical experience
colnames(MusExp) <- "MusExp"


#Initialize the DESIGN info
#... for the rows
Composers <- list()
Composers$labels <- c('Bach', 'Beethoven', 'Mozart')
Composers$vec <- rep(Composers$labels, each=12)
Composers$mat <- makeNominalData(as.matrix(Composers$vec))[, paste0(".", Composers$labels)]
rownames(Composers$mat) <- rownames(Sort_balanced)
colnames(Composers$mat) <- Composers$labels
#
DESIGN_Composers <- Initialize_DESIGNs(DESIGN_rows_mat = Composers$mat,  colors_B = c('#3c81c1', '#7d4dbc', '#52af6f'))

#...for the tables
Experience <- list()
Experience$MusExp <- MusExp[-drop_these_MidMusExp]
Experience$labels <- c('Low', 'Mid', 'High')
#cut the DESIGN variable
Experience$vec <- cut(x = Experience$MusExp, breaks = c(-1,.5, 5, 50), labels = Experience$labels) #0-0.9; 1-4.9, 5+
#create the DESIGN matrix, with columns in order
Experience$mat    <- makeNominalData(as.matrix(Experience$vec))[, paste0(".", Experience$labels)]
rownames(Experience$mat) <- paste0("s", 1:nrow(Experience$mat))
colnames(Experience$mat) <- Experience$labels
#
DESIGN_MusExp    <- Initialize_DESIGNs(DESIGN_tables_mat = Experience$mat, colors_D = c('#fcd305', '#ed961c', '#ce5050'))

# * The Composer Hypothesis ####


## Run DiDiSTATIS, MFA2 = FALSE, all inference = TRUE
res_DiDiSTATIS_Composers_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                              DESIGN_rows = DESIGN_Composers$DESIGN_rows,
                                              DESIGN_tables = DESIGN_MusExp$DESIGN_tables,
                                              MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                              MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                              Boot_tables = T,    Boot_tables_niter = 1000,
                                              LOO_rows = T,       LOO_rows_multiplier = 4,
                                              SH_rows = T,        SH_rows_niter = 100)

res_DiDiSTATIS_Composers_MFA2_F$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_DiDiSTATIS_Composers_MFA2_F)
Print_res2ppt_DiDiSTATIS(res_DiDiSTATIS = res_DiDiSTATIS_Composers_MFA2_F, main = "Music - Composers")


# * The Musician Hypothesis ####
Musician_vec <- rep(c("Arrau", "Barenboim", "Pires", "Richter"), 3, each = 3)
DESIGN_Musicians <- Initialize_DESIGNs(DESIGN_rows_mat = Musician_vec)
rownames(DESIGN_Musicians$DESIGN_rows$mat) <- rownames(Sort_balanced)

res_DiDiSTATIS_Musicians_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                              DESIGN_rows = DESIGN_Musicians$DESIGN_rows,
                                              DESIGN_tables = DESIGN_MusExp$DESIGN_tables,
                                              MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                              MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                              Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                              Boot_tables = T,    Boot_tables_niter = 1000,
                                              LOO_rows = T,       LOO_rows_multiplier = 4,
                                              SH_rows = T,        SH_rows_niter = 100)

res_DiDiSTATIS_Musicians_MFA2_F$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_DiDiSTATIS_Musicians_MFA2_F)
Print_res2ppt_DiDiSTATIS(res_DiDiSTATIS = res_DiDiSTATIS_Musicians_MFA2_F, main = "Music - Musicians")




# * The Arousal-Valence Hypothesis ####

#Create the DESIGN matrix on the rows (arousal_valence)
AV <- c(3, 6, 8, 11, 12, 13, 18, 26, 29, 30, 34, 36)
av <- c(2, 7, 9, 25, 28, 32)
Av <- c(10, 23, 24, 27, 31, 33)
aV <- c(1, 4, 5, 14, 15, 16, 17, 19, 20, 21, 22, 35)

Arousal_Valence_DESIGN_mat <- matrix(0, nrow(Sort_balanced), 4, dimnames = list(rownames(Sort_balanced), c('AV', 'av', 'Av', 'aV')))
Arousal_Valence_DESIGN_mat[AV,1] <- 1
Arousal_Valence_DESIGN_mat[av,2] <- 1
Arousal_Valence_DESIGN_mat[Av,3] <- 1
Arousal_Valence_DESIGN_mat[aV,4] <- 1
# rowSums(Arousal_Valence_DESIGN_mat)

DESIGN_AV <- Initialize_DESIGNs(DESIGN_rows_mat = Arousal_Valence_DESIGN_mat)

res_DiDiSTATIS_AV_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                       DESIGN_rows = DESIGN_AV$DESIGN_rows,
                                       DESIGN_tables = DESIGN_MusExp$DESIGN_tables,
                                       MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                       MFA2_Flag = FALSE, RV2_Flag = TRUE,
                                       Perm_omni_sort = T, Perm_omni_sort_niter = 1000,
                                       Boot_tables = T,    Boot_tables_niter = 1000,
                                       LOO_rows = T,       LOO_rows_multiplier = 4,
                                       SH_rows = T,        SH_rows_niter = 100)

res_DiDiSTATIS_AV_MFA2_F$TheTable <- DiDiSTATIS_Summary_Stats_Table(res_DiDiSTATIS_AV_MFA2_F)
Print_res2ppt_DiDiSTATIS(res_DiDiSTATIS = res_DiDiSTATIS_AV_MFA2_F, main = "Music - AV", Flip_axis2 = T)
Print_res2ppt_DiDiSTATIS(res_DiDiSTATIS = res_DiDiSTATIS_AV_MFA2_F, axes = c(3,2), main = "Music - AV", Flip_axis2 = T)
