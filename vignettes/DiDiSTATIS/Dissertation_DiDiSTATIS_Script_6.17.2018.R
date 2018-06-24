# The script/vignette to print DiDiSTATIS results as in the Dissertation

rm(list=ls())
library(DiDiSTATIS)
library(PlotDiDiSTATIS)


# Example 1: Music Free Sort
MusicFreeSort

#Select the data for the 36 stimuli
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

# Example 1a: Composers ####
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
Print_res2ppt_DiDiSTATIS(res_DiDiSTATIS_Composers_MFA2_F, main = "Music - Composers")


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
                                              MFA2_Flag = FALSE, RV2_Flag = TRUE)

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


res_HiDiSTATIS <- HiDiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                             DESIGN_rows = DESIGN$DESIGN_rows,
                             DESIGN_tables = DESIGN$DESIGN_tables,
                             n2k=NULL)

Plot_HiDiSTATIS_F..(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 2, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 3, Flip_axis1 = TRUE, Flip_axis2 = FALSE)




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

res_DiDiSTATIS_AV_MFA2_F <- DiDiSTATIS(DATA = Sort_balanced, data_are = 'sort', n2k = NULL,
                                       DESIGN_rows = DESIGN$DESIGN_rows,
                                       DESIGN_tables = DESIGN$DESIGN_tables,
                                       MFA1_Flag = TRUE, RV1_Flag = TRUE,
                                       MFA2_Flag = FALSE, RV2_Flag = TRUE)

res_DiDiSTATIS <- res_DiDiSTATIS_AV_MFA2_F
source('C:/Users/Michael A. Kriegsman/Box Sync/Dissertation/RStudio2Git/DiDiSTATIS/vignettes/DiDiSTATIS/Print_DiDiSTATIS_Ex1.2_AV_to_pptx.R')



res_HiDiSTATIS <- HiDiSTATIS(DATA = Sort_balanced, data_are = 'sort',
                             DESIGN_rows = DESIGN$DESIGN_rows,
                             DESIGN_tables = DESIGN$DESIGN_tables,
                             n2k=NULL,
                             Boot_tables = T, Boot_tables_niter = 1000)

Plot_HiDiSTATIS_F..(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = TRUE, Flip_axis2 = FALSE)

Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 1, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 2, Flip_axis1 = TRUE, Flip_axis2 = FALSE)
Plot_HiDiSTATIS_F.d(res_HiDiSTATIS, axes = c(1,2), d = 3, Flip_axis1 = TRUE, Flip_axis2 = FALSE)

for(i in 1:res_HiDiSTATIS$input$DESIGN_rows$AB){
  Plot_HiDiSTATIS_Boot_centered_CIs(res_HiDiSTATIS = res_HiDiSTATIS, i = i)
}

