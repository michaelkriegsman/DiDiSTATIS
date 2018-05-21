#Initialize_Balanced_Dowling_free_balanced_Pianists.R

SortFree_36 <- read.csv('C:/Users/Michael A. Kriegsman/Google Drive/Projects/DiDiSTATIS_Composers/WithMoreSubj_ForDissertation/natural36_free_forR.csv', row.names=1)
Sort <- SortFree_36[-37,]
MusExp <- t(SortFree_36[37,]) #subjects' musical experience
colnames(MusExp) <- "MusExp"


#Balance the groups
#Randomly select 7 participants to remove from the Mid Musical Experience group
#Leaves 10 participants in each group.

# drop_these_MidMusExp <- sample(which(DESIGN$tables$MusExp$mat[,2]==1), 7)
#The results were c(3, 22,  9, 21, 31, 24, 11)

# 4.25.2018, I had previously removed subjects 33 and 34, as mentioned below, but these are in the High Experience group, so I need to keep them, and instead I revert my balancing to the previous setup...
# #Then, noticed that bc033 and bc034 each created 15 categories, and their PFS are unstable and unusual and huge.
# #So, take those out, and bring in 2 other subjects
# drop_these_MidMusExp <- c(9, 21, 31, 24, 11, 33, 34)
drop_these_MidMusExp <- c(3, 22,  9, 21, 31, 24, 11)
Sort_balanced <- Sort[,-drop_these_MidMusExp]






#Create the DESIGN matrix on the rows (arousal_valence)
Pianist_vec <- rep(c("Arrau", "Barenboim", "Pires", "Richter"), 3, each = 3)




#Create the DESIGN matrix on the tables (still Mus.Exp)
MusExp_balanced <- MusExp[-drop_these_MidMusExp]
MusExp_labels <- c('Low', 'Mid', 'High')
#cut the DESIGN variable
MusExp_vec <- cut(x = MusExp_balanced, breaks = c(-1,.5, 5, 50), labels = MusExp_labels) #0-0.9; 1-4.9, 5+
#create the DESIGN matrix, with columns in order
MusExp_mat <- makeNominalData(as.matrix(MusExp_vec))[, paste0(".", MusExp_labels)]
rownames(MusExp_mat) <- paste0("s", 1:nrow(MusExp_mat))
colnames(MusExp_mat) <- MusExp_labels



#Create the DESIGNs!
DESIGN <- Initialize_DESIGNs(DESIGN_rows_mat   = Pianist_vec,
                             DESIGN_tables_mat = MusExp_mat, colors_D = c('#fcd305','#ed961c','#ce5050'))

rownames(DESIGN$DESIGN_rows$mat) <- rownames(Sort_balanced)
