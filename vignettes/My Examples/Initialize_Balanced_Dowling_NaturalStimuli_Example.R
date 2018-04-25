#Initialize Dowling_NaturalStimuli_Example

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



### DESIGN$tables
DESIGN <- list()
###make the user input #labels# and #vec#
### I can always get from $vec and $labels to $mat and $colors_

###need functions to start creating the DESIGN matrix:
#1. input the labels
#2. Create $vec.
### if in a simple order, just use rep()
### if nominal but out of order, initialize the order as the variable name, in $vec trade numbers for labels
### if numeric, cut into categories, and assign the labels.



### DESIGN$rows
###categorical: just rpoduce the right vec
DESIGN$rows$Comp$labels <- c("Bach", 'Beethoven', 'Mozart')
DESIGN$rows$Comp$vec <- rep(DESIGN$rows$Comp$labels,each=12)
DESIGN$rows$Comp$mat <- makeNominalData(as.matrix(DESIGN$rows$Comp$vec))[, paste0(".", DESIGN$rows$Comp$labels)]
rownames(DESIGN$rows$Comp$mat) <- rownames(Sort_balanced)
colnames(DESIGN$rows$Comp$mat) <- DESIGN$rows$Comp$labels

DESIGN$rows$Comp$Pb_Full  <- Bary_Projector(DESIGN$rows$Comp$mat)
DESIGN$rows$Comp$Pb_Cond  <- Bary_Projector_Cond(DESIGN$rows$Comp$mat)

DESIGN$rows$Comp$B  <- ncol(DESIGN$rows$Comp$mat)
DESIGN$rows$Comp$AB <- nrow(DESIGN$rows$Comp$mat)


# DESIGN$rows$Comp$colors_B  <- prettyGraphsColorSelection(3, offset=3)
DESIGN$rows$Comp$colors_B  <- c('#3c81c1', '#7d4dbc', '#52af6f') #possible 4th color... brown #9d6725 or #714107
DESIGN$rows$Comp$colors_AB <- DESIGN$rows$Comp$vec
for(b in 1:DESIGN$rows$Comp$B){
  DESIGN$rows$Comp$colors_AB[which(DESIGN$rows$Comp$mat[,b]==1)] <- DESIGN$rows$Comp$colors_B[b]
}


### DESIGN$tables
#Low, Mid, High MusExp
############################need###############################
#labels, breaks, colors_B
############################need###############################
###continuous: cut into categories
DESIGN$tables$MusExp$MusExp <- MusExp[-drop_these_MidMusExp]
DESIGN$tables$MusExp$labels <- c('Low', 'Mid', 'High')
#cut the DESIGN variable
DESIGN$tables$MusExp$vec <- cut(x = DESIGN$tables$MusExp$MusExp, breaks = c(-1,.5, 5, 50), labels = DESIGN$tables$MusExp$labels) #0-0.9; 1-4.9, 5+
#create the DESIGN matrix, with columns in order
DESIGN$tables$MusExp$mat    <- makeNominalData(as.matrix(DESIGN$tables$MusExp$vec))[, paste0(".", DESIGN$tables$MusExp$labels)]
rownames(DESIGN$tables$MusExp$mat) <- paste0("s", 1:nrow(DESIGN$tables$MusExp$mat))
colnames(DESIGN$tables$MusExp$mat) <- DESIGN$tables$MusExp$labels

DESIGN$tables$MusExp$Pd_Full <- Bary_Projector(DESIGN$tables$MusExp$mat)
DESIGN$tables$MusExp$Pd_Cond <- Bary_Projector_Cond(DESIGN$tables$MusExp$mat)

DESIGN$tables$MusExp$D  <- ncol(DESIGN$tables$MusExp$mat)
DESIGN$tables$MusExp$CD <- nrow(DESIGN$tables$MusExp$mat)

#Set group colors
# DESIGN$tables$MusExp$colors_D  <- c("dodgerblue", "tomato2", "goldenrod")
DESIGN$tables$MusExp$colors_D  <- c('#fcd305','#ed961c','#ce5050')
DESIGN$tables$MusExp$colors_CD <- as.matrix(DESIGN$tables$MusExp$vec)
for(d in 1:DESIGN$tables$MusExp$D){
  DESIGN$tables$MusExp$colors_CD[which(DESIGN$tables$MusExp$mat[,d]==1)] <- DESIGN$tables$MusExp$colors_D[d]
}



