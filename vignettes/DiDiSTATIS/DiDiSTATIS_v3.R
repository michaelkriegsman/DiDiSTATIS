#DiDiSTATIS vignette (& ToDo List)

rm(list=ls())
library(DiDiSTATIS)


## 1. Load data (called Sort) and DESIGN (called DESIGN)
#for Free Sort of natural stimuli (Dowling, W.J.)
source('C:/Users/Michael A. Kriegsman/Google Drive/Dissertation/DiDiSTATIS/DiDiSTATIS/vignettes/Initialize_Dowling_NaturalStimuli_Example.R')


## 2. Preprocess
#Convert Sort to Distance (double center sorting data)
D2_array <- DistanceFromSort(Sort)


## 3. Analyze
res_DiDiSTATIS <- DiDiSTATIS(Data_array = D2_array, data_are = 'D2_array',
                             DESIGN_rows = DESIGN$rows$Comp, DESIGN_tables = DESIGN$tables$MusExp,
                             n2k=NULL)


## 4. Inference
######### DiDiSTATIS_inf will be the wrapper, or may be deleted to run each inference separately.
######### These parameters are just suggestions. Things I may need to code.
# infer_DiDiSTATIS <- DiDiSTATIS_inf(res_DiDiSTATIS = res_DiDiSTATIS,
                                   # perm_rows=F, perm_tables=F, perm_rows_tables=F,
                                   # boot_tables=F,
                                   # LOO_rows=F) #NULL or k. If k=1, SpHa==jackknife. If
                                   # Not sure how to set switches for the SpHa yet... Need to ugly code it...

## 5. Plotting functions
