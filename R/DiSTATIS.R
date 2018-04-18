#The DiSTATIS function within DiDiSTATIS
#
#'Conduct DiSTATIS
#'
#'@param D2_array An array of squared distance matrices
# #'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@param DESIGN_tables DESIGN matrix for the tables
#'@param n2k Number (of components) to keep
#'@param main Title for Factor Maps
#'@return Factor maps, and a list of computational results
#'@export

DiSTATIS <- function(D2_array, DESIGN_tables, n2k=NULL, main = NULL){

  #Convert data to CP
  ## CP_array <- apply(D_array, c(1,2), GetCP, data_are='d2')
  # would need to make it loop through tables of D_array...
  # for now, data_are must == 'd2'. Thus the name, D2_array
  CP_array <- Dist2CP(D2_array)

  ##Step 1: Identify individual and group table weights
  #Get group and grand compromise.
  #MFA:        GetConsensus()
  #HMFA:       GetGrandConsensus()
  #DiSTATIS:   GetCompromise()
  #HiDiSTATIS: GetGrandCompromise()

  DiSTATIS_collapsed <- GetCompromise(CP_array, DESIGN_tables)

  ##Step 2: Decompose the Grand Compromise
  res_DiSTATIS <- EigenDiSTATIS(DiSTATIS_collapsed, DESIGN, n2k=n2k)
  #res <- 
  
  #Use that output to plot Factor Maps
  PlotDiSTATIS(res_DiSTATIS, axes=c(1,2), main = main)

  return(res_DiSTATIS)

}
