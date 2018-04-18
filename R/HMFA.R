#The HMFA function within DiDiSTATIS
#
#'Conduct Hierarchical MFA
#'
#'@param D2_array An array of squared distance matrices
# #'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@param DESIGN_tables DESIGN matrix for the tables
#'@param n2k Number (of components) to keep
#'@param main Title for Factor Maps
#'@return Factor maps, and a list of computational results
#'@export

HMFA <- function(D2_array, DESIGN_tables, n2k=NULL, main = NULL){

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

  HMFA_collapsed <- GetGrandConsensus(CP_array, DESIGN_tables)

  ##Step 2: Decompose the Grand Compromise
  res_HMFA <- EigenHMFA(HMFA_collapsed, DESIGN, n2k=n2k)

  #Use that output to plot Factor Maps
  PlotHMFA(res_HMFA, axes=c(1,2), main = main)

  return(res_HMFA)

}
