#The MFA function within DiDiSTATIS
#
#'Conduct MFA (specifically for squared distance matrices)
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #sort, d_array, d2_array, CP_array
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@param n2k Number (of components) to keep
#'@return A list of computational results
#'@export

MFA <- function(DATA, data_are, DESIGN_rows, DESIGN_tables, n2k=NULL){

  #Convert data to CP
  CP_array <- GetCP_array(DATA, data_are)

  ##Step 1: Identify table weights and compute Compromise, here called Consensus
  MFA_collapsed <- GetConsensus(CP_array, DESIGN_rows, DESIGN_tables)

  ##Step 2: Decompose the Consensus
  res_MFA <- EigenMFA(MFA_collapsed, DESIGN_rows, DESIGN_tables, n2k=n2k)

  input <- list(DATA = DATA,
                data_are = data_are,
                DESIGN_rows = DESIGN_rows,
                DESIGN_tables = DESIGN_tables)

  returnME <- list(input = input,
                   MFA_collapsed = MFA_collapsed,
                   res_MFA = res_MFA)

  return(returnME)

}
