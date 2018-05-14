#The DiSTATIS function within DiDiSTATIS
#
#'Conduct DiSTATIS
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #sort, d_array, d2_array, CP_array
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@param n2k Number (of components) to keep
#'@return A list of computational results
#'@export

DiSTATIS <- function(DATA, data_are, DESIGN_rows, DESIGN_tables, n2k=NULL){

  #Convert data to CP
  CP_array <- GetCP_array(DATA, data_are)

  ##Step 1: Identify table weights
  DiSTATIS_collapsed <- GetCompromise(CP_array, DESIGN_rows, DESIGN_tables)

  ##Step 2: Decompose the Grand Compromise
  res_DiSTATIS <- EigenDiSTATIS(DiSTATIS_collapsed, DESIGN_rows, DESIGN_tables, n2k=n2k)

  input <- list(DATA = DATA,
                data_are = data_are,
                DESIGN_rows = DESIGN_rows,
                DESIGN_tables = DESIGN_tables)

  returnME <- list(input = input,
                   DiSTATIS_collapsed = DiSTATIS_collapsed,
                   res_DiSTATIS = res_DiSTATIS)

  return(returnME)

}
