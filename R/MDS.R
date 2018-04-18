#The MDS function within DiDiSTATIS
#
#'Conduct metric multidimensional scaling
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #d, d2, CP
#'@param DESIGN List of DESIGN info
#'@param n2k Number of components to keep
#'@param main Title for Factor Maps
#'@return Factor maps, and a list of computational results
#'@export

MDS <- function(DATA, data_are=NULL, DESIGN, n2k=NULL, main = NULL){

  #Convert data to CP
  CP <- GetCP(DATA, data_are)

  #Eigen-decompose CP
  res_MDS <- EigenCP(CP, DESIGN, n2k=n2k)

  #Use that output to plot Factor Maps
  PlotCP(res_CP = res_MDS, axes = c(1,2), main = main)

  return(res_MDS)
}
