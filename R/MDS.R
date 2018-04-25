#The MDS function within DiDiSTATIS
#
#'Conduct metric multidimensional scaling
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #d, d2, CP
#'@param DESIGN_rows List of DESIGN info
#'@param n2k Number of components to keep
#'@return Results of MDS
#'@export

MDS <- function(DATA, data_are=NULL, DESIGN_rows, n2k=NULL){

  #Convert data to CP
  CP <- GetCP(DATA, data_are)

  #Eigen-decompose CP
  res_CP <- EigenCP_Full(CP = CP, DESIGN_rows = DESIGN_rows, n2k=n2k)


  #Build lists to organize output
  input <- list()
  input$DATA          <- DATA
  input$data_are      <- data_are
  input$CP            <- CP
  input$DESIGN_rows   <- DESIGN_rows

  returnME <- list(input = input,
                   eig = res_CP$eig)

  return(returnME)
}

