#'Transform from input data to a CP_array
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #sort, d_array, d2_array, CP_array
#'@return An array of cross-product matrices
#'@export

GetCP_array <- function(DATA, data_are=NULL){

  if(data_are=="sort"){
    #Convert Sort to Distance, and distance to CP
    D2_array <- DistanceFromSort(DATA)
    CP_array <- Dist2CP(D2_array)
  }
  if(data_are=='d_array'){
    #Square and double center matrices of a distance array to give a CP array
    CP_array <- array(apply(DATA^2, 3, DblCenterD2), dim(DATA), dimnames(DATA))
  }

  if(data_are=='d2_array'){
    #Double center matrices of a distance array to give a CP array
    CP_array <- array(apply(DATA, 3, DblCenterD2), dim(DATA), dimnames(DATA))
  }

  if(data_are=='CP_array'){
    #Rename a CP matrix as CP
    CP_array <- DATA
  }

  return(CP_array)
}
