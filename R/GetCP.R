#'Transform from input data to a CP matrix
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@return Cross-product matrix
#'@export

GetCP <- function(DATA, data_are=NULL){


  if(data_are=='d'){
    #Square and double center a distance matrix to give a CP matrix
    CP <- DblCenterD2(DATA^2)
  }

  if(data_are=='d2'){
    #Double center a squared distance matrix to give a CP matrix
    CP <- DblCenterD2(DATA)
  }

  if(data_are=='CP'){
    #Rename a CP matrix as CP
    CP <- DATA
  }

  if(data_are=='X_to_cos'){
    #compute cross-product
    X <- DATA
    CP <- X %*% t(X)
  }

  if(data_are=='X_to_cov'){
    #center and compute cross-product
    X <- expo.scale(DATA, center=TRUE, scale=FALSE)
    CP <- X %*% t(X)
  }

  if(data_are=='X_to_cor'){
    #center and compute cross-product
    X <- expo.scale(DATA, center=TRUE, scale='ss1')
    CP <- X %*% t(X)
  }

  return(CP)
}
