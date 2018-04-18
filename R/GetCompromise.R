# GetCompromise.R
#Part of the DiSTATIS function within DiDiSTATIS
#
#'Collapse tables for DiSTATIS
#'
#'@param D2_array A array of squared distance matrices
#'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@param DESIGN_tables Column vector(s) to color tables (nominal or colors or DESIGN)
#'@param n2k Number of components to keep
#'@param main Title for Factor Maps
#'@return Factor maps, and a list of computational results
#'@export

GetCompromise <- function(CP_array, DESIGN_tables){

  DiSTATIS_collapsed <- list()
  DiSTATIS_collapsed$data$CP_array <- CP_array

  ###########################
  ## 1. Get Compromise
  #  1a. dilate1, a scalar, the total number of people/tables, "K"
  dilate1 <- nrow(DESIGN_tables)
  DiSTATIS_collapsed$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "K", that gives the MFA coefficient of each of the K tables
  MFA1 <- MFAnormCPFinder(CP_array)
  DiSTATIS_collapsed$coef$MFA1 <- MFA1

  NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  DiSTATIS_collapsed$data$NormedCP_array <- NormedCP_array

  #  1c. alpha1 (on the way, compute RV_C_in_d<d>, alpha_C_in_d<d>,
  #              and while looping, may as well compute GroupCompromise_array[,,<d>])

  RV = TRUE
  RV     <- DiSTATIS_collapsed$InnerProduct$RV_K    <- GetCmat(NormedCP_array, RV=RV)
  alpha1 <- DiSTATIS_collapsed$coef$alpha1 <- GetAlpha(DiSTATIS_collapsed$InnerProduct$RV_K)
  DiSTATIS_collapsed$data$Compromise <- ComputeSplus(NormedCP_array, DiSTATIS_collapsed$coef$alpha1)
  dimnames(DiSTATIS_collapsed$data$Compromise) <- list(rownames(CP_array), rownames(CP_array))



  ###########################
  ## 2. Hierarchical == None for DiSTATIS
  ###########################


  ###########################
  ## 3. Apply coefficients to compute the OverWeighted individual and group data.

  ######
  #  3a. OverWeighted_CP_array
  OverWeighted_CP_array <- array(NA, dim=c(A, A, K))

  for(k in 1:K){
    this_table <- k
    OverWeighted_CP_array[,,this_table] <- (CP_array[,,this_table] *
                                            dilate1 *
                                            MFA1[this_table] *
                                            alpha1[this_table])
  }

  DiSTATIS_collapsed$data$OverWeighted_CP_array <- OverWeighted_CP_array


  ### RETURNS ###
  # DiSTATIS_collapsed$data$CP_array
  # DiSTATIS_collapsed$coef$dilate1
  # DiSTATIS_collapsed$coef$MFA1
  # DiSTATIS_collapsed$data$NormedCP_array
  # DiSTATIS_collapsed$InnerProduct$RV_K
  # DiSTATIS_collapsed$coef$alpha1
  # DiSTATIS_collapsed$data$Compromise
  # DiSTATIS_collapsed$data$OverWeighted_CP_array


  return(DiSTATIS_collapsed)
}
