# GetCompromise.R
# Part of the DiSTATIS function within DiDiSTATIS
#
#'Collapse tables for DiSTATIS
#'
#'@param CP_array A array of cross-product matrices
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@return A list of compromises and other computed objects
#'@export

GetCompromise <- function(CP_array, DESIGN_rows, DESIGN_tables){

  DiSTATIS_collapsed <- list()
  DiSTATIS_collapsed$data$CP_array <- CP_array

  ###########################
  ## 1. Get Compromise
  #  1a. dilate1, a scalar, the total number of people/tables, "K"
  dilate1 <- nrow(DESIGN_tables$mat)
  DiSTATIS_collapsed$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "K", that gives the MFA coefficient of each of the K tables
  MFA1 <- MFAnormCPFinder(CP_array)
  DiSTATIS_collapsed$coef$MFA1 <- MFA1

  NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  DiSTATIS_collapsed$data$NormedCP_array <- NormedCP_array

  #  1c. alpha1
  RV = TRUE
  DiSTATIS_collapsed$InnerProduct$RV_K       <- GetCmat(NormedCP_array, RV=RV)
  DiSTATIS_collapsed$InnerProduct$GetAlpha_res <- GetAlpha(DiSTATIS_collapsed$InnerProduct$RV_K)
  alpha1 <- DiSTATIS_collapsed$coef$alpha1 <- DiSTATIS_collapsed$InnerProduct$GetAlpha_res$alpha
  DiSTATIS_collapsed$data$Compromise   <- ComputeSplus(NormedCP_array, alpha1)
  dimnames(DiSTATIS_collapsed$data$Compromise) <- list(rownames(CP_array), rownames(CP_array))



  ###########################
  ## 2. Hierarchical == None for DiSTATIS
  ###########################


  ###########################
  ## 3. Apply coefficients to compute the OverWeighted individual and group data.

  ######
  #  3a. OverWeighted_CP_array
  OverWeighted_CP_array <- array(NA, dim=c(DESIGN_rows$AB, DESIGN_rows$AB, DESIGN_tables$CD))

  for(k in 1:DESIGN_tables$CD){
    this_table <- k
    OverWeighted_CP_array[,,this_table] <- (CP_array[,,this_table] *
                                            dilate1 *
                                            MFA1[this_table] *
                                            alpha1[this_table])
  }

  DiSTATIS_collapsed$data$OverWeighted_CP_array <- OverWeighted_CP_array

  return(DiSTATIS_collapsed)
}
