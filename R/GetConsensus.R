# GetConsensus.R
# Part of the MFA function within DiDiSTATIS
#
#'Collapse tables for MFA
#'
#'@param CP_array A array of cross-product matrices
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@return A list of compromises and other computed objects
#'@export

GetConsensus <- function(CP_array, DESIGN_rows, DESIGN_tables){

  MFA_collapsed <- list()
  MFA_collapsed$data$CP_array <- CP_array

  ###########################
  ## 1. Get Compromise
  #  1a. dilate1, a scalar, the total number of people/tables, "K"
  dilate1 <- nrow(DESIGN_tables$mat)
  MFA_collapsed$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "K", that gives the MFA coefficient of each of the K tables
  MFA1 <- MFAnormCPFinder(CP_array)
  MFA_collapsed$coef$MFA1 <- MFA1

  NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  MFA_collapsed$data$NormedCP_array <- NormedCP_array


  MFA_collapsed$data$Consensus <- apply(NormedCP_array, c(1,2), sum)





  ###########################
  ## 3. Apply coefficients to compute the OverWeighted individual and group data.

  ######
  #  3a. OverWeighted_CP_array
  OverWeighted_CP_array <- array(NA, dim=c(DESIGN_rows$AB, DESIGN_rows$AB, DESIGN_tables$CD))

  for(cd in 1:DESIGN_tables$CD){
    this_table <- cd
    OverWeighted_CP_array[,,this_table] <- (CP_array[,,this_table] *
                                              dilate1 *
                                              MFA1[this_table])
  }

  MFA_collapsed$data$OverWeighted_CP_array <- OverWeighted_CP_array


  ### RETURNS ###
  # MFA_collapsed$data$CP_array
  # MFA_collapsed$coef$dilate1
  # MFA_collapsed$coef$MFA1
  # MFA_collapsed$data$NormedCP_array
  # MFA_collapsed$data$Consensus
  # MFA_collapsed$data$OverWeighted_CP_array


  return(MFA_collapsed)
}
