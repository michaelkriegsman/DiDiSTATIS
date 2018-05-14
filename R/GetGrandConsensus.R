#Part of the HMFA function within DiDiSTATIS
#
#'Collapse tables for HMFA
#'
#'@param CP_array A array of cross-product matrices
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@return A list of compromises and other computed objects
#'@export

GetGrandConsensus <- function(CP_array, DESIGN_rows, DESIGN_tables){

  HMFA_collapsed <- list()
  HMFA_collapsed$data$CP_array <- CP_array

  ###########################
  ## 1. Get Group Consensuses
  #  1a. dilate1, a vector of length "D", that gives the number of people in each group, "C(d)"
  dilate1 <- colSums(DESIGN_tables$mat)
  HMFA_collapsed$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "CD", that gives the MFA coefficient of each of the CD tables (this is 1 for each participant)
  MFA1 <- MFAnormCPFinder(CP_array)
  HMFA_collapsed$coef$MFA1 <- MFA1

  NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  HMFA_collapsed$data$NormedCP_array <- NormedCP_array

  # Compute GroupConsensus_array[,,<d>])

  HMFA_collapsed$data$GroupConsensus_array <- array(NA, dim=c(DESIGN_rows$AB,DESIGN_rows$AB,DESIGN_tables$D))

  #For each of the D groups...
  for(d in 1:DESIGN_tables$D){

    #designate the relevant tables
    these_tables <- c(which(DESIGN_tables$mat[,d]==1))

    ##Compute each GroupConsensus (depends on ComputeSplus.R)
    #use alpha_C_in_D<d> to compute Consensus_Plus_in_d<d>

    GetGroupConsensus <- paste0("HMFA_collapsed$data$GroupConsensus_array[,,",d,"] <- apply(NormedCP_array[,,these_tables], c(1,2), sum)")
    eval(parse(text = GetGroupConsensus))
  }

  #And reassign the names to the Group Consensuses
  dimnames(HMFA_collapsed$data$GroupConsensus_array) <- list(rownames(CP_array), rownames(CP_array), colnames(DESIGN_tables))


  ###########################
  ## 2. Get Grand Consensus
  #  2a. dilate2, a scalar, the number of groups, "D".
  dilate2 <- DESIGN_tables$D
  HMFA_collapsed$coef$dilate2 <- dilate2

  #  2b. MFA2, a vector of length "D", that gives the MFA coefficient of each of the D GroupConsensuses
  MFA2 <- MFAnormCPFinder(HMFA_collapsed$data$GroupConsensus_array)
  HMFA_collapsed$coef$MFA2 <- MFA2

  # and apply MFA2 to the Group Consensuses to give the NormedGroupConsensus_array
  NormedGroupConsensus_array <- CP2MFAnormedCP(HMFA_collapsed$data$GroupConsensus_array)
  HMFA_collapsed$data$NormedGroupConsensus_array <- NormedGroupConsensus_array

  # Compute the Grand Consensus
  GrandConsensus <- apply(NormedGroupConsensus_array, c(1,2), sum)
  HMFA_collapsed$data$GrandConsensus <- GrandConsensus

  ###########################
  ## 3. Apply coefficients to compute the OverWeighted individual and group data.

  ######
  #  3a. OverWeighted_CP_array
  OverWeighted_CP_array <- array(NA, dim=c(DESIGN_rows$AB, DESIGN_rows$AB, DESIGN_tables$CD))

  for(d in 1:DESIGN_tables$D){
    for(c in 1:colSums(DESIGN_tables$mat)[d]){

      this_table <- which(DESIGN_tables$mat[,d]==1)[c]
      OverWeighted_CP_array[,,this_table] <- (CP_array[,,this_table] *
                                                dilate1[d] *
                                                MFA1[this_table] *
                                                dilate2 *
                                                MFA2[d])

    }
  }

  HMFA_collapsed$data$OverWeighted_CP_array <- OverWeighted_CP_array



  #####
  # 3b. OverWeighted_GroupConsensus_array
  OverWeighted_GroupConsensus_array <- array(NA, dim=c(DESIGN_rows$AB, DESIGN_rows$AB, DESIGN_tables$D))

  for(d in 1:DESIGN_tables$D){

    OverWeighted_GroupConsensus_array[,,d] <- (HMFA_collapsed$data$GroupConsensus_array[,,d] *
                                                  dilate2 *
                                                  MFA2[d])

  }

  HMFA_collapsed$data$OverWeighted_GroupConsensus_array <- OverWeighted_GroupConsensus_array



  return(HMFA_collapsed)
}
