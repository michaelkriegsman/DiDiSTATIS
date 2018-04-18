#Part of the HMFA function within DiDiSTATIS
#
#'Collapse tables for HMFA
#'
#'@param D2_array A array of squared distance matrices
#'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@param DESIGN_tables Column vector(s) to discriminate tables (nominal or colors or DESIGN)
#'@param n2k Number of components to keep
#'@param main Title for Factor Maps
#'@return Factor maps, and a list of computational results
#'@export

GetGrandConsensus <- function(CP_array, DESIGN_tables){

  HMFA_collapsed <- list()
  HMFA_collapsed$data$CP_array <- CP_array

  ###########################
  ## 1. Get Group Consensuses
  #  1a. dilate1, a vector of length "D", that gives the number of people in each group, "C(d)"
  dilate1 <- colSums(DESIGN_tables)
  HMFA_collapsed$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "CD", that gives the MFA coefficient of each of the CD tables (this is 1 for each participant)
  MFA1 <- MFAnormCPFinder(CP_array)
  HMFA_collapsed$coef$MFA1 <- MFA1

  NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  HMFA_collapsed$data$NormedCP_array <- NormedCP_array

  # Compute GroupConsensus_array[,,<d>])

  HMFA_collapsed$data$GroupConsensus_array <- array(NA, dim=c(A,A,D))

  #For each of the D groups...
  for(d in 1:D){

    #designate the relevant tables
    these_tables <- c(which(DESIGN_tables[,d]==1))

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
  dilate2 <- D
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
  OverWeighted_CP_array <- array(NA, dim=c(A, A, CD))

  for(d in 1:D){
    for(c in 1:colSums(DESIGN_tables)[d]){

      this_table <- which(DESIGN_tables[,d]==1)[c]
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
  OverWeighted_GroupConsensus_array <- array(NA, dim=c(A, A, D))

  for(d in 1:D){

    OverWeighted_GroupConsensus_array[,,d] <- (HMFA_collapsed$data$GroupConsensus_array[,,d] *
                                                  dilate2 *
                                                  MFA2[d])

  }

  HMFA_collapsed$data$OverWeighted_GroupConsensus_array <- OverWeighted_GroupConsensus_array




  ### RETURNS ###
  #Part I
  # HMFA_collapsed$data$CP_array
  # HMFA_collapsed$coef$dilate1
  # HMFA_collapsed$coef$MFA1
  # HMFA_collapsed$data$NormedCP_array
  # HMFA_collapsed$data$GroupConsensus_array
  #Part II
  # HMFA_collapsed$coef$dilate2
  # HMFA_collapsed$coef$MFA2
  # HMFA_collapsed$data$GrandConsensus
  #OverWeight
  # HMFA_collapsed$data$OverWeighted_CP_array
  # HMFA_collapsed$data$OverWeighted_GroupConsensus_array

  return(HMFA_collapsed)
}
