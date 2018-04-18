#Part of the DiDiSTATIS function within DiDiSTATIS
#
#'Integrate tables hierarchically, to give barycentric group and grand compormises
#'Differs from GetBaryGrandComp.R by computing alpha2_supp,
#'because bootstrapped group compormises are projected as supplementary
#'and so need to be weighted as supplementary to the original Rv_D space.
#'
#'@param CP_array An array of CP matrices
#'@param DESIGN_rows List of DESIGN info for rows
#'@param DESIGN_tables List of DESIGN info for tables
#'@param Hierarchy_of_tables Results related to computing Group and Grand Compromises
#'@param MFA1_Flag #TRUE gives MFA-norm; FALSE gives no norm
#'@param RV1_Flag # TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/CD (1/N)
#'@param MFA2_Flag #TRUE gives MFA-norm; FALSE gives no norm
#'@param RV2_Flag # TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/D
#'@return A list of compromises and other computed objects
#'@export

GetBaryGrandComp_BootTables <- function(CP_array, DESIGN_rows, DESIGN_tables, Hierarchy_of_tables, MFA1_Flag=TRUE, RV1_Flag=TRUE, MFA2_Flag=TRUE, RV2_Flag=TRUE){

  RETURN <- list()
  RETURN$data$CP_array <- CP_array

  ###########################
  ## 1. Get Group Compromises
  #  1a. dilate1, a vector of length "D", that gives the number of people in each group, "C(d)"
  dilate1 <- colSums(DESIGN_tables$mat)
  RETURN$coef$dilate1 <- dilate1

  #  1b. MFA1, a vector of length "CD", that gives the MFA coefficient of each of the CD tables (this is 1 for each participant)
  if(MFA1_Flag){
    MFA1 <- MFAnormCPFinder(CP_array)
    RETURN$coef$MFA1 <- MFA1
    NormedCP_array <- CP2MFAnormedCP(CP_array) #This is CP_array with each table scaled by its MFA1 coefficient
  }
  if(!MFA1_Flag){
    # RETURN$coef$MFA1 <- MFA1 <- c(rep(1/DESIGN_tables$CD, DESIGN_tables$CD))
    RETURN$coef$MFA1 <- MFA1 <- rep(mean(MFAnormCPFinder(CP_array)), DESIGN_tables$CD)
    NormedCP_array <- CP_array * mean(MFAnormCPFinder(CP_array))
  }

  RETURN$data$NormedCP_array <- NormedCP_array

  #  1c. alpha1 (on the way, compute RV_C_in_d<d>, alpha_C_in_d<d>,
  #              and while looping, may as well compute GroupCompromise_array[,,<d>])

  alpha1 <- rep(NA, nrow(DESIGN_tables$mat))

  RETURN$data$GroupCompromise_array <- array(NA, dim=c(nrow(CP_array),nrow(CP_array),DESIGN_tables$D))
  RETURN$data$Pb_GroupCompromise_Pb_array <- array(NA, dim=c(nrow(CP_array),nrow(CP_array),DESIGN_tables$D))

  #For each of the D groups...
  for(d in 1:DESIGN_tables$D){

    #designate the relevant tables
    these_tables <- c(which(DESIGN_tables$mat[,d]==1))

    if(is.logical(RV1_Flag)){
      ##Compute the inner product (the RV matrix, or C matrix) within each group: RV_C_in_d<d>
      #depends (GetCmat.R)
      RV = TRUE    #TRUE gives RV; FALSE gives C.
      GetRVwithin <- paste0("RETURN$InnerProduct$RV_C_in_d",d," <- GetCmat(NormedCP_array[,,these_tables], RV=",RV,")")
      eval(parse(text = GetRVwithin))

      ##From eig(RV_within_d<d>), get AlphaWithin; alpha_C_in_d<d>
      #depends (GetAlpha.R)
      GetAlphaWithin <- paste0("RETURN$InnerProduct$alpha_C_in_d" ,d, " <- GetAlpha(RETURN$InnerProduct$RV_C_in_d" ,d, ")")
      eval(parse(text = GetAlphaWithin))

      #Get all alpha-weights back into 1 vector in the same order as rows of DESIGN_tables$mat
      GoGetAlpha1 <- paste0("alpha1[these_tables] <- RETURN$InnerProduct$alpha_C_in_d" ,d, "$alpha")
      eval(parse(text = GoGetAlpha1))
    }
    if(RV1_Flag=="skip"){
      alpha1[these_tables] <- rep(1/colSums(DESIGN_tables$mat)[d], colSums(DESIGN_tables$mat)[d])
    }

    ##Compute each GroupCompromise (depends on ComputeSplus.R)
    #use alpha_C_in_D<d> to compute Compromise_Plus_in_d<d>
    GetGroupCompromise <- paste0("RETURN$data$GroupCompromise_array[,,",d,"] <- ComputeSplus(NormedCP_array[,,these_tables], RETURN$InnerProduct$alpha_C_in_d",d,"$alpha)")
    eval(parse(text = GetGroupCompromise))


    ###########################
    ## 2. Apply row design
    RETURN$data$Pb_GroupCompromise_Pb_array[,,d] <- DESIGN_rows$Pb_Full %*% RETURN$data$GroupCompromise_array[,,d] %*% DESIGN_rows$Pb_Full
    ###########################
  }

  RETURN$coef$alpha1 <- alpha1
  #And reassign the names to the Group Compromises
  dimnames(RETURN$data$GroupCompromise_array) <- list(rownames(CP_array), rownames(CP_array), colnames(DESIGN_tables$mat))
  dimnames(RETURN$data$Pb_GroupCompromise_Pb_array) <- dimnames(RETURN$data$GroupCompromise_array)



  ###########################
  ## 3. Get Grand Compromise
  #  3a. dilate2, a scalar, the number of groups, "D".
  dilate2 <- DESIGN_tables$D
  RETURN$coef$dilate2 <- dilate2

  #  3b. MFA2, a vector of length "D", that gives the MFA coefficient of each of the D GroupCompromises
  if(MFA2_Flag){
    MFA2 <- MFAnormCPFinder(RETURN$data$Pb_GroupCompromise_Pb_array)
    RETURN$coef$MFA2 <- MFA2
    Normed_Pb_GroupCompromise_Pb_array <- CP2MFAnormedCP(RETURN$data$Pb_GroupCompromise_Pb_array)
  }
  if(!MFA2_Flag){
    # RETURN$coef$MFA2 <- MFA2 <- c(rep(1/DESIGN_tables$D, DESIGN_tables$D))
    RETURN$coef$MFA2 <- MFA2 <- rep(mean(MFAnormCPFinder(RETURN$data$Pb_GroupCompromise_Pb_array)), DESIGN_tables$D)
    Normed_Pb_GroupCompromise_Pb_array <- RETURN$data$Pb_GroupCompromise_Pb_array * mean(MFAnormCPFinder(RETURN$data$Pb_GroupCompromise_Pb_array))
  }

  # and apply MFA2 to the Barycentric Group Compromises to give the Normed_Pb_GroupCompromise_Pb_array
  RETURN$data$Normed_Pb_GroupCompromise_Pb_array <- Normed_Pb_GroupCompromise_Pb_array

  #  3c. alpha2, a vector of length "D", that gives the alpha-weight for each MFA-Normalized Group

  if(is.logical(RV2_Flag)){
    #on the way, compute RV_D
    array_for_RV_supp <- array(c(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array, RETURN$data$Normed_Pb_GroupCompromise_Pb_array),
                               dim=c(nrow(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array),
                                     ncol(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array),
                                     dim(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array)[3]*2))

    #Get Rv_rows... the Rvs between the original normed group compromises and the booted normed group compromises
    RV_B_D_supp <- GetCmat(array_for_RV_supp, RV=RV2_Flag)[1:DESIGN_tables$D, -c(1:DESIGN_tables$D)]
    rownames(RV_B_D_supp) <- paste0("boot", dimnames(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array)[[3]])
    colnames(RV_B_D_supp) <- dimnames(Hierarchy_of_tables$data$Normed_Pb_GroupCompromise_Pb_array)[[3]]
    RETURN$InnerProduct$RV_B_D_supp <- RV_B_D_supp

    #compute alpha2_supp
    # Project supplementary RV_rows into the original Rv space,
    #reconstitute supplementary factor scores for the booted tables
    #and work backward to get to supplementary alpha weights, called alpha2_supp
    RV_B_D_F_supp <- RV_B_D_supp %*% Hierarchy_of_tables$InnerProduct$res_RV_B_D$eig$ProjMat
    RV_B_D_U_supp <- abs(RV_B_D_F_supp[,1] * Hierarchy_of_tables$InnerProduct$res_RV_B_D$eig$Lambda_vec[1]^(-1/2))
    alpha2_supp <- as.matrix(RV_B_D_U_supp) %*% sum(Hierarchy_of_tables$InnerProduct$res_RV_B_D$eig$U[,1])^-1
    RETURN$coef$alpha2_supp <- alpha2_supp
  }
  if(RV2_Flag=="skip"){
    RETURN$coef$alpha2_supp <- alpha2_supp <- rep(1/DESIGN_tables$D, DESIGN_tables$D)
  }



  # Compute the Bary_Grand Compromise
  Bary_GrandCompromise <- ComputeSplus(Normed_Pb_GroupCompromise_Pb_array, alpha2_supp)
  RETURN$data$Bary_GrandCompromise <- Bary_GrandCompromise

  # Compute SS of Bary_Grand Compromise
  RETURN$data$SSb.._fromTrace <- sum(diag(Bary_GrandCompromise))

  ###########################
  ## 4. Apply coefficients to compute the OverWeighted individual and group data.

  ######
  #  4a. OverWeighted_CP_array
  OverWeighted_CP_array <- array(NA, dim=c(nrow(CP_array), nrow(CP_array), DESIGN_tables$CD))

  for(d in 1:DESIGN_tables$D){
    for(c in 1:colSums(DESIGN_tables$mat)[d]){

      this_table <- which(DESIGN_tables$mat[,d]==1)[c]
      OverWeighted_CP_array[,,this_table] <- (CP_array[,,this_table] *
                                                dilate1[d] *
                                                MFA1[this_table] *
                                                alpha1[this_table] *
                                                dilate2 *
                                                MFA2[d] *
                                                alpha2_supp[d])

    }
  }

  RETURN$data$OverWeighted_CP_array <- OverWeighted_CP_array

  #Compute SS of Individual data tables
  RETURN$data$Overweighted_SScd <- matrix(NA, DESIGN_tables$CD, 1)
  for(cd in 1:DESIGN_tables$CD){
    RETURN$data$Overweighted_SScd[cd] <- sum(diag(RETURN$data$OverWeighted_CP_array[,,cd]))
  }

  #####
  # 4b. OverWeighted_GroupCompromise_array
  OverWeighted_GroupCompromise_array <- array(NA, dim=c(nrow(CP_array), nrow(CP_array), DESIGN_tables$D))

  for(d in 1:DESIGN_tables$D){

    OverWeighted_GroupCompromise_array[,,d] <- (RETURN$data$GroupCompromise_array[,,d] *
                                                  dilate2 *
                                                  MFA2[d] *
                                                  alpha2_supp[d])

  }

  RETURN$data$OverWeighted_GroupCompromise_array <- OverWeighted_GroupCompromise_array


  #Compute SS of Group Compromises
  RETURN$data$Overweighted_SS.d <- matrix(NA, DESIGN_tables$D, 1)
  for(d in 1:DESIGN_tables$D){
    RETURN$data$Overweighted_SS.d[d] <- sum(diag(RETURN$data$OverWeighted_GroupCompromise_array[,,d]))
  }




  RETURN$data$OverWeighted_GrandCompromise <- apply(OverWeighted_GroupCompromise_array, c(1,2), mean)










  #####
  # 3c. OverWeighted_Pb_GroupCompromise_Pb_array
  OverWeighted_Pb_GroupCompromise_Pb_array <- array(NA, dim=c(nrow(CP_array), nrow(CP_array), DESIGN_tables$D))

  for(d in 1:DESIGN_tables$D){

    OverWeighted_Pb_GroupCompromise_Pb_array[,,d] <- (RETURN$data$Pb_GroupCompromise_Pb_array[,,d] *
                                                        dilate2 *
                                                        MFA2[d] *
                                                        alpha2_supp[d])

  }

  RETURN$data$OverWeighted_Pb_GroupCompromise_Pb_array <- OverWeighted_Pb_GroupCompromise_Pb_array


  #Compute SS of Group Compromises
  RETURN$data$Overweighted_SSb.d <- matrix(NA, DESIGN_tables$D, 1)
  for(d in 1:DESIGN_tables$D){
    RETURN$data$Overweighted_SSb.d[d] <- sum(diag(RETURN$data$OverWeighted_Pb_GroupCompromise_Pb_array[,,d]))
  }









  ######
  #  3d. OverWeighted_Pb_CP_Pb_array
  OverWeighted_Pb_CP_Pb_array <- array(NA, dim=c(nrow(CP_array), nrow(CP_array), nrow(DESIGN_tables$mat)))

  for(d in 1:DESIGN_tables$D){
    for(c in 1:colSums(DESIGN_tables$mat)[d]){

      this_table <- which(DESIGN_tables$mat[,d]==1)[c]
      OverWeighted_Pb_CP_Pb_array[,,this_table] <- (DESIGN_rows$Pb_Full %*% CP_array[,,this_table] %*% DESIGN_rows$Pb_Full *
                                                      dilate1[d] *
                                                      MFA1[this_table] *
                                                      alpha1[this_table] *
                                                      dilate2 *
                                                      MFA2[d] *
                                                      alpha2_supp[d])
    }
  }

  RETURN$data$OverWeighted_Pb_CP_Pb_array <- OverWeighted_Pb_CP_Pb_array

  #Compute SS of Individual data tables
  RETURN$data$Overweighted_SSbcd <- matrix(NA, nrow(DESIGN_tables$mat), 1)
  for(cd in 1:nrow(DESIGN_tables$mat)){
    RETURN$data$Overweighted_SSbcd[cd] <- sum(diag(RETURN$data$OverWeighted_Pb_CP_Pb_array[,,cd]))
  }








  ### RETURNS ###
  #Part I
  # RETURN$data$CP_array
  # RETURN$coef$dilate1
  # RETURN$coef$MFA1
  # RETURN$data$NormedCP_array
  # RETURN$coef$alpha1
  # RETURN$data$GroupCompromise_array
  #Part II
  # RETURN$coef$dilate2
  # RETURN$coef$MFA2
  # RETURN$coef$alpha2
  # RETURN$data$GrandCompromise
  #OverWeight
  # RETURN$data$OverWeighted_CP_array
  # RETURN$data$OverWeighted_GroupCompromise_array

  return(RETURN)
}
