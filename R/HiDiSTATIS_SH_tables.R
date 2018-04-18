#HiDiSTATIS_SH_tables.R
#
#'Split Half cross-validation for the tables of HiDiSTATIS
#'
#'@param input Items input to HiDiSTATIS
#'@param Hierarchy_of_tables Results related to computing Group and Grand Compromises
#'@param res_GrandComp Results from decomposing the Grand Compromise
#'@param niter the number of iterations
#'@return Prediction_Random
#'@export

HiDiSTATIS_SH_tables <- function(input = NULL, Hierarchy_of_tables = NULL, res_GrandComp = NULL, niter = 1){

  #Pick tables to split
  Leave_out_these_tables <- Pick_tables_to_split(DESIGN_tables = input$DESIGN_tables, niter = niter)
  total_iter <- ncol(Leave_out_these_tables)

  #Initialize DESIGN for the LeftOut tables
  DESIGN_tables_LeftOut <- list()
  DESIGN_tables_LeftOut$mat <- NA
  DESIGN_tables_LeftOut$CD_out <- nrow(Leave_out_these_tables)

  #Initialize DESIGN for the LeftIn analysis
  DESIGN_tables_LeftIn <- list()
  DESIGN_tables_LeftIn$mat <- NA
  DESIGN_tables_LeftIn$CD_out <- nrow(Leave_out_these_tables)
  DESIGN_tables_LeftIn$CD_in  <- input$DESIGN_tables$CD - DESIGN_tables_LeftIn$CD_out
  DESIGN_tables_LeftIn$D      <- input$DESIGN_tables$D
  DESIGN_tables_LeftIn$colors_CD_LeftIn <- NA





  #Initialize objects to RETURN
  Normed_LeftOut_CP_array               <- array(NA, dim=c(dim(input$CP_array)[c(1,2)], DESIGN_tables_LeftIn$CD_out))
  ###If Abdi-approved to set alpha2_supp to mean(alpha2), can delete these
  # RV_D_supp                             <- array(NA, dim=c(nrow(Leave_out_these_tables), input$DESIGN_tables$D, total_iter))
  # # rownames(RV_D_supp) <- paste0("Table ", Leave_out_these_tables[,i], " LeftOut from Group ", input$DESIGN_tables$labels[apply(input$DESIGN_tables$mat[Leave_out_these_tables,], 1, which.max)])
  # # colnames(RV_D_supp) <- dimnames(Hierarchy_of_tables$data$NormedGroupCompromise_array)[[3]]
  alpha2_supp                           <- matrix(NA, DESIGN_tables_LeftIn$CD_out, total_iter)
  OverWeighted_LeftOut_tables           <- array(NA, dim=c(dim(input$CP_array)[c(1,2)], DESIGN_tables_LeftIn$CD_out))
  Dev2_out_2_D                          <- array(NA, dim=c(DESIGN_tables_LeftIn$CD_out, input$DESIGN_tables$D, input$DESIGN_rows$AB))
  Prediction_array                      <- array(0, dim=c(input$DESIGN_rows$AB, input$DESIGN_tables$CD, input$DESIGN_tables$D, total_iter),
                                                 dimnames=list(rownames(input$DESIGN_rows$mat), rownames(input$DESIGN_tables$mat), input$DESIGN_tables$labels, paste0('iter ', 1:total_iter)))
  Fhat_LeftOut_Tables                   <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_GrandComp$eig$Lambda_vec), input$DESIGN_tables$CD, total_iter))
  Fhat_LeftOut_Tables_Cond              <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_GrandComp$eig$Lambda_vec), DESIGN_tables_LeftIn$CD_out, total_iter))
  Pb_Fhat_LeftOut_Tables_Cond           <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_GrandComp$eig$Lambda_vec), DESIGN_tables_LeftIn$CD_out, total_iter))





  #for each iteration...
  for(i in 1:total_iter){

    ### Analyze the LeftIn tables ###
    ###
    #Define the array of LeftIn tables
    CP_array_LeftIn  <- input$CP_array[,,-Leave_out_these_tables[,i]]

    #Create DESIGN matrix for the LeftIn tables
    #NOTE: these can be in a different order each iteration, so need their own DESIGN_mat for each iteration
    DESIGN_tables_LeftIn$mat <- input$DESIGN_tables$mat[-Leave_out_these_tables[,i],]

    #Using only the LeftIn tables, compute Group and Grand Compromises
    Hierarchy_of_tables_LeftIn <- GetGrandCompromise(CP_array = CP_array_LeftIn, DESIGN_tables = DESIGN_tables_LeftIn)

    #Decompose LeftIn_GrandCompromise, and project in the LeftIn tables
    res_GrandComp_LeftIn <- EigenHiDiSTATIS(Hierarchy_of_tables_LeftIn, DESIGN_tables_LeftIn, n2k=n2k)







    ### Project LeftOut tables into the LeftIn GrandCompromise ###
    ###
    #Define an array of the LeftOut tables
    CP_array_LeftOut <- input$CP_array[,,Leave_out_these_tables[,i]]

    #Create DESIGN matrix for the LeftOut tables
    #NOTE: Each iteration selects a new subset of the tables from each group.
    #      However, it always grabs the same number from each group, and orders them by group,
    #      so, the LeftOut DESIGN_mat is the same for each iteration
    DESIGN_tables_LeftOut$mat     <- input$DESIGN_tables$mat[Leave_out_these_tables[,i],]
    DESIGN_tables_LeftOut$Pd_Cond <- Bary_Projector_Cond(DESIGN_tables_LeftOut$mat)

    #Each LeftOut table needs to be MFA-normed
    MFA2_supp <- MFAnormCPFinder(CP_array_LeftOut)

    #And each LeftOut table is given an alpha2_supp equal to 1/D
    alpha2_supp[,i] <- mean(Hierarchy_of_tables$coef$alpha2)

    #Then, compute the array of Overweighted LeftOut tables
    #    ... and project them into the LeftIn Grand Compromise
    F_LeftOut_Tables <- array(NA, dim=c(dim(res_GrandComp_LeftIn$eig$ProjMat),
                                        DESIGN_tables_LeftIn$CD_out),
                              dimnames=list(rownames(res_GrandComp_LeftIn$eig$ProjMat),
                                            colnames(res_GrandComp_LeftIn$eig$ProjMat),
                                            paste0("LeftOut Table #", 1:DESIGN_tables_LeftIn$CD_out)))
    for(cd_out in 1:DESIGN_tables_LeftIn$CD_out){
      OverWeighted_LeftOut_tables[,,cd_out] <- CP_array_LeftOut[,,cd_out] * Hierarchy_of_tables$coef$dilate2 * MFA2_supp[cd_out] * alpha2_supp[cd_out,i]
      F_LeftOut_Tables[,,cd_out] <- OverWeighted_LeftOut_tables[,,cd_out] %*% res_GrandComp_LeftIn$eig$ProjMat
    }









    ### In the LeftIn space, loop through each stimulus, and loop through each LeftOut table...
    ### and identify which LeftIn Group is closest to that LeftOut table. ###

    #For each ab stimulus, compute the dev2 (sq dev) from each LeftOut table to each LeftIn Group
    for(ab in 1:input$DESIGN_rows$AB){#here, put the ab stimuli on the 3rd dimension (of the array of Dev2)
      Dev2_out_2_D[,,ab] <- Dev2(t(F_LeftOut_Tables[ab,,]), t(res_GrandComp_LeftIn$ProjGroup$F[ab,,]))
      closest_to <- apply(Dev2_out_2_D[,,ab], 1, which.min)

      #For this iteration, go to the corresponding table of Prediction_array, and...
      #place a 1 at the intersection of each left_out table and its closest/predicted category
      for(cd_out in 1:nrow(Leave_out_these_tables)){
        Prediction_array[ab, Leave_out_these_tables[cd_out,i], closest_to[cd_out], i] <- 1
      }
    }






    ### Part 2: To visualize the LOO results,
    #I want to show one stimulus at a time, with it's Group Compromise projections
    #and then double-project the LeftOut tables.
    #In other words, take the F_LeftOut_Tables (from their LeftIn spaces)
    #and project them a second time, into the Original Grand Compromise space.
    #This will provide a map of a single stimulus, with it's D Group PFSs,
    #and a 95% prediction interval around each D.
    # These intervals answer: for this stimulus, how well can the model predict the group membership of a new participant?
    ### I think I can also collapse this across stimuli, if I work with the distances relative to each stimulus
    # Of note, if the row_hypothesis is relevant to the table_hypothesis, then this answer should be better in DiDiSTATIS than in HiDiSTATIS.
    for(cd_out in 1:DESIGN_tables_LeftIn$CD_out){
      Fhat_LeftOut_Tables[,,Leave_out_these_tables[cd_out,i],i] <- F_LeftOut_Tables[,,cd_out] %*% t(res_GrandComp_LeftIn$eig$U) %*% res_GrandComp$eig$ProjMat

      #Fhat_LeftOut_Tables can be used to explore a single stimulus.
      #In most cases, more useful to remove all of the empty spaces in this array, and stop tracking which stimulus was projected
      Fhat_LeftOut_Tables_Cond[,,cd_out,i]                      <- F_LeftOut_Tables[,,cd_out] %*% t(res_GrandComp_LeftIn$eig$U) %*% res_GrandComp$eig$ProjMat
      Pb_Fhat_LeftOut_Tables_Cond[,,cd_out,i]                   <- input$DESIGN_rows$Pb %*% Fhat_LeftOut_Tables_Cond[,,cd_out,i]
    }

  }#end LOO total_iter






  DESIGN_tables_LeftIn$colors_CD_LeftIn <- input$DESIGN_tables$colors_CD[-Leave_out_these_tables[,niter]]

  DESIGN_tables_LeftOut$Cd_out <- colSums(DESIGN_tables_LeftOut$mat)
  DESIGN_tables_LeftOut$colors_CD_LeftOut <- input$DESIGN_tables$colors_CD[Leave_out_these_tables[,niter]]







  #The confusion matrix is computed from the distances in the barycentric sub-space of the LeftIn_Rows
  ##First, sum across the tables of Prediction_array, to show the cumulative results of the LOO_rows...
  Prediction_array_sum <- apply(Prediction_array, c(1,2,3), sum)

  #We don't really care about the difference in the number of times each stimulus was sampled, so divide each row by its sum
  Prediction_array_rownorm <- array(NA, dim=dim(Prediction_array_sum))
  for(ab in 1:nrow(Prediction_array_sum)){
    Prediction_array_rownorm[ab,,] <- Prediction_array_sum[ab,,] / rowSums(Prediction_array_sum[ab,,])
  }

  Prediction_mat_rownorm <- apply(Prediction_array_rownorm, c(2,3), mean)
  # dimnames(Prediction_mat_rownorm) <- list(rownames())

  #Transform the design matrix to give a confusion matrix
  Confusion_rand <- t(input$DESIGN_tables$mat) %*% Prediction_mat_rownorm
  rownames(Confusion_rand) <- paste0(input$DESIGN_tables$labels, "_actual")

  Confusion_rand_norm <- round(Confusion_rand / rowSums(Confusion_rand), 2) *100










  returnME <- list()
  returnME$Leave_out_these_tables      <- Leave_out_these_tables
  returnME$DESIGN_tables_LeftOut       <- DESIGN_tables_LeftOut
  returnME$alpha2_supp                 <- alpha2_supp
  returnME$Prediction_array            <- Prediction_array
  returnME$Prediction_array_sum        <- Prediction_array_sum
  returnME$Prediction_mat_rownorm      <- Prediction_mat_rownorm
  returnME$Confusion_rand              <- Confusion_rand
  returnME$Confusion_rand_norm         <- Confusion_rand_norm
  returnME$Fhat_LeftOut_Tables         <- Fhat_LeftOut_Tables
  returnME$Fhat_LeftOut_Tables_Cond    <- Fhat_LeftOut_Tables_Cond
  returnME$Pb_Fhat_LeftOut_Tables_Cond <- Pb_Fhat_LeftOut_Tables_Cond
  returnME$Last_iter$DESIGN_tables_LeftIn <- DESIGN_tables_LeftIn
  returnME$Last_iter$res_GrandComp_LeftIn <- res_GrandComp_LeftIn
  returnME$Last_iter$F_LeftOut_Tables     <- F_LeftOut_Tables

  return(returnME)

}

