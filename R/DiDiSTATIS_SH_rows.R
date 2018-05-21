#DiDiSTATIS_SH_rows.R
#
#'Leave One Out cross-validation for the rows for DiDiSTATIS
#'
#'@param input A list of things input to DiDiSTATIS
#'@param Hierarchy_of_tables Output of GetBaryGrandComp()
#'@param res_BaryGrand Output of EigenDiDiSTATIS()
#'@param niter Number of iterations
#'@return A list of the SH results
#'@export

DiDiSTATIS_SH_rows <- function(input, Hierarchy_of_tables, res_BaryGrand, niter = 100){

  #rename for ease
  DESIGN_rows   <- input$DESIGN_rows
  DESIGN_tables <- input$DESIGN_tables

  #Pick rows to leave out
  Leave_out_these_rows <- Pick_rows_to_split(DESIGN_rows, niter = niter)
  total_iter <- ncol(Leave_out_these_rows)

  #Initialize DESIGN for the LeftOut tables
  DESIGN_rows_LeftOut <- list()
  DESIGN_rows_LeftOut$mat <- NA
  DESIGN_rows_LeftOut$AB_out <- nrow(Leave_out_these_rows)

  #Initialize DESIGN for the LeftIn analysis
  DESIGN_rows_LeftIn <- list()
  DESIGN_rows_LeftIn$labels <- DESIGN_rows$labels
  DESIGN_rows_LeftIn$mat <- NA
  DESIGN_rows_LeftIn$AB_out <- nrow(Leave_out_these_rows)
  DESIGN_rows_LeftIn$AB_in  <- DESIGN_rows$AB - DESIGN_rows_LeftIn$AB_out
  DESIGN_rows_LeftIn$B      <- DESIGN_rows$B
  DESIGN_rows_LeftIn$colors_AB_LeftIn <- NA

  ########################################################
  ### Predict stimulus categories for the grand compromise
  #Initialize...
  CP_array_LeftIn  <- array(NA, dim=c(DESIGN_rows_LeftIn$AB_in, DESIGN_rows_LeftIn$AB_in, DESIGN_tables$CD))
  CP_array_LeftOut <- array(NA, dim=c(DESIGN_rows_LeftIn$AB_out, DESIGN_rows_LeftIn$AB_in, DESIGN_tables$CD))

  #Factor scores for the B LeftOut_Rows in the Lb dimensions of the LeftIn barycentric sub-space
  # Fdisc_LeftOut_Rows_Full    <- array(NA, dim=c(DESIGN_rows$B, DESIGN_rows$B, total_iter),
  #                                     dimnames = list(paste0(DESIGN_rows$labels, "_out"), paste0('Comp ', 1:DESIGN_rows$B), paste0('iter ',1:total_iter)))
  #
  # Fdisc_LeftOut_Rows_Cond    <- array(NA, dim=c(DESIGN_rows$B, DESIGN_rows$B, total_iter),
  #                                     dimnames = list(paste0(DESIGN_rows$labels, "_out"), paste0('Comp ', 1:DESIGN_rows$B), paste0('iter ',1:total_iter)))

  OverWeighted_LeftOut_tables <- array(NA, dim=c(DESIGN_rows_LeftIn$AB_out, DESIGN_rows_LeftIn$AB_in, DESIGN_tables$CD))

  Prediction_array   <- array(0, dim=c(DESIGN_rows$AB, DESIGN_rows$B, total_iter),
                              dimnames = list(paste0(rownames(DESIGN_rows$mat), "_out"), paste0(DESIGN_rows$labels, "_predicted"), paste0('iter ',1:total_iter)))

  Overweighted_LeftOut_Group_tables <- array(NA, dim=c(DESIGN_rows_LeftIn$AB_out, DESIGN_rows_LeftIn$AB_in, DESIGN_tables$D))

  Prediction_array_D  <- array(0, dim=c(DESIGN_rows$AB, DESIGN_rows$B, DESIGN_tables$D, total_iter),
                               dimnames=list(rownames(DESIGN_rows$mat), colnames(DESIGN_rows$mat), colnames(DESIGN_tables$mat), paste0('iter ', 1:total_iter)))




  # Fhatdisc_LeftOut_Rows_Full <- array(NA, dim=c(DESIGN_rows$B, ncol(res_Disc_Full$eig$Fb_Full), total_iter),
  #                                     dimnames = list(paste0(DESIGN_rows$labels, "_out"), colnames(res_Disc_Full$eig$Fb_Full), paste0('iter ',1:total_iter)))
  #
  # Fhatdisc_LeftOut_Rows_Cond <- array(NA, dim=c(DESIGN_rows$B, ncol(res_Disc_Full$eig$Fb_Full), total_iter),
  #                                     dimnames = list(paste0(DESIGN_rows$labels, "_out"), colnames(res_Disc_Full$eig$Fb_Full), paste0('iter ',1:total_iter)))


  #for each iteration...
  for(i in 1:total_iter){

    ### Analyze the LeftIn tables ###
    #Define the array of LeftIn rows
    for(cd in 1:DESIGN_tables$CD){
      CP_array_LeftIn[,,cd]  <- input$CP_array[-Leave_out_these_rows[,i], -Leave_out_these_rows[,i], cd]
      CP_array_LeftOut[,,cd] <- input$CP_array[Leave_out_these_rows[,i], -Leave_out_these_rows[,i], cd]
    }


    # CP_LeftOut_Rows <- input$CP[Leave_out_these_rows[,i],  -Leave_out_these_rows[,i]]

    #Create DESIGN matrix for the LeftIn analysis
    #NOTE: these can be in a different order each iteration, so need their own DESIGN_mat for each iteration
    DESIGN_rows_LeftIn$mat   <- DESIGN_rows$mat[-Leave_out_these_rows[,i],]
    DESIGN_rows_LeftIn$colors_AB_LeftIn <- DESIGN_rows$colors_AB[-Leave_out_these_rows[,i]]
    DESIGN_rows_LeftIn$Pb_Full      <- Bary_Projector(DESIGN_rows_LeftIn$mat)
    DESIGN_rows_LeftIn$Pb_Cond      <- Bary_Projector_Cond(DESIGN_rows_LeftIn$mat)

    #compute bary grand comp for LeftIn
    Hierarchy_of_tables_LeftIn <- GetBaryGrandComp(CP_array = CP_array_LeftIn,
                                                   DESIGN_rows = DESIGN_rows_LeftIn,
                                                   DESIGN_tables = DESIGN_tables,
                                                   MFA1_Flag = input$MFA1_Flag,
                                                   RV1_Flag = input$RV1_Flag,
                                                   MFA2_Flag = input$MFA2_Flag,
                                                   RV2_Flag = input$RV2_Flag)


    #Decompose BaryGrandComp_LeftIn
    res_BaryGrand_LeftIn <- EigenCP(CP = Hierarchy_of_tables_LeftIn$data$Bary_GrandCompromise)
    names(res_BaryGrand_LeftIn$input) <- c("Bary_GrandCompromise")
    names(res_BaryGrand_LeftIn$eig)   <- c("Ub..", "Lambdab.._vec", "Lambdab..", "ProjMatb..",
                                           "tb..", "Fb..", "Ctrbb..")

    res_BaryGrand_LeftIn$eig$Fb..Cond <- DESIGN_rows_LeftIn$Pb_Cond %*% res_BaryGrand_LeftIn$eig$Fb..

    #And project groups
    res_BaryGrand_LeftIn$Proj_B.D$F_B.D_Cond <- array(NA, dim=c(dim(res_BaryGrand_LeftIn$eig$Fb..Cond), DESIGN_tables$D))
    for(d in 1:DESIGN_tables$D){
      res_BaryGrand_LeftIn$Proj_B.D$F_B.D_Cond[,,d] <- DESIGN_rows_LeftIn$Pb_Cond %*% Hierarchy_of_tables_LeftIn$data$OverWeighted_Pb_GroupCompromise_Pb_array[,,d] %*% res_BaryGrand_LeftIn$eig$ProjMatb..

    }





    #Work on the LeftOut rows ####

    #Integrate these CD rectangular (AB_out * AB_in) tables into a single table
    for(CD in 1:DESIGN_tables$CD){
      which_group <- which(DESIGN_tables$mat[CD,]==1)
      OverWeighted_LeftOut_tables[,,CD] <- (CP_array_LeftOut[,,CD] *
                                            Hierarchy_of_tables_LeftIn$coef$dilate1 *
                                            Hierarchy_of_tables_LeftIn$coef$MFA1[CD] *
                                            Hierarchy_of_tables_LeftIn$coef$alpha1[CD] *
                                            Hierarchy_of_tables_LeftIn$coef$dilate2 *
                                            Hierarchy_of_tables_LeftIn$coef$MFA2[which_group] *
                                            Hierarchy_of_tables_LeftIn$coef$alpha2[which_group])


      # F_LeftOut_Tables[,,cd_out] <- OverWeighted_LeftOut_tables[,,cd_out] %*% res_GrandComp_LeftIn$eig$ProjMat
    }

    LeftOut_BaryGrandComp <- apply(OverWeighted_LeftOut_tables, c(1,2), mean)

    F_LeftOut_Rows <- LeftOut_BaryGrandComp %*% res_BaryGrand_LeftIn$eig$ProjMat


    # prettyPlot(F_LeftOut_Rows, col = DESIGN_rows$colors_B)
    # prettyPlot(res_BaryGrand_LeftIn$eig$Fb..Cond, col = DESIGN_rows$colors_B, dev.new = F, new.plot = F)


    ###Get the random confusion matrix
    #In the barycentric sub-space for the LeftIn_Rows...
    #Compute d2 from the left_out rows/stimuli to all categories (to give an out x B matrix)
    Dev2_out_2_B <- Dev2(F_LeftOut_Rows, res_BaryGrand_LeftIn$eig$Fb..Cond)

    #Assign left_out rows/stimuli to the nearest category (identify which B_in is closest to each left_out stimulus)
    closest_to <- apply(Dev2_out_2_B, 1, which.min)

    #For this iteration, go to the corresponding table of Prediction_array, and...
    #place a 1 at the intersection of each left_out row/stimulus and its closest/predicted category
    for(j in 1:nrow(Leave_out_these_rows)){
      Prediction_array[Leave_out_these_rows[j,i],closest_to[j],i] <- 1
    }






    #And onto the prediction_array_d, for the groups' perspectives
    F_D_LeftOut_Rows <- array(NA, dim=c(DESIGN_rows_LeftIn$AB_out, ncol(res_BaryGrand_LeftIn$eig$ProjMat), DESIGN_tables$D))

    for(d in 1:DESIGN_tables$D){
      which_participants <- which(DESIGN_tables$mat[,d]==1)
      Overweighted_LeftOut_Group_tables[,,d] <- apply(OverWeighted_LeftOut_tables[,,which_participants], c(1,2), mean)

      F_D_LeftOut_Rows[,,d] <- Overweighted_LeftOut_Group_tables[,,d] %*% res_BaryGrand_LeftIn$eig$ProjMat


      # Dev2_out_2_B <- Dev2(F_D_LeftOut_Rows[,,d], res_BaryGrand_LeftIn$Proj_B.D$F_B.D_Cond[,,d])
      Dev2_out_2_B <- Dev2(F_D_LeftOut_Rows[,,d], res_BaryGrand_LeftIn$eig$Fb..Cond)

      #Assign left_out rows/stimuli to the nearest category (identify which B_in is closest to each left_out stimulus)
      closest_to <- apply(Dev2_out_2_B, 1, which.min)

      #For this iteration, go to the corresponding table of Prediction_array, and...
      #place a 1 at the intersection of each left_out row/stimulus and its closest/predicted category
      for(j in 1:nrow(Leave_out_these_rows)){
        Prediction_array_D[Leave_out_these_rows[j,i],closest_to[j],d,i] <- 1
      }


    }
  }


  Prediction_array_sum <- apply(Prediction_array, c(1,2), sum)
  Confusion_rand <- t(DESIGN_rows$mat) %*% Prediction_array_sum
  rownames(Confusion_rand) <- paste0(DESIGN_rows$labels, "_actual")

  Confusion_rand_norm <- round(Confusion_rand / rowSums(Confusion_rand), 2) *100






  Prediction_array_D_sum <- apply(Prediction_array_D, c(1,2,3), sum)
  Confusion_rand_D      <- array(0, dim=c(DESIGN_rows$B, DESIGN_rows$B, DESIGN_tables$D))
  dimnames(Confusion_rand_D) <- list(paste0(DESIGN_rows$labels, "_actual"),
                                     paste0(DESIGN_rows$labels, "_predicted"),
                                     DESIGN_tables$labels)
  Confusion_rand_D_norm <- array(0, dim=c(DESIGN_rows$B, DESIGN_rows$B, DESIGN_tables$D))
  dimnames(Confusion_rand_D_norm) <- list(paste0(DESIGN_rows$labels, "_actual"),
                                          paste0(DESIGN_rows$labels, "_predicted"),
                                          DESIGN_tables$labels)


  for(d in 1:DESIGN_tables$D){
    Confusion_rand_D[,,d]    <- t(DESIGN_rows$mat) %*% Prediction_array_D_sum[,,d]
    Confusion_rand_D_norm[,,d] <- round(Confusion_rand_D[,,d] / rowSums(Confusion_rand_D[,,d]), 2) *100
  }










  returnME <- list()
  returnME$DESIGN_rows_LeftIn           <- DESIGN_rows_LeftIn
  returnME$Leave_out_these_rows         <- Leave_out_these_rows
  returnME$Grand$Prediction_array       <- Prediction_array
  returnME$Grand$Prediction_array_sum   <- Prediction_array_sum
  returnME$Grand$Confusion_rand         <- Confusion_rand
  returnME$Grand$Confusion_rand_norm    <- Confusion_rand_norm
  returnME$Group$Prediction_array_D     <- Prediction_array_D
  returnME$Group$Prediction_array_D_sum <- Prediction_array_D_sum
  returnME$Group$Confusion_rand_D       <- Confusion_rand_D
  returnME$Group$Confusion_rand_D_norm  <- Confusion_rand_D_norm



  return(returnME)

}

