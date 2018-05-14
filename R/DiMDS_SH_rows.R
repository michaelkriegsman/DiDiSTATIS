#DiMDS_SH_rows.R
#
#'Split Half cross-validation for the rows for DiMDS
#'
#'@param input Items input to DiMDS
#'@param res_Disc_Full Output from decomposing the barycentric discriminant sub-space
#'@param niter The number of iterations
#'@return Prediction_Random
#'@export

DiMDS_SH_rows <- function(input = NULL, res_Disc_Full = NULL, niter = 1){

  DESIGN_rows <- input$DESIGN_rows

  #Pick rows to leave out
  Leave_out_these_rows <- Pick_rows_to_split(DESIGN_rows, niter = niter)
  total_iter <- ncol(Leave_out_these_rows)

  #Initialize...
  #Factor scores for the B LeftOut_Rows in the Lb dimensions of the LeftIn barycentric sub-space
  Fdisc_LeftOut_Rows_Full    <- array(NA, dim=c(nrow(Leave_out_these_rows), DESIGN_rows$B, total_iter),
                                      dimnames = list(rep(paste0(DESIGN_rows$labels, "_out"), each=nrow(Leave_out_these_rows) / DESIGN_rows$B), paste0('Comp ', 1:DESIGN_rows$B), paste0('iter ',1:total_iter)))

  Prediction_array   <- array(0, dim=c(DESIGN_rows$AB, DESIGN_rows$B, total_iter),
                              dimnames = list(paste0(rownames(DESIGN_rows$mat), "_out"), paste0(DESIGN_rows$labels, "_predicted"), paste0('iter ',1:total_iter)))

  # removed feature 5.01.2018
  # Fhatdisc_LeftOut_Rows_Full <- array(NA, dim=c(nrow(Leave_out_these_rows), ncol(res_Disc_Full$eig$Fb_Full), total_iter),
  #                                     dimnames = list(rep(paste0(DESIGN_rows$labels, "_out"), each=nrow(Leave_out_these_rows) / DESIGN_rows$B), colnames(res_Disc_Full$eig$Fb_Full), paste0('iter ',1:total_iter)))



  # For each iteration... ####
  for(i in 1:total_iter){
    # ** create LeftIn_Rows and LeftOut_Rows ####
    CP_LeftIn_Rows  <- input$CP[-Leave_out_these_rows[,i], -Leave_out_these_rows[,i]]
    CP_LeftOut_Rows <- input$CP[Leave_out_these_rows[,i],  -Leave_out_these_rows[,i]]

    DESIGN_Rows_LeftIn <- list()
    DESIGN_Rows_LeftIn$mat   <- DESIGN_rows$mat[-Leave_out_these_rows[,i],]
    DESIGN_Rows_LeftIn$AB_out <- nrow(Leave_out_these_rows)
    DESIGN_Rows_LeftIn$AB_in  <- input$DESIGN_rows$AB - DESIGN_Rows_LeftIn$AB_out
    DESIGN_Rows_LeftIn$colors_AB_LeftIn <- DESIGN_rows$colors_AB[-Leave_out_these_rows[,i]]
    DESIGN_Rows_LeftIn$Pb_Full      <- Bary_Projector(DESIGN_Rows_LeftIn$mat)
    DESIGN_Rows_LeftIn$Pb_Cond      <- Bary_Projector_Cond(DESIGN_Rows_LeftIn$mat)
    DESIGN_Rows_LeftIn$BintBin      <-      (t(DESIGN_Rows_LeftIn$mat) %*% DESIGN_Rows_LeftIn$mat)


    DESIGN_Rows_LeftOut <- list()
    DESIGN_Rows_LeftOut$mat     <- DESIGN_rows$mat[Leave_out_these_rows[,i],]
    DESIGN_Rows_LeftOut$Pb_Cond <- Bary_Projector_Cond(DESIGN_Rows_LeftOut$mat)



    # ** analyze training data ####
    #Eigen the barycenters of the LeftIn_Rows data
    res_LeftIn_Rows_Disc_Full <- EigenCP_Full(CP = DESIGN_Rows_LeftIn$Pb_Full %*% CP_LeftIn_Rows %*% t(DESIGN_Rows_LeftIn$Pb_Full),
                                              DESIGN_rows = DESIGN_Rows_LeftIn)

    names(res_LeftIn_Rows_Disc_Full$input) <- "Pb_CP_Pb_Full_LeftIn_Rows"
    names(res_LeftIn_Rows_Disc_Full$eig) <- c("Ub_Full_LeftIn_Rows", "Ub_Cond_LeftIn_Rows",
                                              "Lambdab_vec_LeftIn_Rows", "Lambdab_LeftIn_Rows",
                                              "ProjMatb_Full_LeftIn_Rows", "ProjMatb_Cond_LeftIn_Rows",
                                              "tb_LeftIn_Rows",
                                              "Fb_Full_LeftIn_Rows", "Fb_Cond_LeftIn_Rows",
                                              "Ctrbb_Full_LeftIn_Rows", "Ctrbb_Cond_LeftIn_Rows")



    res_LeftIn_Rows_Disc_Full$proj2Bary$Fb_Full <-                                res_LeftIn_Rows_Disc_Full$input$Pb_CP_Pb_Full_LeftIn_Rows         %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    res_LeftIn_Rows_Disc_Full$proj2Bary$Fb_Cond <- DESIGN_Rows_LeftIn$Pb_Cond %*% res_LeftIn_Rows_Disc_Full$proj2Bary$Fb_Full
    res_LeftIn_Rows_Disc_Full$proj2Bary$Fdisc   <- CP_LeftIn_Rows %*% DESIGN_Rows_LeftIn$Pb_Full    %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    # equivalently:                                CP_LeftIn_Rows %*%                                   res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    # equivalently:                                CP_LeftIn_Rows %*% t(DESIGN_Rows_LeftIn$Pb_Cond) %*% DESIGN_Rows_LeftIn$BintBin %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Cond_LeftIn_Rows


    # ** project testing into trained space ####
    # #Project the LeftOut Rows into the LeftIn_Rows barycentric sub-space
    Fdisc_LeftOut_Rows_Full[,,i] <- CP_LeftOut_Rows %*%                                res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    # equivalently:                 CP_LeftOut_Rows %*% DESIGN_Rows_LeftIn$Pb_Full %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    # equivalently:                 CP_LeftOut_Rows %*% t(DESIGN_Rows_LeftIn$Pb_Cond) %*% DESIGN_Rows_LeftIn$BintBin %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Cond_LeftIn_Rows

    #feature removed 5.01.2018
    # ** and double-project THAT into the fixed space
    # #Project LeftOut Rows from their LeftIn_Rows space to the real barycentric space
    # #This is just for visualizing, so I don't need to store exactly which stimuli were left out (thus, these rownames)
    # Fhatdisc_LeftOut_Rows_Full[,,i]    <- Fdisc_LeftOut_Rows_Full[,,i] %*% t(res_LeftIn_Rows_Disc_Full$eig$Ub_Full_LeftIn_Rows) %*% t(DESIGN_Rows_LeftIn$Pb_Cond) %*% DESIGN_Rows_LeftIn$BintBin      %*% input$DESIGN_rows$Pb_Cond %*% res_Disc_Full$eig$ProjMatb_Full

    # ** and predict categories of testing set ####
    ###Get the random confusion matrix
    #In the barycentric sub-space for the LeftIn_Rows...
    #Compute d2 from the left_out rows/stimuli to all categories (to give an out x B matrix)
    Dev2_out_2_B <- Dev2(Fdisc_LeftOut_Rows_Full[,,i], res_LeftIn_Rows_Disc_Full$proj2Bary$Fb_Cond)

    #Assign left_out rows/stimuli to the nearest category (identify which B_in is closest to each left_out stimulus)
    closest_to <- apply(Dev2_out_2_B, 1, which.min)

    #For this iteration, go to the corresponding table of Prediction_array, and...
    #place a 1 at the intersection of each left_out row/stimulus and its closest/predicted category
    for(j in 1:nrow(Leave_out_these_rows)){
      Prediction_array[Leave_out_these_rows[j,i],closest_to[j],i] <- 1
    }

  }#end SH total_iter

  # Then, ####
  #The confusion matrix is computed from the distances in the barycentric sub-space of the LeftIn_Rows
  ##First, sum across the tables of Prediction_array, to show the cumulative results of the LOO_rows...
  Prediction_mat_raw <- apply(Prediction_array, c(1,2), sum)

  #We don't really care about the difference in the number of times each stimulus was sampled, so divide each row by its sum
  Prediction_mat <- Prediction_mat_raw / rowSums(Prediction_mat_raw)
  Prediction_mat[is.nan(Prediction_mat)] <- 0

  # ** get the confusion matrix ####
  #Transform the design matrix to give a confusion matrix
  Confusion_rand <- t(DESIGN_rows$mat) %*% Prediction_mat
  rownames(Confusion_rand) <- paste0(DESIGN_rows$labels, "_actual")
  cbind(Confusion_rand, colSums(DESIGN_rows$mat))

  Confusion_rand_norm <- round(Confusion_rand / rowSums(Confusion_rand), 2) *100
  cbind(Confusion_rand_norm, rep(100, 3))



  returnME <- list()
  returnME$DESIGN_Rows_LeftIn <- DESIGN_Rows_LeftIn
  returnME$DESIGN_Rows_LeftOut <- DESIGN_Rows_LeftOut
  returnME$Leave_out_these_rows <- Leave_out_these_rows
  returnME$Fdisc_LeftOut_Rows_Full      <- Fdisc_LeftOut_Rows_Full
  returnME$Prediction_mat_raw   <- Prediction_mat_raw
  returnME$Prediction_mat       <- Prediction_mat
  returnME$Confusion_rand       <- Confusion_rand
  returnME$Confusion_rand_norm  <- Confusion_rand_norm
  # returnME$Fhatdisc_LeftOut_Rows_Full    <- Fhatdisc_LeftOut_Rows_Full #feature removed 5.01.2018

  return(returnME)

}

