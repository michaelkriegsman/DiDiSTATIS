#DiMDS_LOO_rows.R
#
#'Leave One Out cross-validation for the rows for DiMDS
#'
#'@param input Items input to DiMDS
#'@param res_Disc_Full Output from decomposing the barycentric discriminant sub-space
#'@param multiplier to increase the number of iterations
#'@return Prediction_Random
#'@export

DiMDS_LOO_rows <- function(input = NULL, res_Disc_Full = NULL, multiplier = 1){

  DESIGN_rows <- input$DESIGN_rows

  #Pick rows to leave out
  Leave_out_these_rows <- Pick_rows_to_leave_out(DESIGN_rows, multiplier = multiplier)
  total_iter <- ncol(Leave_out_these_rows)

  #Initialize...
  #Factor scores for the B LeftOut_Rows in the Lb dimensions of the LeftIn barycentric sub-space
  Fdisc_LeftOut_Rows_Full    <- array(NA, dim=c(DESIGN_rows$B, DESIGN_rows$B, total_iter),
                                      dimnames = list(paste0(DESIGN_rows$labels, "_out"), paste0('Comp ', 1:DESIGN_rows$B), paste0('iter ',1:total_iter)))

  Prediction_array   <- array(0, dim=c(DESIGN_rows$AB, DESIGN_rows$B, total_iter),
                              dimnames = list(paste0(rownames(DESIGN_rows$mat), "_out"), paste0(DESIGN_rows$labels, "_predicted"), paste0('iter ',1:total_iter)))

  # removed feature 4.2.5.2018
  # Fhatdisc_LeftOut_Rows_Full <- array(NA, dim=c(DESIGN_rows$B, ncol(res_Disc_Full$eig$Fb_Full), total_iter),
  #                                     dimnames = list(paste0(DESIGN_rows$labels, "_out"), colnames(res_Disc_Full$eig$Fb_Full), paste0('iter ',1:total_iter)))



  #For each iteration, ####
  for(i in 1:total_iter){
    # ** separate testing and training data ####
    #Create the LeftIn_Rows and LeftOut_Rows data
    CP_LeftIn_Rows  <- input$CP[-Leave_out_these_rows[,i], -Leave_out_these_rows[,i]]
    CP_LeftOut_Rows <- input$CP[Leave_out_these_rows[,i],  -Leave_out_these_rows[,i]]

    # ** create training DESIGN ####
    DESIGN_Rows_LeftIn <- list()
    DESIGN_Rows_LeftIn$mat   <- DESIGN_rows$mat[-Leave_out_these_rows[,i],]
    DESIGN_Rows_LeftIn$AB_out <- nrow(Leave_out_these_rows)
    DESIGN_Rows_LeftIn$AB_in  <- input$DESIGN_rows$AB - DESIGN_Rows_LeftIn$AB_out
    DESIGN_Rows_LeftIn$colors_AB_LeftIn <- DESIGN_rows$colors_AB[-Leave_out_these_rows[,i]]
    DESIGN_Rows_LeftIn$Pb_Full      <- Bary_Projector(DESIGN_Rows_LeftIn$mat)
    DESIGN_Rows_LeftIn$Pb_Cond      <- Bary_Projector_Cond(DESIGN_Rows_LeftIn$mat)
    DESIGN_Rows_LeftIn$BintBin      <- t(DESIGN_Rows_LeftIn$mat) %*% DESIGN_Rows_LeftIn$mat


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
    res_LeftIn_Rows_Disc_Full$proj2Bary$Fdisc   <- CP_LeftIn_Rows %*% DESIGN_Rows_LeftIn$Pb_Full %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows
    # equivalently:                                CP_LeftIn_Rows %*%                                res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows


    # ** project testing into trained space ####
    # #Project the LeftOut Rows into the LeftIn_Rows barycentric sub-space
    Fdisc_LeftOut_Rows_Full[,,i] <- CP_LeftOut_Rows %*% res_LeftIn_Rows_Disc_Full$eig$ProjMatb_Full_LeftIn_Rows


    # ** and double-project THAT into the fixed space #removed feature 4.2.5.2018
    # #Project LeftOut Rows from their LeftIn_Rows space to the real barycentric space
    # #This is for visualizing, as prediction intervals.
    # #...so I don't need to store exactly which stimuli were left out
    # Fhatdisc_LeftOut_Rows_Full[,,i] <- (Fdisc_LeftOut_Rows_Full[,,i] %*% #The Factor Scores of LeftOut stimuli, times...
    #                                     t(res_LeftIn_Rows_Disc_Full$eig$Ub_Full_LeftIn_Rows) %*% #t(U_LeftIn), as in Abdi 2018 BADA
    #                                     t(DESIGN_Rows_LeftIn$Pb_Cond) %*% #times Pb_Cond_LeftIn, to transform from 33 LeftIn stimuli to B=3 categories
    #                                     DESIGN_Rows_LeftIn$BintBin %*% #times the number of LeftIn stimuli in each group (a needed scaling factor, for reducing from 33 to 3 columns)
    #                                     input$DESIGN_rows$Pb_Cond %*% #times Pb_Cond, to transform from B=3 categories to AB=36 stimuli of the fixed effects factor scores
    #                                     res_Disc_Full$eig$ProjMatb_Full) #times ProjMat, to project that mess into the fixed effects barycentric space.

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

  }#end LOO total_iter

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
  returnME$Leave_out_these_rows <- Leave_out_these_rows
  returnME$Fdisc_LeftOut_Rows_Full      <- Fdisc_LeftOut_Rows_Full
  returnME$Prediction_mat_raw   <- Prediction_mat_raw
  returnME$Prediction_mat       <- Prediction_mat
  returnME$Confusion_rand       <- Confusion_rand
  returnME$Confusion_rand_norm  <- Confusion_rand_norm
  # returnME$Fhatdisc_LeftOut_Rows_Full    <- Fhatdisc_LeftOut_Rows_Full #removed feature 4.2.5.2018

  return(returnME)

}

