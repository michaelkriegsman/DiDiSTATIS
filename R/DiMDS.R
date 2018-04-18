#The DiMDS function within DiDiSTATIS
#
#'Conduct barycentric discriminant metric multidimensional scaling
#'
#'@param DATA A distance-data matrix
#'@param data_are Flag to indicate data type #d, d2, CP, or X_to_cos, X_to_cov, X_to_cor
#'@param DESIGN_rows Column vector(s) to discriminate rows (nominal or colors or DESIGN)
#'@param n2k Number of components to keep
#'@param Perm_rows Boolean to conduct permutation test
#'@param Perm_rows_niter Number of permutation iterations
#'@param Boot_rows Boolean to conduct bootstrap resampling
#'@param Boot_rows_niter Number of bootstrap iterations
#'@param LOO_rows Boolean to conduct Leave One Out (LOO) cross-validation
#'@param LOO_rows_multiplier Increase the number of LOO iterations
#'@param SH_rows Boolean to conduct Split Half (SH) cross-validation
#'@param SH_rows_niter Number of SH iterations
#'@return Results of DiMDS
#'@export

DiMDS <- function(DATA, data_are, DESIGN_rows, n2k=NULL,
                  Perm_rows = F, Perm_rows_niter = 1000,
                  Boot_rows = F, Boot_rows_niter = 1000,
                  LOO_rows = F,  LOO_rows_multiplier = 10,
                  SH_rows = F,   SH_rows_niter = 50){


  input <- list()
  input$DATA <- DATA
  input$data_are <- data_are

  #Convert data to CP
  CP <- input$CP <- GetCP(DATA, data_are)
  input$SStotal <- sum(diag(CP))


  #Work on DESIGN_rows
  input$DESIGN_rows <- DESIGN_rows
  rm(DESIGN_rows)

  #Define the full projector matrices, for computing
  Pb_Full  <- input$DESIGN_rows$Pb_Full <- Bary_Projector(input$DESIGN_rows$mat)
  ### Pab_Full <- input$DESIGN_rows$Pab_Full <- Ortho_Projector(Pb_Full)

  ### #Define the compressed projector matrices, for plotting
  Pb_Cond  <- input$DESIGN_rows$Pb_Cond <- Bary_Projector_Cond(input$DESIGN_rows$mat)
  ### PbT <- input$DESIGN_rows$PbT <- t(Pb)

  Pb_Cond_Sqrt <- input$DESIGN_rows$Pb_Cond_Sqrt <- Bary_Projector_Cond_Sqrt(input$DESIGN_rows$mat)

  BtB      <- input$DESIGN_rows$BtB      <-      (t(input$DESIGN_rows$mat) %*% input$DESIGN_rows$mat)
  BtB_sqrt <- input$DESIGN_rows$BtB_sqrt <- sqrt((t(input$DESIGN_rows$mat) %*% input$DESIGN_rows$mat))

  Pb_Cond_Inv <- input$DESIGN_rows$mat %*% sqrt(t(input$DESIGN_rows$mat) %*% input$DESIGN_rows$mat)

  #Define (and compute on) the barycentric sub-space
  Pb_CP_Pb_Full <- input$Pb_CP_Pb_Full <- Pb_Full %*% CP %*% Pb_Full
  input$SSb_Full       <- sum(diag(Pb_CP_Pb_Full))
  input$r2total.b_Full <- input$SSb_Full / input$SStotal









  #Decompose the barycentric sub-space
  res_Disc_Full <- EigenCP_Full(CP = Pb_CP_Pb_Full, DESIGN_rows = input$DESIGN_rows)
  names(res_Disc_Full$input) <- "Pb_CP_Pb_Full"
  names(res_Disc_Full$eig) <- c("Ub_Full", "Ub_Cond", "Ub_Cond_Sqrt", "Lambdab_vec", "Lambdab", "ProjMatb_Full",
                                "ProjMatb_Cond", "ProjMatb_Cond_Sqrt", "tb", "Fb_Full", "Fb_Cond", "Fb_Cond_Sqrt",
                                "Ctrbb_Full", "Ctrbb_Cond")

  #Project the various segments of the data into the barycentric sub-sapce
  res_Disc_Full$proj2Bary$Fb_Full         <- Pb_CP_Pb_Full         %*% res_Disc_Full$eig$ProjMatb_Full
  res_Disc_Full$proj2Bary$Fb_condensed    <- Condense_Rows(res_Disc_Full$proj2Bary$Fb_Full, input$DESIGN_rows$mat)
  res_Disc_Full$proj2Bary$Fb_condensed_v2 <- Pb_Cond_Sqrt %*% CP %*% t(Pb_Cond_Sqrt) %*% res_Disc_Full$eig$ProjMatb_Cond
  res_Disc_Full$proj2Bary$Fdisc    <- CP %*% Pb_Full %*% res_Disc_Full$eig$ProjMatb_Full
  # res_Disc_Full$proj2Bary$Fdisc_v2 <- CP %*% t(Pb_Cond_Sqrt) %*% res_Disc_Full$eig$ProjMatb_Cond
  # res_Disc_Full$proj2Bary$Fdisc_v3 <- CP %*% t(Pb_Cond) %*% res_Disc_Full$eig$ProjMatb_Cond_Sqrt
  res_Disc_Full$proj2Bary$Fab   <- res_Disc_Full$proj2Bary$Fdisc - res_Disc_Full$proj2Bary$Fb_Full

  #... and compute the associated summary statistics
  res_Disc_Full$proj2Bary$SSb       <- SS_from_F(res_Disc_Full$proj2Bary$Fb_Full)
  res_Disc_Full$proj2Bary$SSb_v2    <- SS_from_F(input$DESIGN_rows$Pb_Cond_Sqrt %*% res_Disc_Full$proj2Bary$Fb_Full)
  res_Disc_Full$proj2Bary$SSdisc <- SS_from_F(res_Disc_Full$proj2Bary$Fdisc)
  res_Disc_Full$proj2Bary$SSab   <- SS_from_F(res_Disc_Full$proj2Bary$Fab)

  res_Disc_Full$proj2Bary$SSe <- input$SStotal - res_Disc_Full$proj2Bary$SSdisc

  res_Disc_Full$proj2Bary$r2disc.B <- res_Disc_Full$proj2Bary$SSb / res_Disc_Full$proj2Bary$SSdisc









  #Decompose the barycentric sub-space (again...)
  # Pb_CP_Pbt_Cond <- input$Pb_CP_Pbt_Cond <- Pb_Cond_Sqrt %*% CP %*% t(Pb_Cond_Sqrt)
  Pb_CP_Pbt_Cond <- input$Pb_CP_Pbt_Cond <- Pb_Cond %*% CP %*% t(Pb_Cond)
  input$SSb_Cond       <- sum(diag(BtB_sqrt %*% Pb_CP_Pbt_Cond %*% BtB_sqrt))
  input$r2total.b_Cond <- input$SSb_Cond / input$SStotal

  res_Disc_Cond <- EigenCP(Pb_CP_Pbt_Cond)
  names(res_Disc_Cond$input) <- "Pb_CP_Pbt_Cond"
  names(res_Disc_Cond$eig) <- c("Ub_Cond", "Lambdab_vec", "Lambdab", "ProjMatb_Cond",
                                "tb", "Fb_Cond", "Ctrbb_Cond")

  ############## Project data into its own space...

  res_Disc_Cond$proj2Bary$Fb_Cond <- Pb_Cond %*% CP %*% t(Pb_Cond) %*% res_Disc_Cond$eig$ProjMatb_Cond

  res_Disc_Cond$proj2Bary$Fdisc   <-             CP %*% t(Pb_Cond) %*% res_Disc_Cond$eig$ProjMatb_Cond










  #Test the quality of the predictive model for the sample.
  ###Fixed effects. Predict *old* observations.

  Predict_Fixed_Rows <- list()

  #Compute d2 from stimulus a(b) to all categories B (to give an a(b)xB matrix)
  Dev2_ab_2_B <- Dev2(res_Disc_Full$proj2Bary$Fdisc, res_Disc_Full$proj2Bary$Fb_condensed)

  #Assign ab to B (identify which B is closest to each ab)
  Prediction_vec <- input$DESIGN_rows$labels[apply(Dev2_ab_2_B, 1, which.min)]

  #Transform the prediction into a design matrix
  Predict_Fixed_Rows$Prediction_mat <- makeNominalData(as.matrix(Prediction_vec))[,paste0('.',input$DESIGN_rows$labels)]
  dimnames(Predict_Fixed_Rows$Prediction_mat) <- dimnames(input$DESIGN_rows$mat)

  #Transform the design matrix to give a confusion matrix
  Predict_Fixed_Rows$Confusion_mat <- t(input$DESIGN_rows$mat) %*% Predict_Fixed_Rows$Prediction_mat
  rownames(Predict_Fixed_Rows$Confusion_mat) <- paste0(input$DESIGN_rows$labels, "_actual")
  colnames(Predict_Fixed_Rows$Confusion_mat) <- paste0(input$DESIGN_rows$labels, "_predicted")








  if(Perm_rows==TRUE){

    Perm_Rows <- DiMDS_perm_rows(input = input,
                                 res_Disc_Full = res_Disc_Full,
                                 res_Disc_Cond = res_Disc_Cond,
                                 niter=Perm_rows_niter)

  }#end Perm_rows










  if(Boot_rows==TRUE){

    Boot_Rows <- DiMDS_boot_rows(input = input,
                                 res_Disc_Full = res_Disc_Full,
                                 niter=Boot_rows_niter)

  }#end Boot_rows








  #Test the quality of the predictive model for out-of-sample stimuli.
  if(LOO_rows){

    LOO_Rows <- DiMDS_LOO_rows(input = input,
                               res_Disc_Full = res_Disc_Full,
                               res_Disc_Cond = res_Disc_Cond,
                               DESIGN_rows = input$DESIGN_rows,
                               multiplier = LOO_rows_multiplier)

    # library(RColorBrewer)
    #         heatmap(Confusion_rand,
    #         Rowv = NA, Colv=NA, revC = TRUE,
    #         col = brewer.pal(9, "PuRd"))

  }#end LOO_rows













  #Test the quality of the predictive model for out-of-sample stimuli.
  if(SH_rows){

    SH_Rows <- DiMDS_SH_rows(input = input,
                             res_Disc_Full = res_Disc_Full,
                             res_Disc_Cond = res_Disc_Cond,
                             DESIGN_rows = input$DESIGN_rows,
                             niter = SH_rows_niter)

  }#end SH_rows













  returnME <- list(input = input,
                   res_Disc_Full = res_Disc_Full,
                   res_Disc_Cond = res_Disc_Cond,
                   Predict_Fixed_Rows = Predict_Fixed_Rows)

  if(Perm_rows){
    returnME$Perm_Rows <- Perm_Rows #Permutation test of Row DESIGN vs Null (random) DESIGNs
  }
  if(Boot_rows){
    returnME$Boot_Rows <- Boot_Rows #Boostrap resampling to test stability of Categories in Row DESIGN
  }
  if(LOO_rows){
    returnME$LOO_Rows  <- LOO_Rows #LOO cross-validation to test quality of predictive model
  }
  if(SH_rows){
    returnME$SH_Rows   <- SH_Rows #SH cross-validation to test quality of predictive model
  }



  return(returnME)

}
