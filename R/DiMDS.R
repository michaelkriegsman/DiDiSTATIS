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
# #'@param Boot_rows Boolean to conduct bootstrap resampling #feature removed 4.25.2018
# #'@param Boot_rows_niter Number of bootstrap iterations #feature removed 4.25.2018
#'@param LOO_rows Boolean to conduct Leave One Out (LOO) cross-validation
#'@param LOO_rows_multiplier Increase the number of LOO iterations
#'@param SH_rows Boolean to conduct Split Half (SH) cross-validation
#'@param SH_rows_niter Number of SH iterations
#'@return Results of DiMDS
#'@export

DiMDS <- function(DATA, data_are, DESIGN_rows, n2k=NULL,
                  Perm_rows = F, Perm_rows_niter = 1000,
                  # Boot_rows = F, Boot_rows_niter = 1000, #feature removed 4.25.2018
                  LOO_rows = F,  LOO_rows_multiplier = 10,
                  SH_rows = F,   SH_rows_niter = 50){


  #Setup the input list ####
  input <- list(DATA = DATA,
                data_are = data_are,
                CP = NULL,
                SSplain = NULL,
                DESIGN_rows = DESIGN_rows)

  #Convert data to CP
  CP <- input$CP <- GetCP(DATA, data_are)
  input$SSplain <- sum(diag(CP))


  #Compute the barycentric sub-space ####
  Pb_CP_Pb_Full   <- input$Pb_CP_Pb_Full <- DESIGN_rows$Pb_Full %*% CP %*% DESIGN_rows$Pb_Full
  input$SSb       <- sum(diag(Pb_CP_Pb_Full))
  input$r2plain.b <- input$SSb / input$SSplain


  #Decompose the barycentric sub-space ####
  res_Disc_Full <- EigenCP_Full(CP = Pb_CP_Pb_Full, DESIGN_rows = DESIGN_rows, n2k=n2k)
  names(res_Disc_Full$input) <- "Pb_CP_Pb_Full"
  names(res_Disc_Full$eig) <- c("Ub_Full", "Ub_Cond",
                                "Lambdab_vec", "Lambdab",
                                "ProjMatb_Full", "ProjMatb_Cond",
                                "tb",
                                "Fb_Full", "Fb_Cond",
                                "Ctrbb_Full", "Ctrbb_Cond")

  #Project into the barycentric sub-sapce ####
  res_Disc_Full$proj2Bary$Fb_Full <- Pb_CP_Pb_Full %*% res_Disc_Full$eig$ProjMatb_Full
  res_Disc_Full$proj2Bary$Fb_Cond <- Condense_Rows(res_Disc_Full$proj2Bary$Fb_Full, DESIGN_rows$mat)

  res_Disc_Full$proj2Bary$Fdisc   <- CP %*% DESIGN_rows$Pb_Full %*% res_Disc_Full$eig$ProjMatb_Full
  # equivalently:                    CP %*%                         res_Disc_Full$eig$ProjMatb_Full
  res_Disc_Full$proj2Bary$Fab     <- res_Disc_Full$proj2Bary$Fdisc - res_Disc_Full$proj2Bary$Fb_Full

  #... and compute summary statistics ####
  res_Disc_Full$proj2Bary$SSb    <- SS_from_F(res_Disc_Full$proj2Bary$Fb_Full)
  res_Disc_Full$proj2Bary$SSdisc <- SS_from_F(res_Disc_Full$proj2Bary$Fdisc)
  res_Disc_Full$proj2Bary$SSab   <- SS_from_F(res_Disc_Full$proj2Bary$Fab)
  res_Disc_Full$proj2Bary$SSe <- input$SSplain - res_Disc_Full$proj2Bary$SSdisc

  res_Disc_Full$proj2Bary$r2disc.B     <- res_Disc_Full$proj2Bary$SSb / res_Disc_Full$proj2Bary$SSdisc
  res_Disc_Full$proj2Bary$r2plain.b    <- input$r2plain.b
  res_Disc_Full$proj2Bary$r2plain.disc <- res_Disc_Full$proj2Bary$SSdisc / input$SSplain







  # Test quality of predictive model ####
  ###Fixed effects. Predict *old* observations.

  Predict_Fixed_Rows <- list()

  #Compute d2 from stimulus a(b) to all categories B (to give an a(b)xB matrix)
  Dev2_ab_2_B <- Dev2(res_Disc_Full$proj2Bary$Fdisc, res_Disc_Full$proj2Bary$Fb_Cond)

  #Assign ab to B (identify which B is closest to each ab)
  Prediction_vec <- input$DESIGN_rows$labels[apply(Dev2_ab_2_B, 1, which.min)]

  #Transform the prediction into a design matrix
  Predict_Fixed_Rows$Prediction_mat <- makeNominalData(as.matrix(Prediction_vec))[,paste0('.',DESIGN_rows$labels)]
  dimnames(Predict_Fixed_Rows$Prediction_mat) <- dimnames(DESIGN_rows$mat)

  #Transform the design matrix to give a confusion matrix
  Predict_Fixed_Rows$Confusion_mat <- t(DESIGN_rows$mat) %*% Predict_Fixed_Rows$Prediction_mat
  rownames(Predict_Fixed_Rows$Confusion_mat) <- paste0(input$DESIGN_rows$labels, "_actual")
  colnames(Predict_Fixed_Rows$Confusion_mat) <- paste0(input$DESIGN_rows$labels, "_predicted")

  Predict_Fixed_Rows$Confusion_mat_norm <- round(Predict_Fixed_Rows$Confusion_mat / rowSums(Predict_Fixed_Rows$Confusion_mat), 2)







  if(Perm_rows==TRUE){ # Permute rows ####
    Perm_Rows <- DiMDS_perm_rows(input = input,
                                 res_Disc_Full = res_Disc_Full,
                                 niter=Perm_rows_niter)
  }#end Perm_rows
  # if(Boot_rows==TRUE){ #feature removed 4.25.2018
  #   Boot_Rows <- DiMDS_boot_rows(input = input,
  #                                res_Disc_Full = res_Disc_Full,
  #                                niter=Boot_rows_niter)
  # }#end Boot_rows
  if(LOO_rows){ # LOO rows ####
    LOO_Rows <- DiMDS_LOO_rows(input = input,
                               res_Disc_Full = res_Disc_Full,
                               multiplier = LOO_rows_multiplier)
  }#end LOO_rows
  if(SH_rows){ # SH rows ####
    SH_Rows <- DiMDS_SH_rows(input = input,
                             res_Disc_Full = res_Disc_Full,
                             niter = SH_rows_niter)
  }#end SH_rows



  returnME <- list(input = input,
                   res_Disc_Full = res_Disc_Full,
                   Predict_Fixed_Rows = Predict_Fixed_Rows)

  if(Perm_rows){
    returnME$Perm_Rows <- Perm_Rows #Permutation test of Row DESIGN vs. random DESIGNs
  }
  # if(Boot_rows){ #feature removed 4.25.2018
  #   returnME$Boot_Rows <- Boot_Rows #Boostrap resampling to test stability of Categories in Row DESIGN
  # }
  if(LOO_rows){
    returnME$LOO_Rows  <- LOO_Rows #LOO cross-validation to test quality of predictive model
  }
  if(SH_rows){
    returnME$SH_Rows   <- SH_Rows #SH cross-validation to test quality of predictive model
  }



  return(returnME)

}
