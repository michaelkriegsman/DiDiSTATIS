#DiDiSTATIS_perm_omni_sort.R
#
#'Permutation test for the omnibus null hypothesis for DiDiSTATIS sorting data
#'
#'@param input Items input to HiDiSTATIS
#'@param Hierarchy_of_tables Results related to computing Group and Grand Compromises
#'@param res_BaryGrand Results from decomposing the Barycentric Grand Compromise
#'@param niter Number of iterations (permutations)
#'@return Permuted output
#'@export


DiDiSTATIS_perm_omni_sort <- function(input = NULL,
                                      Hierarchy_of_tables = NULL,
                                      res_BaryGrand = NULL,
                                      niter = 100){

  if(input$data_are!='sort'){return(paste0('DATA are not sorting data. Check "data_are".'))}



  ###Initialize objects
  RETURN_omni <- list()
  RETURN_omni$Plain$SS_plain_fromTrace.perm          <- array(NA, dim=c(1, niter))
  RETURN_omni$r2$Plain_disc.perm                     <- array(NA, dim=c(1, niter))
  RETURN_omni$r2$disc_B.perm                         <- array(NA, dim=c(1, niter))
  RETURN_omni$Proj_B.D$sum_SS_B.D_FromF.perm         <- array(NA, dim=c(1, niter))
  RETURN_omni$Proj_disc.cd$sum_SS_disc.cd_FromF.perm <- array(NA, dim=c(1, niter))
  RETURN_omni$r2$disc.cd_B.D.perm                    <- array(NA, dim=c(1, niter))
  # RETURN_omni$Dev2$SS_Dev2_Fb.._Fb.d.perm            <- array(NA, dim=c(niter, ncol(res_BaryGrand$Dev2$SS_Dev2_Fb.._Fb.d)))
  # RETURN_omni$Dev2$r2_Fb.._Fb.d.perm                 <- array(NA, dim=c(niter, ncol(res_BaryGrand$Dev2$r2_Fb.._Fb.d)))

  RETURN_omni$r2$disc.D_B.D.perm                     <- array(NA, dim=c(input$DESIGN_tables$D, niter))
  RETURN_omni$r2$plain.D_B.D.perm                    <- array(NA, dim=c(input$DESIGN_tables$D, niter))



  #permute the indices for the rows, for all iterations
  permuted_row_order <- matrix(NA, input$DESIGN_rows$AB, niter)

  #permute the indices for the tables, for all iterations
  permuted_table_order <- matrix(NA, input$DESIGN_tables$CD, niter)

  ###One iteration at a time
  for(i in 1:niter){

    #Set this iteration's permuted order
    permuted_row_order[,i]   <- sample(1:nrow(input$DESIGN_rows$mat))
    permuted_table_order[,i] <- sample(1:nrow(input$DESIGN_tables$mat))

    res_DiDiSTATIS_omni <- DiDiSTATIS(DATA = input$DATA[permuted_row_order[,i], permuted_table_order[,i]],
                                      data_are = 'sort', n2k = NULL,
                                      MFA1_Flag = input$MFA1_Flag,
                                      RV1_Flag = input$RV1_Flag,
                                      MFA2_Flag = input$MFA2_Flag,
                                      RV2_Flag = input$RV2_Flag,
                                      DESIGN_rows = input$DESIGN_rows,
                                      DESIGN_tables = input$DESIGN_tables)


    #See how the plain changes over perm iterations
    RETURN_omni$Plain$SS_plain_fromTrace.perm[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$Plain$SS_plain_fromTrace

    #Whats the ratio of the DISC space (all stimuli, from barygrand perspective) to the Plain
    RETURN_omni$r2$Plain_disc.perm[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$r2$Plain_disc

    #The sum_SS_B.D is the sum of the between-category effect and the between-group effect
    #So this is a test of the omnibus
    RETURN_omni$Proj_disc.cd$sum_SS_disc.cd_FromF.perm[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromF
    RETURN_omni$Proj_B.D$sum_SS_B.D_FromF.perm[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$Proj_B.D$sum_SS_B.D_FromF

    RETURN_omni$r2$disc.cd_B.D.perm[,i] <- RETURN_omni$Proj_B.D$sum_SS_B.D_FromF.perm[,i] / RETURN_omni$Proj_disc.cd$sum_SS_disc.cd_FromF.perm[,i]

    RETURN_omni$r2$disc.D_B.D.perm[,i]  <- res_DiDiSTATIS_omni$res_BaryGrand$Proj_B.D$SS_B.D_FromF / res_DiDiSTATIS_omni$res_BaryGrand$Proj_disc.D$SS_disc.D_FromF
    RETURN_omni$r2$plain.D_B.D.perm[,i] <- RETURN_omni$Proj_B.D$sum_SS_B.D_FromF.perm[,i] / RETURN_omni$Proj_disc.cd$sum_SS_disc.cd_FromF.perm[,i]


    # #Test the omnibus on the row effect
    # #SSb.. / SSdisc..
    # #SSb.. / (SSb.. + SSab..)
    # RETURN_omni$r2$disc_B.perm[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$r2$disc_B
    # res_DiDiSTATIS_omni$res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromF
    # #Test the omnibus on the table effect
    # #sum(Dev2(Fb.., Fb.d)) = the variability of the groups around the stimulus categories.
    # #So, this looks like an "error"... variability around a point.
    # #But, it is the signal for the group effect.
    # #Quantifies the variability of the groups around their mean.
    # res_DiDiSTATIS_omni$res_BaryGrand$Dev2$SS_Dev2_Fb.._Fb.d
    # res_DiDiSTATIS_omni$res_BaryGrand$Dev2$r2_Fb.._Fb.d
    #
    # res_DiDiSTATIS_omni$res_BaryGrand$Dev2$r2_Fb.._Fb.d - mean(res_DiDiSTATIS_omni$res_BaryGrand$Dev2$r2_Fb.._Fb.d)

  }

  return(RETURN_omni)

}
