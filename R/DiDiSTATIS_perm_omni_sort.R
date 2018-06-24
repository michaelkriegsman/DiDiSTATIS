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

  if(input$data_are!='sort' & input$data_are!='sort_dist'){return(paste0('DATA are not sorting data. Check "data_are".'))}



  ###Initialize objects
  RETURN_omni <- list()
  RETURN_omni$SS_.b..                  <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$SS_.b.D                  <- array(NA, dim=c(input$DESIGN_tables$D, niter), dimnames = list(input$DESIGN_tables$labels, paste0("iter", 1:niter)))
  RETURN_omni$SS_ab.D                  <- array(NA, dim=c(input$DESIGN_tables$D, niter), dimnames = list(input$DESIGN_tables$labels, paste0("iter", 1:niter)))
  RETURN_omni$SS_plain..               <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$SS_b_BETWEEN             <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$SS_b_WITHIN              <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Categories..          <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Categories.D          <- array(NA, dim=c(input$DESIGN_tables$D, niter), dimnames = list(input$DESIGN_tables$labels, paste0("iter", 1:niter)))
  # RETURN_omni$r2_Categories.D_summed   <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  # RETURN_omni$r2_CategoriesCD_summed   <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Groups_b              <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Groups_Disc           <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_BD_ABCD               <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Plain_b_..            <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Plain_b_.D            <- array(NA, dim=c(input$DESIGN_tables$D, niter), dimnames = list(input$DESIGN_tables$labels, paste0("iter", 1:niter)))
  RETURN_omni$r2_Plain_Disc_..         <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  RETURN_omni$r2_Plain_Disc_.D         <- array(NA, dim=c(input$DESIGN_tables$D, niter), dimnames = list(input$DESIGN_tables$labels, paste0("iter", 1:niter)))
  # RETURN_omni$r2_Plain_Disc_.D_summed  <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))
  # RETURN_omni$r2_Plain_Disc_CD         <- array(NA, dim=c(input$DESIGN_tables$CD,niter), dimnames = list(rownames(input$DESIGN_tables$mat), paste0("iter", 1:niter)))
  # RETURN_omni$r2_Plain_Disc_CD_summed  <- array(NA, dim=c(1,                     niter), dimnames = list(c(), paste0("iter", 1:niter)))



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


    RETURN_omni$SS_.b..[i]          <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_.b..
    RETURN_omni$SS_.b.D[,i]         <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_.b.D
    RETURN_omni$SS_ab.D[,i]         <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_ab.D
    RETURN_omni$SS_plain..[i]       <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_plain..
    RETURN_omni$SS_b_BETWEEN[i]     <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_b_BETWEEN
    RETURN_omni$SS_b_WITHIN[i]      <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$SS_b_WITHIN
    RETURN_omni$r2_Categories..[i]  <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Categories..
    RETURN_omni$r2_Categories.D[,i] <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Categories.D
    # RETURN_omni$r2_Categories.D_summed[i] <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Categories.D_summed
    # RETURN_omni$r2_CategoriesCD_summed[i] <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_CategoriesCD_summed
    RETURN_omni$r2_Groups_b[i]      <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Groups_b
    RETURN_omni$r2_Groups_Disc[i]   <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Groups_Disc
    RETURN_omni$r2_BD_ABCD[i]       <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_BD_ABCD
    RETURN_omni$r2_Plain_b_..[i]    <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_b_..
    RETURN_omni$r2_Plain_b_.D[,i]   <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_b_.D
    RETURN_omni$r2_Plain_Disc_..[i] <- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_Disc_..
    RETURN_omni$r2_Plain_Disc_.D[,i]<- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_Disc_.D
    # RETURN_omni$r2_Plain_Disc_.D_summed[i]<- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_Disc_.D_summed
    # RETURN_omni$r2_Plain_Disc_CD[,i]<- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_Disc_CD
    # RETURN_omni$r2_Plain_Disc_CD_summed[i]<- res_DiDiSTATIS_omni$res_BaryGrand$EffectSize$r2_Plain_Disc_CD_summed

  }

  return(RETURN_omni)

}
