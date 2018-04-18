#DiDiSTATIS_boot_tables.R
#
#'Bootstrap resample rows within hypothesized groups for DiDiSTATIS
#'
#'@param input Items input to DiDiSTATIS
#'@param Hierarchy_of_tables Results related to computing Barycentric Group and Grand Compromises
#'@param res_BaryGrand Results from decomposing the Barycentric Grand Compromise
#'@param niter Number of bootstrap samples
#'@return Bootstrap output
#'@export
#'

DiDiSTATIS_boot_tables <- function(input = input,
                                   Hierarchy_of_tables = Hierarchy_of_tables,
                                   res_BaryGrand = res_BaryGrand,
                                   niter=100){

  booted_table_order <- matrix(NA, input$DESIGN_tables$CD, niter)

  Fab..boot       <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_BaryGrand$eig$Lambdab.._vec), niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), paste0("niter ", 1:niter)))
  F.b..boot_Full  <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_BaryGrand$eig$Lambdab.._vec), niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), paste0("niter ", 1:niter)))
  F.b..boot_Cond  <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_BaryGrand$eig$Lambdab.._vec), niter), dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), paste0("niter ", 1:niter)))

  Fab_boot_d <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_BaryGrand$eig$Lambdab.._vec), input$DESIGN_tables$D, niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))
  F.b_boot_d <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_BaryGrand$eig$Lambdab.._vec), input$DESIGN_tables$D, niter), dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))


  Fab_boot_d_corrected <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_BaryGrand$eig$Lambdab.._vec), input$DESIGN_tables$D, niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))
  F.b_boot_d_corrected <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_BaryGrand$eig$Lambdab.._vec), input$DESIGN_tables$D, niter), dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_BaryGrand$eig$Fb..), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))


  for(i in 1:niter){

    #Get the booted table order, resampling Cd (all C tables within each d group)
    for(d in 1:input$DESIGN_tables$D){
      Cd <- which(input$DESIGN_tables$mat[,d]==1)
      booted_table_order[Cd,i] <- sample(Cd, replace=T)
    }

    #Compute Bary_Group_Compromises_boot and Bary_Grand_Compromise_boot
    TheBaryCompromises.boot <- GetBaryGrandComp_BootTables(CP_array = input$CP_array[,,booted_table_order[,i]],
                                                           DESIGN_rows = input$DESIGN_rows,
                                                           DESIGN_tables=input$DESIGN_tables,
                                                           Hierarchy_of_tables = Hierarchy_of_tables,
                                                           MFA1_Flag = input$MFA1_Flag, RV1_Flag = input$RV1_Flag,
                                                           MFA2_Flag = input$MFA2_Flag, RV2_Flag = input$RV2_Flag)

    Fab..boot[,,i] <- TheBaryCompromises.boot$data$OverWeighted_GrandCompromise %*% res_BaryGrand$eig$ProjMatb..
    F.b..boot_Full[,,i] <- input$DESIGN_rows$Pb_Full %*% Fab..boot[,,i]
    F.b..boot_Cond[,,i] <- input$DESIGN_rows$Pb_Cond %*% Fab..boot[,,i]
    #                 TheBaryCompromises.boot$data$Bary_GrandCompromise %*% res_BaryGrand$eig$ProjMatb..





    #Project over-weighted bootstrapped group compromises into the grand compromise space
    for(d in 1:input$DESIGN_tables$D){
      Fab_boot_d[,,d,i] <- TheBaryCompromises.boot$data$OverWeighted_GroupCompromise_array[,,d] %*% res_BaryGrand$eig$ProjMatb..
      F.b_boot_d[,,d,i] <- Condense_Rows(X = Fab_boot_d[,,d,i], DESIGN_rows_mat = input$DESIGN_rows$mat)
      #                    TheBaryCompromises.boot$data$OverWeighted_Pb_GroupCompromise_Pb_array[,,d] %*% res_BaryGrand$eig$ProjMatb..
    }





    #Compute Fab_boot_d_corrected
    #  For each ab stimulus, compute the deviation between the booted GrandComp and the fixed GrandComp.
    deviation_AB_i <- Fab..boot[,,i] - res_BaryGrand$Proj_disc..$F_disc..
    deviation_B_i  <- F.b..boot_Cond[,,i] - res_BaryGrand$eig$Fb..Cond

    for(d in 1:input$DESIGN_tables$D){
      Fab_boot_d_corrected[,,d,i] <- Fab_boot_d[,,d,i] - deviation_AB_i
      F.b_boot_d_corrected[,,d,i] <- F.b_boot_d[,,d,i] - deviation_B_i
    }

  }#end bootstrap iterations

  returnME <- list()
  returnME$Boot_tables_niter <- niter
  returnME$booted_table_order <- booted_table_order
  returnME$Fab..boot      <- Fab..boot
  returnME$F.b..boot_Full <- F.b..boot_Full
  returnME$F.b..boot_Cond <- F.b..boot_Cond
  returnME$Fab_boot_d     <- Fab_boot_d
  returnME$F.b_boot_d     <- F.b_boot_d
  returnME$Fab_boot_d_corrected <- Fab_boot_d_corrected
  returnME$F.b_boot_d_corrected <- F.b_boot_d_corrected

  return(returnME)
}
