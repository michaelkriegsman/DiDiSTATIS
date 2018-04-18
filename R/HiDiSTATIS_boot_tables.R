#HiDiSTATIS_boot_tables.R
#
#'Bootstrap resample rows within hypothesized groups for HiDiSTATIS
#'
#'@param input Items input to HiDiSTATIS
#'@param Hierarchy_of_tables Results related to computing Group and Grand Compromises
#'@param res_GrandComp Results from decomposing the Grand Compromise
#'@param niter Number of bootstrap samples
#'@return Bootstrap output
#'@export
#'

HiDiSTATIS_boot_tables <- function(input = input,
                                   Hierarchy_of_tables = Hierarchy_of_tables,
                                   res_GrandComp = res_GrandComp,
                                   niter=10){

  booted_table_order <- matrix(NA, input$DESIGN_tables$CD, niter)
  # CP.boot <- array(NA, dim=c(input$DESIGN_rows$AB, input$DESIGN_rows$AB, niter))
  # Fb.boot <- array(NA, dim=c(nrow(res_Disc_Full$eig$Fb_Cond), ncol(res_Disc_Full$eig$Fb_Cond), niter))

  Fab..boot  <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_GrandComp$eig$Lambda_vec), niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), paste0("niter ", 1:niter)))
  F.b..boot  <- array(NA, dim=c(input$DESIGN_rows$B, length(res_GrandComp$eig$Lambda_vec), niter),  dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), paste0("niter ", 1:niter)))

  Fab_boot_d <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_GrandComp$eig$Lambda_vec), input$DESIGN_tables$D, niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))
  F.b_boot_d <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_GrandComp$eig$Lambda_vec), input$DESIGN_tables$D, niter), dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))

  #I also need to correct the group projections...
  #For each booted GrandCompromise projected into the grand compromise space,
  # each stimulus is a certain distance from it's fixed effects position.
  # This is a deviation of bootstrapping (each grand compromise differs slightls),
  # and is separate from the error we wish to test, which is the deviation between
  # the groups and their barycenter.
  #  Therefore, I will compute the deviation between each bootstrapped grand compromise factor scores
  #  and the fixed effects grand compromise factor scores, and then,
  #  subtract this difference from the Fab_boot_d. The result will be the deviation of
  #  the booted group compromises from their grand compromise, visualized relative to the
  #  fixed effects grand compromise factor scores.
  #   In other words, I am manually removing the deviation caused by each bootstrap giving a
  #   slightly different grand compromise.

  Fab_boot_d_corrected <- array(NA, dim=c(input$DESIGN_rows$AB, length(res_GrandComp$eig$Lambda_vec), input$DESIGN_tables$D, niter), dimnames=list(rownames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))
  F.b_boot_d_corrected <- array(NA, dim=c(input$DESIGN_rows$B,  length(res_GrandComp$eig$Lambda_vec), input$DESIGN_tables$D, niter), dimnames=list(colnames(input$DESIGN_rows$mat), colnames(res_GrandComp$eig$F), input$DESIGN_tables$labels, paste0("niter ", 1:niter)))


  for(i in 1:niter){

    #Get the booted table order, resampling Cd (all C tables within each d group)
    for(d in 1:input$DESIGN_tables$D){
      Cd <- which(input$DESIGN_tables$mat[,d]==1)
      booted_table_order[Cd,i] <- sample(Cd, replace=T)
    }

    #Compute Group_Compromises_boot and Grand_Compromise_boot
    TheCompromises.boot <- GetGrandCompromise_BootTables(CP_array = input$CP_array[,,booted_table_order[,i]],
                                                         DESIGN_tables=input$DESIGN_tables,
                                                         Hierarchy_of_tables = Hierarchy_of_tables)

    Fab..boot[,,i] <- TheCompromises.boot$data$GrandCompromise %*% res_GrandComp$eig$ProjMat
    F.b..boot[,,i] <- Condense_Rows(X = Fab..boot[,,i], DESIGN_rows_mat = input$DESIGN_rows$mat)

    #Project over-weighted bootstrapped group compromises into the grand compromise space
    for(d in 1:input$DESIGN_tables$D){
      Fab_boot_d[,,d,i] <- TheCompromises.boot$data$OverWeighted_GroupCompromise_array[,,d] %*% res_GrandComp$eig$ProjMat
      F.b_boot_d[,,d,i] <- Condense_Rows(X = Fab_boot_d[,,d,i], DESIGN_rows_mat = input$DESIGN_rows$mat)
    }

    #Compute Fab_boot_d_corrected
    #  For each ab stimulus, compute the deviation between the booted GrandComp and the fixed GrandComp.
    deviation_i <- Fab..boot[,,i] - res_GrandComp$eig$F

    for(d in 1:input$DESIGN_tables$D){
      Fab_boot_d_corrected[,,d,i] <- Fab_boot_d[,,d,i] - deviation_i
      F.b_boot_d_corrected[,,d,i] <- Condense_Rows(Fab_boot_d_corrected[,,d,i], input$DESIGN_rows$mat)
    }

  }#end bootstrap iterations

  returnME <- list()
  returnME$booted_table_order <- booted_table_order
  returnME$Fab..boot <- Fab..boot
  returnME$F.b..boot <- F.b..boot
  returnME$Fab_boot_d <- Fab_boot_d
  returnME$F.b_boot_d <- F.b_boot_d
  returnME$Fab_boot_d_corrected <- Fab_boot_d_corrected
  returnME$F.b_boot_d_corrected <- F.b_boot_d_corrected

  return(returnME)

}#end HiDiSTATIS_boot_tables()

