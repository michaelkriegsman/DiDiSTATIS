#HiDiSTATIS_perm_tables.R
#
#'Permutation test for the null hypothesis on the tables of HiDiSTATIS
#'
#'@param input Items input to HiDiSTATIS
#'@param Hierarchy_of_tables Results related to computing Group and Grand Compromises
#'@param res_GrandComp Results from decomposing the Grand Compromise
#'@param niter Number of iterations (permutations)
#'@return Permuted output
#'@export


HiDiSTATIS_perm_tables <- function(input = NULL,
                                   Hierarchy_of_tables = NULL,
                                   res_GrandComp = NULL,
                                   niter = 100){

  ################## Move these fixed things to Eigen_HiDiSTATIS #####################
  #Fixed results
  Dev2_Fab.d_2_Fab <- matrix(NA, nrow(input$CP_array), input$DESIGN_tables$D, dimnames = list(rownames(input$CP_array), input$DESIGN_tables$labels))

  for(d in 1:input$DESIGN_tables$D){
    Dev2_Fab.d_2_Fab[,d] <- diag(Dev2(res_GrandComp$ProjGroup$F[,,d], res_GrandComp$eig$F))
  }

  Dev2_F.d_2_F <- apply(Dev2_Fab.d_2_Fab, 2, sum)
  ####################################################################################



  #Initialize objects

  #permute the indices for the tables, for all iterations
  permuted_table_order <- matrix(NA, input$DESIGN_tables$CD, niter)


  Dev2_Fab.d.perm_2_Fab.perm <- array(NA, dim=c(nrow(input$CP_array), input$DESIGN_tables$D, niter))
  dimnames(Dev2_Fab.d.perm_2_Fab.perm) <- list(rownames(input$CP_array), input$DESIGN_tables$labels, paste0("iter ", 1:niter))

  r2_Groups_perm <- matrix(NA, niter, 1)


  #One iteration at a time
  for(i in 1:niter){

    #This iteration's permuted order
    permuted_table_order[,i] <- sample(1:nrow(input$DESIGN_tables$mat))

    DESIGN_tables.perm <- list()

    #create a permuted DESIGN_tables_mat
    DESIGN_tables.perm$mat <- input$DESIGN_tables$mat[permuted_table_order[,i],]
    rownames(DESIGN_tables.perm$mat) <- rownames(input$DESIGN_tables$mat)

    DESIGN_tables.perm$D <- input$DESIGN_tables$D
    DESIGN_tables.perm$CD <- input$DESIGN_tables$CD

    Hierarchy_of_tables.perm <- GetGrandCompromise(input$CP_array, DESIGN_tables.perm)

    res_GrandComp.perm <- EigenHiDiSTATIS(Hierarchy_of_tables.perm, DESIGN_tables.perm, n2k=n2k)

    #Compute distance from res_GrandComp.perm$ProjGroup$F to res_GrandComp.perm$eig$F
    #for all AB stimuli
    for(d in 1:input$DESIGN_tables$D){
      Dev2_Fab.d.perm_2_Fab.perm[,d,i] <- diag(Dev2(res_GrandComp.perm$ProjGroup$F[,,d], res_GrandComp.perm$eig$F))
    }

    r2_Groups_perm[i] <- res_GrandComp.perm$EffectSize$r2_Groups
  }

  #Aggregate (sum) the Dev2 over stimuli (for histograms)
  Dev2_F.d.perm_2_F.perm <- apply(Dev2_Fab.d.perm_2_Fab.perm, c(3,2), sum)

  #Average the Dev2 over the groups (for visualizing average distance from each stimulus)
  Avg_Dev2_Fab.d.perm_2_Fab.perm <- apply(Dev2_Fab.d.perm_2_Fab.perm, c(1,3), mean)

  return(list(r2_Groups_perm = r2_Groups_perm,
              Dev2_Fab.d_2_Fab = Dev2_Fab.d_2_Fab,
              Dev2_F.d_2_F     = Dev2_F.d_2_F,
              Dev2_Fab.d.perm_2_Fab.perm = Dev2_Fab.d.perm_2_Fab.perm,
              Dev2_F.d.perm_2_F.perm     = Dev2_F.d.perm_2_F.perm,
              Avg_Dev2_Fab.d.perm_2_Fab.perm = Avg_Dev2_Fab.d.perm_2_Fab.perm))
}
