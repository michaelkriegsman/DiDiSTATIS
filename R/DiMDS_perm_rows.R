#DiMDS_perm_rows.R
#
#'Permutation test for the null hypothesis on the rows of DiMDS
#'
#'@param input Items input to DiMDS
#'@param res_Disc_Full Output from decomposing the barycentric discriminant sub-space
#'@param niter Number of iterations (permutations)
#'@return Permuted output
#'@export


DiMDS_perm_rows <- function(input = NULL,
                            res_Disc_Full = NULL,
                            niter = 100){

  #Initialize objects

  #permute the indices for the rows, for all iterations
  permuted_row_order <- matrix(NA, input$DESIGN_rows$AB, niter)

  #results for permuted barycenters
  SSb.perm       <- matrix(NA, niter, 1)
  # r2total.b_perm <- matrix(NA, niter, 1)

  SSdisc.perm        <- matrix(NA, niter, 1)
  # r2disc_perm.b_perm <- matrix(NA, niter, 1)



  #One iteration at a time
  for(i in 1:niter){

    #This iteration's permuted order
    permuted_row_order[,i] <- sample(1:nrow(input$DESIGN_rows$mat))

    #create a permuted DESIGN_rows_mat
    DESIGN_rows_mat.perm <- input$DESIGN_rows$mat[permuted_row_order[,i],]
    rownames(DESIGN_rows_mat.perm) <- rownames(input$DESIGN_rows$mat)

    #and corresponding projector matrix for the permuted hypothesized DESIGN_rows
    Pb_Full.perm <- Bary_Projector(DESIGN_rows_mat.perm)
    Pb_Cond.perm <- Bary_Projector_Cond(DESIGN_rows_mat.perm)

    #Compute SSb for permuted barycenters
    Pb_CP_Pb_Full.perm <- Pb_Full.perm %*% input$CP %*% Pb_Full.perm
    SSb.perm[i]        <- sum(diag(Pb_CP_Pb_Full.perm))
    # code replaced below... r2total.b_perm[i]  <- SSb.perm[i] / input$SStotal

    #Eigen-Decompose the permuted barycenters to give the permuted discriminant space.
    res_Disc_Full.perm <- EigenCP_Full(CP = Pb_CP_Pb_Full.perm,
                                       DESIGN_rows = input$DESIGN_rows)
    names(res_Disc_Full.perm$input) <- "Pb_CP_Pb_Full.perm"
    names(res_Disc_Full.perm$eig) <- c("Ub_Full.perm", "Ub_Cond.perm",
                                       "Lambdab_vec.perm", "Lambdab.perm",
                                       "ProjMatb_Full.perm", "ProjMatb_Cond.perm",
                                       "tb.perm",
                                       "Fb_Full.perm", "Fb_Cond.perm",
                                       "Ctrbb_Full.perm", "Ctrbb_Cond.perm")

    Fdisc.perm <- input$CP %*% Pb_Full.perm %*% res_Disc_Full.perm$eig$ProjMatb_Full.perm
    # equivalent: input$CP                  %*% res_Disc_Full.perm$eig$ProjMatb_Full.perm

    SSdisc.perm[i] <- SS_from_F(Fdisc.perm)

  }

  r2plain.b_perm     <- SSb.perm / input$SSplain
  r2disc_perm.b_perm <- SSb.perm / SSdisc.perm

  returnME <- list()
  returnME$permuted_row_order <- permuted_row_order
  returnME$SSb.perm           <- SSb.perm
  returnME$SSdisc.perm        <- SSdisc.perm
  returnME$r2plain.b_perm     <- r2plain.b_perm
  returnME$r2disc_perm.b_perm <- r2disc_perm.b_perm

  return(returnME)

}





