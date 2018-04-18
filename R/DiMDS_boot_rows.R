#DiMDS_boot_rows.R
#
#'Bootstrap resample rows within hypothesized groups for DiMDS
#'
#'@param input Items input to DiMDS
#'@param res_Disc_Full Output from decomposing the barycentric discriminant sub-space
#'@param niter Number of iterations (permutations)
#'@return Permuted output
#'@export
#'

DiMDS_boot_rows <- function(input = NULL,
                            res_Disc_Full = res_Disc_Full,
                            niter=10){

  booted_row_order <- matrix(NA, input$DESIGN_rows$AB, niter)
  CP.boot <- array(NA, dim=c(input$DESIGN_rows$AB, input$DESIGN_rows$AB, niter))
  Fb.boot <- array(NA, dim=c(nrow(res_Disc_Full$eig$Fb_Cond), ncol(res_Disc_Full$eig$Fb_Cond), niter))

  for(i in 1:niter){

    #Get the booted row order, resampling Ab (all A stimuli within each b group)
    for(b in 1:input$DESIGN_rows$B){
      Ab <- which(input$DESIGN_rows$mat[,b]==1)
      booted_row_order[Ab,i] <- sample(Ab, replace=T)
    }

    #Get the booted CP matrix
    for(rows in 1:input$DESIGN_rows$AB){
      for(cols in 1:input$DESIGN_rows$AB){
        CP.boot[rows, cols, i] <- input$CP[booted_row_order[rows,i], booted_row_order[cols,i]]
      }
    }

    Fb.boot[,,i] <- input$DESIGN_rows$Pb_Cond %*% CP.boot[,,i] %*% input$DESIGN_rows$Pb_Full %*% res_Disc_Full$eig$ProjMatb_Full

  }#end niter

  returnME <- list()
  returnME$booted_row_order <- booted_row_order
  returnME$CP.boot <- CP.boot
  returnME$Fb.boot <- Fb.boot

  return(returnME)

}#end function
