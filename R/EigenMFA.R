#EigenMFA.R

#' Eigen-decompose the Consensus, and then compute everything else
#'
#' @param MFA_collapsed All output related to individuals and Consensus
# #' @param DESIGN_tables DESIGN matrix for the tables
#' @param DESIGN The file structure related to the (table, and if applicable, row) DESIGN
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into MFA_collapsed, and remove from explicit input below

EigenMFA <- function(MFA_collapsed, DESIGN = NULL, n2k=NULL){

  #Decompose the Consensus
  res_Consensus <- EigenCP(CP = MFA_collapsed$data$Consensus)
  res_Consensus$input$DESIGN <- DESIGN
  DESIGN_tables <- DESIGN$tables$MusExp_mat #specifically, the DESIGN_tables_matrix


  #Project the Over-weighted Individuals to give PFS
  res_Consensus$ProjCP$F <- array(NA, dim=c(dim(res_Consensus$eig$F),K))
  dimnames(res_Consensus$ProjCP$F) <- list(rownames(res_Consensus$eig$F), colnames(res_Consensus$eig$F), rownames(DESIGN_tables))

  for(k in 1:K){
    ProjCPs <- paste0("res_Consensus$ProjCP$F[,,",k,"] <- MFA_collapsed$data$OverWeighted_CP_array[,,",k,"] %*% res_Consensus$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Consensus)

}
