#EigenDiSTATIS.R

#' Eigen-decompose the Compromise, and then compute everything else
#'
#' @param DiSTATIS_collapsed All output related to individuals and Compromise
# #' @param DESIGN_tables DESIGN matrix for the tables
#' @param DESIGN The file structure related to the (table, and if applicable, row) DESIGN
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into HiDiSTATIS_collapsed, and remove from explicit input below

EigenDiSTATIS <- function(DiSTATIS_collapsed, DESIGN = NULL, n2k=NULL){

  #Decompose the Grand Compromise
  res_Compromise <- EigenCP(CP = DiSTATIS_collapsed$data$Compromise)
  res_Compromise$input$DESIGN <- DESIGN
  DESIGN_tables <- DESIGN$tables$MusExp_mat #specifically, the DESIGN_tables_matrix


  #Project the Over-weighted Individuals to give PFS
  res_Compromise$ProjCP$F <- array(NA, dim=c(dim(res_Compromise$eig$F),K))
  dimnames(res_Compromise$ProjCP$F) <- list(rownames(res_Compromise$eig$F), colnames(res_Compromise$eig$F), rownames(DESIGN_tables))

#don't need to compute PFS_d for DiSTATIS in the package, but I'd like to see how much the hierarchical stuff helps... so would need it. 
  # res_DiSTATIS$PFS$MusExp.Low  <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,1]==1)], c(1,2), mean)
  # res_DiSTATIS$PFS$MusExp.Mid  <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,2]==1)], c(1,2), mean)
  # res_DiSTATIS$PFS$MusExp.High <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,3]==1)], c(1,2), mean)
  # 
  for(k in 1:K){
    ProjCPs <- paste0("res_Compromise$ProjCP$F[,,",k,"] <- DiSTATIS_collapsed$data$OverWeighted_CP_array[,,",k,"] %*% res_Compromise$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Compromise)

}
