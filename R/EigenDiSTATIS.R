#EigenDiSTATIS.R

#' Eigen-decompose the Compromise, and then compute everything else
#'
#' @param DiSTATIS_collapsed All output related to individuals and Consensus
#' @param DESIGN_rows List of DESIGN info for rows
#' @param DESIGN_tables List of DESIGN info for tables
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into HiDiSTATIS_collapsed, and remove from explicit input below

EigenDiSTATIS <- function(DiSTATIS_collapsed, DESIGN_rows=NULL, DESIGN_tables, n2k=NULL){

  #Decompose the Grand Compromise
  res_Compromise <- EigenCP(CP = DiSTATIS_collapsed$data$Compromise)
  names(res_Compromise$input) <- 'Compromise'

  if(!is.null(DESIGN_rows)){
    res_Compromise$eig$F_Cond <- DESIGN_rows$Pb_Cond %*% res_Compromise$eig$F
  }

  #Project the Over-weighted Individuals to give PFS
  res_Compromise$ProjCP$F <- array(NA, dim=c(dim(res_Compromise$eig$F), DESIGN_tables$CD))
  dimnames(res_Compromise$ProjCP$F) <- list(rownames(res_Compromise$eig$F), colnames(res_Compromise$eig$F), rownames(DESIGN_tables$mat))

  for(k in 1:DESIGN_tables$CD){
    ProjCPs <- paste0("res_Compromise$ProjCP$F[,,",k,"] <- DiSTATIS_collapsed$data$OverWeighted_CP_array[,,",k,"] %*% res_Compromise$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Compromise)

}
