#EigenMFA.R

#' Eigen-decompose the Consensus, and then compute everything else
#'
#' @param MFA_collapsed All output related to individuals and Consensus
#' @param DESIGN_rows List of DESIGN info for rows
#' @param DESIGN_tables List of DESIGN info for tables
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into MFA_collapsed, and remove from explicit input below

EigenMFA <- function(MFA_collapsed, DESIGN_rows=NULL, DESIGN_tables, n2k=NULL){

  #Decompose the Consensus
  res_Consensus <- EigenCP(CP = MFA_collapsed$data$Consensus)
  names(res_Consensus$input) <- 'Consensus'

  if(!is.null(DESIGN_rows)){
    res_Consensus$eig$F_Cond <- DESIGN_rows$Pb_Cond %*% res_Consensus$eig$F
  }

  #Project the Over-weighted Individuals to give PFS
  res_Consensus$ProjCP$F <- array(NA, dim=c(dim(res_Consensus$eig$F), DESIGN_tables$CD))
  dimnames(res_Consensus$ProjCP$F) <- list(rownames(res_Consensus$eig$F), colnames(res_Consensus$eig$F), rownames(DESIGN_tables$mat))

  for(cd in 1:DESIGN_tables$CD){
    ProjCPs <- paste0("res_Consensus$ProjCP$F[,,",cd,"] <- MFA_collapsed$data$OverWeighted_CP_array[,,",cd,"] %*% res_Consensus$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Consensus)

}
