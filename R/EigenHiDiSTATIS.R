#EigenHiDiSTATIS.R

#' Eigen-decompose the Grand Compromise, and then compute everything else
#'
#' @param HiDiSTATIS_collapsed All output related to individuals, Groups, and Grand Compromise
#' @param DESIGN_tables The file structure related to the table DESIGN
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into HiDiSTATIS_collapsed, and remove from explicit input below

EigenHiDiSTATIS <- function(HiDiSTATIS_collapsed, DESIGN_tables = NULL, n2k=NULL){

  #Decompose the Grand Compromise
  res_Grand <- EigenCP(CP = HiDiSTATIS_collapsed$data$GrandCompromise)

  #Project the Over-weighted Groups
  res_Grand$ProjGroup$F <- array(NA, dim=c(dim(res_Grand$eig$F),DESIGN_tables$D))
  dimnames(res_Grand$ProjGroup$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), colnames(DESIGN_tables$mat))

  for(d in 1:DESIGN_tables$D){
    ProjGroupComps <- paste0("res_Grand$ProjGroup$F[,,",d,"] <- HiDiSTATIS_collapsed$data$OverWeighted_GroupCompromise_array[,,",d,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjGroupComps))
  }

  #########################################################
  # res_Grand$ProjGroup$F[,,d] <- HiDiSTATIS_collapsed$data$OverWeighted_GroupCompromise_array[,,",d,"] %*% res_Grand$eig$ProjMat
  #########################################################


  #Project the Over-weighted Individuals
  res_Grand$ProjCP$F <- array(NA, dim=c(dim(res_Grand$eig$F), nrow(DESIGN_tables$mat)))
  dimnames(res_Grand$ProjCP$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), rownames(DESIGN_tables$mat))

  for(cd in 1:nrow(DESIGN_tables$mat)){
    ProjCPs <- paste0("res_Grand$ProjCP$F[,,",cd,"] <- HiDiSTATIS_collapsed$data$OverWeighted_CP_array[,,",cd,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Grand)

}
