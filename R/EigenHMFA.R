#EigenHMFA.R

#' Eigen-decompose the Grand Consensus, and then compute everything else
#'
#' @param HMFA_collapsed All output related to individuals and Consensus
#' @param DESIGN_rows List of DESIGN info for rows
#' @param DESIGN_tables List of DESIGN info for tables
#' @param n2k Number of components 2 keep
#' @return Computational output
#' @export

#eventually, integrate DESIGN into HMFA_collapsed, and remove from explicit input below

EigenHMFA <- function(HMFA_collapsed, DESIGN_rows, DESIGN_tables, n2k=NULL){

  #Decompose the Grand Compromise
  res_Grand <- EigenCP(CP = HMFA_collapsed$data$GrandConsensus)
  names(res_Grand$input) <- 'GrandConsensus'

  # res_Grand$input$DESIGN <- DESIGN
  # DESIGN_tables <- DESIGN$tables$MusExp_mat #specifically, the DESIGN_tables_matrix


  #Project the Over-weighted Groups
  res_Grand$ProjGroup$F <- array(NA, dim=c(dim(res_Grand$eig$F),DESIGN_tables$D))
  dimnames(res_Grand$ProjGroup$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), colnames(DESIGN_tables$mat))

  for(d in 1:DESIGN_tables$D){
    ProjGroupCons <- paste0("res_Grand$ProjGroup$F[,,",d,"] <- HMFA_collapsed$data$OverWeighted_GroupConsensus_array[,,",d,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjGroupCons))
  }



  #Project the Over-weighted Individuals
  res_Grand$ProjCP$F <- array(NA, dim=c(dim(res_Grand$eig$F), DESIGN_tables$CD))
  dimnames(res_Grand$ProjCP$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), rownames(DESIGN_tables$mat))

  for(cd in 1:DESIGN_tables$CD){
    ProjCPs <- paste0("res_Grand$ProjCP$F[,,",cd,"] <- HMFA_collapsed$data$OverWeighted_CP_array[,,",cd,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjCPs))
  }

  return(res_Grand)

}
