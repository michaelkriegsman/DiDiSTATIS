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

  #Decompose the Grand Compromise ####
  # HiDiSTATIS_collapsed <- Hierarchy_of_tables
  res_Grand <- EigenCP(CP = HiDiSTATIS_collapsed$data$GrandCompromise)
  # Can't rename everything right now...
  # names(res_Grand$input) <- c("GrandCompromise")
  # names(res_Grand$eig)   <- c("U..", "Lambda.._vec", "Lambda..", "ProjMat..",
  #                                 "t..", "F..", "Ctrb..")

  res_Grand$eig$SS.. <- SS_from_F(res_Grand$eig$F) * DESIGN_tables$CD




  #Project the Over-weighted Groups ####
  res_Grand$ProjGroup$F <- array(NA, dim=c(dim(res_Grand$eig$F),DESIGN_tables$D))
  dimnames(res_Grand$ProjGroup$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), colnames(DESIGN_tables$mat))
  res_Grand$ProjGroup$SS.D     <- matrix(NA, DESIGN_tables$D, 1)
  # res_Grand$ProjGroup$F_Cond <- DESIGN_rows$Pb_Cond %*% res_Grand$ProjGroup$F

  for(d in 1:DESIGN_tables$D){
    ProjGroupComps <- paste0("res_Grand$ProjGroup$F[,,",d,"] <- HiDiSTATIS_collapsed$data$OverWeighted_GroupCompromise_array[,,",d,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjGroupComps))

    res_Grand$ProjGroup$SS.D[d] <- SS_from_F(res_Grand$ProjGroup$F[,,d]) * colSums(DESIGN_tables$mat)[d]
  }

  res_Grand$ProjGroup$sum_SS.D <- sum(res_Grand$ProjGroup$SS.D)




  #Project the Over-weighted Individuals ####
  res_Grand$ProjCP$F <- array(NA, dim=c(dim(res_Grand$eig$F), nrow(DESIGN_tables$mat)))
  dimnames(res_Grand$ProjCP$F) <- list(rownames(res_Grand$eig$F), colnames(res_Grand$eig$F), rownames(DESIGN_tables$mat))
  res_Grand$ProjCP$SScd     <- matrix(NA, DESIGN_tables$CD, 1)


  for(cd in 1:nrow(DESIGN_tables$mat)){
    ProjCPs <- paste0("res_Grand$ProjCP$F[,,",cd,"] <- HiDiSTATIS_collapsed$data$OverWeighted_CP_array[,,",cd,"] %*% res_Grand$eig$ProjMat")
    eval(parse(text = ProjCPs))

    res_Grand$ProjCP$SScd[cd] <- SS_from_F(res_Grand$ProjCP$F[,,cd])
  }

  res_Grand$ProjCP$sum_SScd <- sum(res_Grand$ProjCP$SScd)

  res_Grand$EffectSize$SSbetween <- res_Grand$ProjGroup$sum_SS.D - res_Grand$eig$SS..
  res_Grand$EffectSize$SSwithin  <- res_Grand$ProjCP$sum_SScd    - res_Grand$ProjGroup$sum_SS.D
  res_Grand$EffectSize$r2_Groups <- res_Grand$EffectSize$SSbetween / (res_Grand$EffectSize$SSbetween + res_Grand$EffectSize$SSwithin)

  X <- " "
  res_Grand$EffectSize$SS_summary <- data.frame(rbind(c(X, "C(D)", ".(D)", "Within-group", X),
                                               c("I", round(res_Grand$ProjCP$sum_SScd,3), round(res_Grand$ProjGroup$sum_SS.D, 3), round(res_Grand$EffectSize$SSwithin, 3), X),
                                               c(X, X, X, X, X),
                                               c(X, X, ".(D)", ".(.)", "Between-group"),
                                               c(X, "I", round(res_Grand$ProjGroup$sum_SS.D, 3), round(res_Grand$eig$SS.., 3), round(res_Grand$EffectSize$SSbetween, 3))))


  return(res_Grand)

}
