#EigenDiDiSTATIS.R

#' Eigen-decompose the Barycentric Grand Compromise, and project in Groups
#'
#' @param Hierarchy_of_tables All output related to individuals, Groups, and Grand Compromise
#' @param DESIGN_rows The file structure related to the row DESIGN
#' @param DESIGN_tables The file structure related to the table DESIGN
#' @param n2k Number of components 2 keep
#' @return Output
#' @export

EigenDiDiSTATIS <- function(Hierarchy_of_tables, DESIGN_rows, DESIGN_tables, n2k=NULL){

  #Decompose the Grand Compromise
  res_BaryGrand <- EigenCP(CP = Hierarchy_of_tables$data$Bary_GrandCompromise)
  names(res_BaryGrand$input) <- c("Bary_GrandCompromise")
  names(res_BaryGrand$eig)   <- c("Ub..", "Lambdab.._vec", "Lambdab..", "ProjMatb..",
                                  "tb..", "Fb..", "Ctrbb..")

  res_BaryGrand$eig$Fb..Cond <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$eig$Fb..
  #Need to scale by CD to include the number of participants behind each data point
  #Don't need to scale by A(b), because I'm computing on all rows (Fb.., Fb..Cond would need to scale by A(b))
  res_BaryGrand$eig$SS_B.._fromTrace <- sum(diag(Hierarchy_of_tables$data$Bary_GrandCompromise)) * DESIGN_tables$CD
  res_BaryGrand$eig$SS_B.._fromF     <- SS_from_F(res_BaryGrand$eig$Fb..) * DESIGN_tables$CD








  #Project Barycentric Group Compromises
  res_BaryGrand$Proj_B.D$F_B.D      <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..), DESIGN_tables$D))
  res_BaryGrand$Proj_B.D$F_B.D_Cond <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..Cond), DESIGN_tables$D))
  res_BaryGrand$Proj_B.D$SS_B.D_FromTrace <- matrix(NA, DESIGN_tables$D, 1)
  res_BaryGrand$Proj_B.D$SS_B.D_FromF     <- matrix(NA, DESIGN_tables$D, 1)
  for(d in 1:DESIGN_tables$D){
    res_BaryGrand$Proj_B.D$F_B.D[,,d]      <- Hierarchy_of_tables$data$OverWeighted_Pb_GroupCompromise_Pb_array[,,d] %*% res_BaryGrand$eig$ProjMatb..
    res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d] <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$Proj_B.D$F_B.D[,,d]
    #Need to scale by C(d) to include the number of participants behind each data point
    #Don't need to scale by A(b), because I'm computing on all rows (Fb.., Fb..Cond would need to scale by A(b))
    res_BaryGrand$Proj_B.D$SS_B.D_FromTrace[d] <- sum(diag(Hierarchy_of_tables$data$OverWeighted_Pb_GroupCompromise_Pb_array[,,d])) * colSums(DESIGN_tables$mat)[d]
    res_BaryGrand$Proj_B.D$SS_B.D_FromF[d]     <- SS_from_F(res_BaryGrand$Proj_B.D$F_B.D[,,d]) * colSums(DESIGN_tables$mat)[d]
  }
  res_BaryGrand$Proj_B.D$sum_SS_B.D_FromTrace <- sum(res_BaryGrand$Proj_B.D$SS_B.D_FromTrace)
  res_BaryGrand$Proj_B.D$sum_SS_B.D_FromF     <- sum(res_BaryGrand$Proj_B.D$SS_B.D_FromF)






  #Project Barycentric Individuals
  res_BaryGrand$Proj_B.cd$F_B.cd <- array(NA, dim=c(dim(res_BaryGrand$eig$ProjMatb..), DESIGN_tables$CD))
  res_BaryGrand$Proj_B.cd$F_B.cd_Cond <- array(NA, dim=c(DESIGN_rows$B, ncol(res_BaryGrand$eig$ProjMatb..), DESIGN_tables$CD))
  res_BaryGrand$Proj_B.cd$SS_B.cd_fromTrace <- matrix(NA, DESIGN_tables$CD, 1)
  res_BaryGrand$Proj_B.cd$SS_B.cd_fromF     <- matrix(NA, DESIGN_tables$CD, 1)
  for(cd in 1:DESIGN_tables$CD){
    res_BaryGrand$Proj_B.cd$F_B.cd[,,cd]      <- Hierarchy_of_tables$data$OverWeighted_Pb_CP_Pb_array[,,cd] %*% res_BaryGrand$eig$ProjMatb..
    res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,cd] <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$Proj_B.cd$F_B.cd[,,cd]
    #Don't need to scale by C(d), because I'm computing on all participants.
    #Don't need to scale by A(b), because I'm computing on all rows (Fb.., Fb..Cond would need to scale by A(b))
    res_BaryGrand$Proj_B.cd$SS_B.cd_fromTrace[cd]   <- sum(diag(Hierarchy_of_tables$data$OverWeighted_Pb_CP_Pb_array[,,cd]))
    res_BaryGrand$Proj_B.cd$SS_B.cd_fromF[cd]       <- SS_from_F(res_BaryGrand$Proj_B.cd$F_B.cd[,,cd])
  }
  res_BaryGrand$Proj_B.cd$sum_SS_B.cd_fromTrace <- sum(res_BaryGrand$Proj_B.cd$SS_B.cd_fromTrace)
  res_BaryGrand$Proj_B.cd$sum_SS_B.cd_fromF     <- sum(res_BaryGrand$Proj_B.cd$SS_B.cd_fromF)





  #Project OverWeighted GrandCompromose
  res_BaryGrand$Proj_disc..$F_disc.. <- array(NA, dim=c(dim(res_BaryGrand$eig$ProjMatb..), 1))
  res_BaryGrand$Proj_disc..$F_disc..Cond <- array(NA, dim=c(DESIGN_rows$B, ncol(res_BaryGrand$eig$ProjMatb..), 1))
  # res_BaryGrand$Proj_disc..$SS_disc.._fromTrace <- matrix(NA, DESIGN_tables$CD, 1)
  # res_BaryGrand$Proj_disc..$SS_disc.._fromF     <- matrix(NA, DESIGN_tables$CD, 1)
  # for(cd in 1:DESIGN_tables$CD){
  res_BaryGrand$Proj_disc..$F_disc..     <- Hierarchy_of_tables$data$OverWeighted_GrandCompromise %*% res_BaryGrand$eig$ProjMatb..
  res_BaryGrand$Proj_disc..$F_disc..Cond <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$Proj_disc..$F_disc..
  #Need to scale by CD to include the number of participants behind each data point
  res_BaryGrand$Proj_disc..$SS_disc.._fromTrace   <- sum(diag(Hierarchy_of_tables$data$OverWeighted_GrandCompromise)) * DESIGN_tables$CD
  res_BaryGrand$Proj_disc..$SS_disc.._fromF       <- SS_from_F(res_BaryGrand$Proj_disc..$F_disc..) * DESIGN_tables$CD
  # }









  #Project Group Compromises
  res_BaryGrand$Proj_disc.D$F_disc.D      <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..), DESIGN_tables$D))
  res_BaryGrand$Proj_disc.D$F_disc.D_Cond <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..Cond), DESIGN_tables$D))
  res_BaryGrand$Proj_disc.D$SS_disc.D_FromTrace <- matrix(NA, DESIGN_tables$D, 1)
  res_BaryGrand$Proj_disc.D$SS_disc.D_FromF     <- matrix(NA, DESIGN_tables$D, 1)
  for(d in 1:DESIGN_tables$D){
    res_BaryGrand$Proj_disc.D$F_disc.D[,,d]      <- Hierarchy_of_tables$data$OverWeighted_GroupCompromise_array[,,d] %*% res_BaryGrand$eig$ProjMatb..
    res_BaryGrand$Proj_disc.D$F_disc.D_Cond[,,d] <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$Proj_disc.D$F_disc.D[,,d]
    #Need to scale by C(d) to include the number of participants behind each data point
    #Don't need to scale by A(b), because I'm computing on all rows (Fb.., Fb..Cond would need to scale by A(b))
    res_BaryGrand$Proj_disc.D$SS_disc.D_FromTrace[d] <- sum(diag(Hierarchy_of_tables$data$OverWeighted_GroupCompromise_array[,,d])) * colSums(DESIGN_tables$mat)[d]
    res_BaryGrand$Proj_disc.D$SS_disc.D_FromF[d]     <- SS_from_F(res_BaryGrand$Proj_disc.D$F_disc.D[,,d]) * colSums(DESIGN_tables$mat)[d]
  }
  res_BaryGrand$Proj_disc.D$sum_SS_disc.D_FromTrace <- sum(res_BaryGrand$Proj_disc.D$SS_disc.D_FromTrace)
  res_BaryGrand$Proj_disc.D$sum_SS_disc.D_FromF     <- sum(res_BaryGrand$Proj_disc.D$SS_disc.D_FromF)







  #Project Individuals
  res_BaryGrand$Proj_disc.cd$F_disc.cd      <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..), DESIGN_tables$CD))
  res_BaryGrand$Proj_disc.cd$F_disc.cd_Cond <- array(NA, dim=c(dim(res_BaryGrand$eig$Fb..Cond), DESIGN_tables$CD))
  res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromTrace <- matrix(NA, DESIGN_tables$CD, 1)
  res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromF     <- matrix(NA, DESIGN_tables$CD, 1)
  for(cd in 1:DESIGN_tables$CD){
    res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd]    <- Hierarchy_of_tables$data$OverWeighted_CP_array[,,cd] %*% res_BaryGrand$eig$ProjMatb..
    res_BaryGrand$Proj_disc.cd$F_disc.cd_Cond[,,cd] <- DESIGN_rows$Pb_Cond %*% res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd]
    #Don't need to scale by C(d), because I'm computing on all participants.
    #Don't need to scale by A(b), because I'm computing on all rows (Fb.., Fb..Cond would need to scale by A(b))
    res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromTrace[cd] <- sum(diag(Hierarchy_of_tables$data$OverWeighted_CP_array[,,cd]))
    res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromF[cd]     <- SS_from_F(res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd])
  }
  res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromTrace <- sum(res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromTrace)
  res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromF <- sum(res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromF)










  #Need to scale by CD to include the number of participants behind each data point
  res_BaryGrand$Plain$SS_plain_fromTrace   <- sum(diag(Hierarchy_of_tables$data$OverWeighted_GrandCompromise)) * DESIGN_tables$CD

  res_BaryGrand$Plain$SS_plain.d_fromTrace <- matrix(NA, DESIGN_tables$D, 1)
  for(d in 1:DESIGN_tables$D){
    #Need to scale by C(d) to include the number of participants behind each data point
    res_BaryGrand$Plain$SS_plain.d_fromTrace[d] <- sum(diag(Hierarchy_of_tables$data$OverWeighted_GroupCompromise_array[,,d])) * colSums(DESIGN_tables$mat)[d]
  }

  res_BaryGrand$Plain$SS_plain.cd_fromTrace <- matrix(NA, DESIGN_tables$CD, 1)
  for(cd in 1:DESIGN_tables$CD){
    #Need to scale by C(d) to include the number of participants behind each data point
    res_BaryGrand$Plain$SS_plain.cd_fromTrace[cd] <- sum(diag(Hierarchy_of_tables$data$OverWeighted_CP_array[,,cd]))
  }


  #Gather effect sizes into one place.
  #note that the capitalized indices give the number of effects within each (.. is a scalar; .D contains D values; CD contains CD values)
  res_BaryGrand$EffectSize$SS_.b.. <- SS_from_F(res_BaryGrand$eig$Fb..) * DESIGN_tables$CD
  res_BaryGrand$EffectSize$SS_.b.D <- res_BaryGrand$Proj_B.D$SS_B.D_FromF
  res_BaryGrand$EffectSize$SS_.bCD <- res_BaryGrand$Proj_B.cd$SS_B.cd_fromF
  res_BaryGrand$EffectSize$SS_ab.. <- res_BaryGrand$Proj_disc..$SS_disc.._fromF
  res_BaryGrand$EffectSize$SS_ab.D <- res_BaryGrand$Proj_disc.D$SS_disc.D_FromF
  res_BaryGrand$EffectSize$SS_abCD <- res_BaryGrand$Proj_disc.cd$SS_disc.cd_FromF
  res_BaryGrand$EffectSize$SS_plain.. <- res_BaryGrand$Plain$SS_plain_fromTrace
  res_BaryGrand$EffectSize$SS_plain.D <- res_BaryGrand$Plain$SS_plain.d_fromTrace
  res_BaryGrand$EffectSize$SS_plainCD <- res_BaryGrand$Plain$SS_plain.cd_fromTrace
  res_BaryGrand$EffectSize$SS_b_BETWEEN <- sum(res_BaryGrand$EffectSize$SS_.b.D) -     res_BaryGrand$EffectSize$SS_.b..
  res_BaryGrand$EffectSize$SS_b_WITHIN  <- sum(res_BaryGrand$EffectSize$SS_.bCD) - sum(res_BaryGrand$EffectSize$SS_.b.D)

  res_BaryGrand$EffectSize$r2_Categories <- res_BaryGrand$EffectSize$SS_.b.. / res_BaryGrand$EffectSize$SS_ab..
  res_BaryGrand$EffectSize$r2_Groups     <- res_BaryGrand$EffectSize$SS_b_BETWEEN / (res_BaryGrand$EffectSize$SS_b_BETWEEN + res_BaryGrand$EffectSize$SS_b_WITHIN)
  res_BaryGrand$EffectSize$r2_BD_ABCD    <- sum(res_BaryGrand$EffectSize$SS_.b.D) / sum(res_BaryGrand$EffectSize$SS_abCD)

  res_BaryGrand$EffectSize$r2_Plain_Disc_.. <-     res_BaryGrand$EffectSize$SS_ab..  /     res_BaryGrand$EffectSize$SS_plain..
  res_BaryGrand$EffectSize$r2_Plain_Disc_.d <- sum(res_BaryGrand$EffectSize$SS_ab.D) / sum(res_BaryGrand$EffectSize$SS_plain.D)
  res_BaryGrand$EffectSize$r2_Plain_Disc_cd <- sum(res_BaryGrand$EffectSize$SS_abCD) / sum(res_BaryGrand$EffectSize$SS_plainCD)


  res_BaryGrand$r2$disc.cd_B.D <- res_BaryGrand$Proj_B.D$sum_SS_B.D_FromF / res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromF










  #Deviation squared (Group to Grand) (Fb.d to Fb..)
  #### There is a faster way to do this.
  #### Now I have computed the sum_SS above, so can compute these deviations by simple subtraction.
  #### LET's CHECK MY ANSWERS. IF THESE WORK, THEN MY SS ARE BARYCENTRIC, AND MY CRAZY HIERARCHICAL FIGURE IS RIGHT.
  Dev2_Fb.d_Fb.. <- matrix(NA, DESIGN_rows$B, DESIGN_tables$D,
                           dimnames = list(DESIGN_rows$labels, DESIGN_tables$labels))
  for(d in 1:DESIGN_tables$D){
    #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
    Dev2_Fb.d_Fb..[,d] <- diag(Dev2(res_BaryGrand$eig$Fb..Cond, res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d]))
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fb.d_Fb..$Dev2_Fb.d_Fb.. <- Dev2_Fb.d_Fb..
  #And multiply by the number of data points hidden with each data point
  #This is simply Weighting the SS computed on means... need to account for number of stimuli in each category...
  res_BaryGrand$Dev2_Fb.d_Fb..$SS_Dev2_Fb.d_Fb.. <- t(colSums(DESIGN_rows$mat)) %*% Dev2_Fb.d_Fb..
  #And to make it an r2, divide each SS by their total.
  #The null would be that each groups is 1/D away from the grand.
  res_BaryGrand$Dev2_Fb.d_Fb..$r2_Dev2_Fb.d_Fb.. <- res_BaryGrand$Dev2_Fb.d_Fb..$SS_Dev2_Fb.d_Fb.. / sum(res_BaryGrand$Dev2_Fb.d_Fb..$SS_Dev2_Fb.d_Fb..)
  #So for composers, Group Mid explains 4% of between-group differences, whereas Group High explains 59%.









  #Deviation squared (disc.cd to b.cd)
  Dev2_Fdisc.cd_Fb.cd <- matrix(NA, DESIGN_rows$AB, DESIGN_tables$CD,
                                dimnames = list(rownames(DESIGN_rows$mat), rownames(DESIGN_tables$mats)))
  for(cd in 1:DESIGN_tables$CD){
    #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
    Dev2_Fdisc.cd_Fb.cd[,cd] <- diag(Dev2(res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd], res_BaryGrand$Proj_B.cd$F_B.cd[,,cd]))
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fdisc.cd_Fb.cd$Dev2_Fdisc.cd_Fb.cd <- Dev2_Fdisc.cd_Fb.cd
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fdisc.cd_Fb.cd$SS_Dev2_Fdisc.cd_Fb.cd <- matrix(1, 1, DESIGN_rows$AB) %*% Dev2_Fdisc.cd_Fb.cd %*% matrix(1, DESIGN_tables$CD, 1)









  #Deviation squared (disc.d to b.d)
  Dev2_Fdisc.d_Fb.d <- matrix(NA, DESIGN_rows$AB, DESIGN_tables$D,
                              dimnames = list(rownames(DESIGN_rows$mat), colnames(DESIGN_tables$mats)))
  for(d in 1:DESIGN_tables$D){
    #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
    Dev2_Fdisc.d_Fb.d[,d] <- diag(Dev2(res_BaryGrand$Proj_disc.D$F_disc.D[,,d], res_BaryGrand$Proj_B.D$F_B.D[,,d]))
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fdisc.d_Fb.d$Dev2_Fdisc.d_Fb.d <- Dev2_Fdisc.d_Fb.d
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fdisc.d_Fb.d$SS_Dev2_Fdisc.d_Fb.d <- matrix(1, 1, DESIGN_rows$AB) %*% Dev2_Fdisc.d_Fb.d %*% colSums(DESIGN_tables$mat)







  #Deviation squared (disc.. to b..)
  Dev2_Fdisc.._Fb.. <- matrix(NA, DESIGN_rows$AB, 1,
                              dimnames = list(rownames(DESIGN_rows$mat), paste0('BaryGrand')))
  # for(d in 1:DESIGN_tables$D){
  #The squared distance from a given row of Fdisc.. to the corresponding row of Fb..
  Dev2_Fdisc.._Fb..[,1] <- diag(Dev2(res_BaryGrand$Proj_disc..$F_disc.., res_BaryGrand$eig$Fb..))
  # }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fdisc.._Fb..$Dev2_Fdisc.._Fb.. <- Dev2_Fdisc.._Fb..
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fdisc.._Fb..$SS_Dev2_Fdisc.._Fb.. <- matrix(1, 1, DESIGN_rows$AB) %*% Dev2_Fdisc.._Fb.. * DESIGN_tables$CD









  #Deviation squared (disc.cd to disc.d)
  Dev2_Fdisc.cd_Fdisc.d <- matrix(NA, DESIGN_rows$AB, DESIGN_tables$CD,
                                  dimnames = list(rownames(DESIGN_rows$mat), rownames(DESIGN_tables$mats)))

  for(d in 1:DESIGN_tables$D){
    for(cd in 1:colSums(DESIGN_tables$mat)[d]){

      which_table <- which(DESIGN_tables$mat[,d]==1)[cd]
      #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
      Dev2_Fdisc.cd_Fdisc.d[,which_table] <- diag(Dev2(res_BaryGrand$Proj_disc.cd$F_disc.cd[,,which_table], res_BaryGrand$Proj_disc.D$F_disc.D[,,d]))
    }
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fdisc.cd_Fdisc.d$Dev2_Fdisc.cd_Fdisc.d <- Dev2_Fdisc.cd_Fdisc.d
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fdisc.cd_Fdisc.d$SS_Dev2_Fdisc.cd_Fdisc.d <- matrix(1, 1, DESIGN_rows$AB) %*% Dev2_Fdisc.cd_Fdisc.d %*% matrix(1, DESIGN_tables$CD, 1)









  #Deviation squared (b.cd to b.d)
  Dev2_Fb.cd_Fb.d <- matrix(NA, DESIGN_rows$B, DESIGN_tables$CD,
                            dimnames = list(colnames(DESIGN_rows$mat), rownames(DESIGN_tables$mats)))

  for(d in 1:DESIGN_tables$D){
    for(cd in 1:colSums(DESIGN_tables$mat)[d]){

      which_table <- which(DESIGN_tables$mat[,d]==1)[cd]
      #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
      Dev2_Fb.cd_Fb.d[,which_table] <- diag(Dev2(res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,which_table], res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d]))
    }
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fb.cd_Fb.d$Dev2_Fb.cd_Fb.d <- Dev2_Fb.cd_Fb.d
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fb.cd_Fb.d$SS_Dev2_Fb.cd_Fb.d <- colSums(DESIGN_rows$mat) %*% Dev2_Fb.cd_Fb.d %*% matrix(1, DESIGN_tables$CD, 1)








  #Deviation squared (disc.d to disc..)
  Dev2_Fdisc.d_Fdisc.. <- matrix(NA, DESIGN_rows$AB, DESIGN_tables$D,
                                 dimnames = list(rownames(DESIGN_rows$mat), colnames(DESIGN_tables$mats)))

  for(d in 1:DESIGN_tables$D){
    #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
    Dev2_Fdisc.d_Fdisc..[,d] <- diag(Dev2(res_BaryGrand$Proj_disc.D$F_disc.D[,,d], res_BaryGrand$Proj_disc..$F_disc..))
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fdisc.d_Fdisc..$Dev2_Fdisc.d_Fdisc.. <- Dev2_Fdisc.d_Fdisc..
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fdisc.d_Fdisc..$SS_Dev2_Fdisc.d_Fdisc.. <- matrix(1, 1, DESIGN_rows$AB) %*% Dev2_Fdisc.d_Fdisc.. %*% colSums(DESIGN_tables$mat)










  #Deviation squared (b.cd to b.d)
  Dev2_Fb.d_Fb.. <- matrix(NA, DESIGN_rows$B, DESIGN_tables$D,
                           dimnames = list(colnames(DESIGN_rows$mat), colnames(DESIGN_tables$mats)))

  for(d in 1:DESIGN_tables$D){
    #The squared distance from a given row of Fb.. to the corresponding row of Fb.d
    Dev2_Fb.d_Fb..[,d] <- diag(Dev2(res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], res_BaryGrand$eig$Fb..Cond))
  }

  #For each stimulus, what's the dev2 from a given group to the grand?
  res_BaryGrand$Dev2_Fb.d_Fb..$Dev2_Fb.d_Fb.. <- Dev2_Fb.d_Fb..
  #And multiply by the number of data points hidden with each data point
  res_BaryGrand$Dev2_Fb.d_Fb..$SS_Dev2_Fb.d_Fb.. <- colSums(DESIGN_rows$mat) %*% Dev2_Fb.d_Fb.. %*% colSums(DESIGN_tables$mat)










  # #Deviation squared (Participant to Group)
  # Dev2_Fb.d_Fbcd <- matrix(NA, DESIGN_rows$B, DESIGN_tables$CD,
  #                          dimnames = list(DESIGN_rows$labels, rownames(DESIGN_tables$mat)))
  # for(d in 1:DESIGN_tables$D){
  #   for(cd in 1:(colSums(DESIGN_tables$mat)[d])){
  #
  #      which_table <- which(DESIGN_tables$mat[,d]==1)[cd]
  #      #The squared distance from a given row of Fb.d to the corresponding row of Fbcd
  #      Dev2_Fb.d_Fbcd[,which_table] <- diag(Dev2(res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,which_table]))
  #
  #    }
  #  }
  #
  #  res_BaryGrand$Dev2$Dev2_Participant_to_Group <- colSums(Dev2_Fb.d_Fbcd)










  X <- " "
  res_BaryGrand$SS_summary <- data.frame(rbind(c(X, "C(D)", ".(D)", "Within-group", X),
                                               c("Disc", round(res_BaryGrand$Proj_disc.cd$sum_SS_disc.cd_FromF,3), round(res_BaryGrand$Proj_disc.D$sum_SS_disc.D_FromF, 3), round(res_BaryGrand$Dev2_Fdisc.cd_Fdisc.d$SS_Dev2_Fdisc.cd_Fdisc.d, 3), X),
                                               c("B", round(res_BaryGrand$Proj_B.cd$sum_SS_B.cd_fromF, 3), round(res_BaryGrand$Proj_B.D$sum_SS_B.D_FromF, 3), round(res_BaryGrand$Dev2_Fb.cd_Fb.d$SS_Dev2_Fb.cd_Fb.d, 3), X),
                                               c("A(B)", round(res_BaryGrand$Dev2_Fdisc.cd_Fb.cd$SS_Dev2_Fdisc.cd_Fb.cd, 3), round(res_BaryGrand$Dev2_Fdisc.d_Fb.d$SS_Dev2_Fdisc.d_Fb.d, 3), X, X),
                                               c(X, X, X, X, X),
                                               c(X, X, ".(D)", ".(.)", "Between-group"),
                                               c(X, "Disc", round(res_BaryGrand$Proj_disc.D$sum_SS_disc.D_FromF, 3), round(res_BaryGrand$Proj_disc..$SS_disc.._fromF, 3), round(res_BaryGrand$Dev2_Fdisc.d_Fdisc..$SS_Dev2_Fdisc.d_Fdisc.., 3)),
                                               c(X, "B", round(res_BaryGrand$Proj_B.D$sum_SS_B.D_FromF, 3), round(res_BaryGrand$eig$SS_B.._fromF, 3), round(res_BaryGrand$Dev2_Fb.d_Fb..$SS_Dev2_Fb.d_Fb.., 3)),
                                               c(X, "A(B)", round(res_BaryGrand$Dev2_Fdisc.d_Fb.d$SS_Dev2_Fdisc.d_Fb.d, 3), round(res_BaryGrand$Dev2_Fdisc.._Fb..$SS_Dev2_Fdisc.._Fb.., 3), X)))







  #Fixed Confusion - Rows
  # ###Fixed effects. Predict old observations. For the grand compromise.
  Prediction_Fixed_Rows <- list()

  #Compute d2 from stimulus a(b) to all categories B (to give an a(b)xB matrix)
  Dev2_ab_2_B <- Dev2(res_BaryGrand$Proj_disc..$F_disc.., res_BaryGrand$eig$Fb..Cond)

  #Assign ab to B (identify which B is closest to each ab)
  prediction_rows_vec <- DESIGN_rows$labels[apply(Dev2_ab_2_B, 1, which.min)]

  #Transform the prediction into a design matrix
  prediction_rows_mat <- makeNominalData(as.matrix(prediction_rows_vec))[,paste0('.',DESIGN_rows$labels)]
  rownames(prediction_rows_mat) <- rownames(Hierarchy_of_tables$data$Bary_GrandCompromise)
  colnames(prediction_rows_mat) <- DESIGN_rows$labels
  Prediction_Fixed_Rows$prediction_rows_mat <- prediction_rows_mat

  #Transform the design matrix to give a confusion matrix
  Confusion_Rows <- t(DESIGN_rows$mat) %*% prediction_rows_mat
  rownames(Confusion_Rows) <- paste0(DESIGN_rows$labels, "_actual")
  colnames(Confusion_Rows) <- paste0(DESIGN_rows$labels, "_predicted")
  Prediction_Fixed_Rows$Confusion_Rows <- Confusion_Rows

  Prediction_Fixed_Rows$Confusion_Rows_norm <- round(Prediction_Fixed_Rows$Confusion_Rows / rowSums(Prediction_Fixed_Rows$Confusion_Rows), 2)










  #Fixed Confusion - Rows from the Groups' perspectives
  ###Fixed effects. Predict old observations. For the GROUP compromises.
  Prediction_Fixed_Rows$prediction_rows_mat.d <- array(0, dim=c(DESIGN_rows$AB, DESIGN_rows$B, DESIGN_tables$D),
                                                       dimnames = list(rownames(Hierarchy_of_tables$data$Bary_GrandCompromise), paste0(DESIGN_rows$labels, "_predicted"), DESIGN_tables$labels))
  Prediction_Fixed_Rows$Confusion.d <- array(NA, dim=c(DESIGN_rows$B, DESIGN_rows$B, DESIGN_tables$D),
                                             dimnames = list(paste0(DESIGN_rows$labels, "_actual"), paste0(DESIGN_rows$labels, "_predicted"), DESIGN_tables$labels))

  Prediction_Fixed_Rows$Confusion_norm.d <- array(NA, dim=c(DESIGN_rows$B, DESIGN_rows$B, DESIGN_tables$D),
                                                  dimnames = list(paste0(DESIGN_rows$labels, "_actual"), paste0(DESIGN_rows$labels, "_predicted"), DESIGN_tables$labels))

  for(d in 1:DESIGN_tables$D){
    #Compute d2 from stimulus a(b) to all categories B (to give an a(b)xB matrix)
    Dev2_ab_2_B.d <- Dev2(res_BaryGrand$Proj_disc.D$F_disc.D[,,d], res_BaryGrand$eig$Fb..Cond)

    #Assign ab to B (identify which B is closest to each ab)
    prediction_rows_vec.d <- DESIGN_rows$labels[apply(Dev2_ab_2_B.d, 1, which.min)]

    #Transform the prediction into a design matrix
    these_categories <- which(DESIGN_rows$labels %in% prediction_rows_vec.d)
    Prediction_Fixed_Rows$prediction_rows_mat.d[,these_categories,d] <- makeNominalData(as.matrix(prediction_rows_vec.d))[,paste0('.',DESIGN_rows$labels[these_categories])]

    #Transform the design matrix to give a confusion matrix
    Prediction_Fixed_Rows$Confusion.d[,,d] <- t(DESIGN_rows$mat) %*% Prediction_Fixed_Rows$prediction_rows_mat.d[,,d]

    Prediction_Fixed_Rows$Confusion_norm.d[,,d] <- round(Prediction_Fixed_Rows$Confusion.d[,,d] / rowSums(Prediction_Fixed_Rows$Confusion.d[,,d]), 2)
  }



  res_BaryGrand$Prediction_Fixed_Rows <- Prediction_Fixed_Rows







  #Fixed Confusion - Tables
  Prediction_Fixed_Tables <- list()
  Prediction_Fixed_Tables$prediction_tables_mat.b  <- array(0, dim=c(DESIGN_rows$B, DESIGN_tables$CD, DESIGN_tables$D),
                                                            dimnames=list(DESIGN_rows$labels, rownames(DESIGN_tables$mat), DESIGN_tables$labels))
  Prediction_Fixed_Tables$Confusion.b <- array(NA, dim=c(DESIGN_rows$B, DESIGN_tables$D, DESIGN_tables$D),
                                               dimnames = list(DESIGN_rows$labels, paste0(DESIGN_tables$labels, "_actual"), paste0(DESIGN_tables$labels, "_predicted")))


  #predict tables, for each B
  for(b in 1:DESIGN_rows$B){
    #Compute d2 from participant c(d) to all Groups D (to give an c(d)xD matrix)
    Dev2_cd_2_D.b <- Dev2(t(res_BaryGrand$Proj_B.cd$F_B.cd_Cond[b,,]), t(res_BaryGrand$Proj_B.D$F_B.D_Cond[b,,]))

    #Assign ab to B (identify which B is closest to each ab)
    prediction_tables_vec.b <- DESIGN_tables$labels[apply(Dev2_cd_2_D.b, 1, which.min)]

    #Transform the prediction into a design matrix
    # Prediction_Fixed_Tables$prediction_tables_mat.b[b,,] <- makeNominalData(as.matrix(prediction_tables_vec.b))[,paste0('.',DESIGN_tables$labels)]
    #########################################
    for(d in 1:DESIGN_tables$D){
      #if  a certain column exists in prediction_tables_vec.b, then paste it into the corresponding column
      if(DESIGN_tables$labels[d] %in% prediction_tables_vec.b){
        Prediction_Fixed_Tables$prediction_tables_mat.b[b,,d] <- makeNominalData(as.matrix(prediction_tables_vec.b))[,paste0('.',DESIGN_tables$labels[d])]
      }

    }

    #Transform the design matrix to give a confusion matrix
    Prediction_Fixed_Tables$Confusion.b[b,,] <- t(DESIGN_tables$mat) %*% Prediction_Fixed_Tables$prediction_tables_mat.b[b,,]
  }


  ##First, sum across the tables of Prediction_array, to show the cumulative results of the LOO_rows...
  Prediction_Fixed_Tables$Confusion.b_sum <- apply(Prediction_Fixed_Tables$Confusion.b, c(2,3), sum)
  Prediction_Fixed_Tables$Confusion.b_sum_norm <- round(Prediction_Fixed_Tables$Confusion.b_sum / rowSums(Prediction_Fixed_Tables$Confusion.b_sum), 2)

  res_BaryGrand$Prediction_Fixed_Tables <- Prediction_Fixed_Tables














  return(res_BaryGrand)

}
