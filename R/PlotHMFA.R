#PlotHMFA.R

#' Plot results of HMFA
#'
#' @param res_HMFA The output of EigenHMFA
#' @param axes Axes to plot. By default, c(1,2)
#' @param main Title of factor map
#' @export

PlotHMFA <- function(res_HMFA, axes = c(1,2), main=NULL){

  DESIGN_rows   <- res_HMFA$input$DESIGN$rows$Composers_mat
  DESIGN_tables <- res_HMFA$input$DESIGN$tables$MusExp_mat
  t             <- res_HMFA$eig$t
  Ctrb          <- res_HMFA$eig$Ctrb


  #Grand Consensus
  F <- res_HMFA$eig$F
  FlipComps <- 2 #Gonna need to clean this later... maybe include it in the input parameters
  F[,FlipComps] <- F[,FlipComps] * -1
  F_Berries <- Condense_Rows(F[,axes], DESIGN_rows)
  F_Berries_verbose <- Bary_Projector(DESIGN_rows) %*% F[,axes]


  #Group Consensuses
  Fd  <- res_HMFA$ProjGroup$F   #Note, "ProjGroup"
  Fd[,FlipComps,] <- Fd[,FlipComps,] * -1
  Fd_Berries <- array(NA, dim=c(B, length(axes), D))
  for(d in 1:D){
    Fd_Berr <- paste0("Fd_Berries[,axes,",d,"]         <- Condense_Rows(Fd[,axes,",d,"], DESIGN_rows)")
    eval(parse(text = Fd_Berr))
  }
  dimnames(Fd_Berries) <- list(colnames(DESIGN_rows), colnames(F)[axes], colnames(DESIGN_tables))


  #Individual Data
  Fcd           <- res_HMFA$ProjCP$F*-1
  Fcd[,FlipComps,] <- Fcd[,FlipComps,] * -1


  #Colors
  colors_D      <- DESIGN$tables$MusExp_colors_D
  colors_CD     <- DESIGN$tables$MusExp_colors_CD

  colors_B      <- DESIGN$rows$Composer_colors_B
  colors_AB     <- DESIGN$rows$Composer_colors_AB



  Fd_rows <- matrix(NA, A*D, ncol(F))
  for(d in 1:D){
    from <- 1 + (A*(d-1))
    to <- A * d
    these_rows <- from:to
    Fd_rows[these_rows,] <- Fd[,,d]
  }

  Fcd_rows <- matrix(NA, A*CD, ncol(F))
  for(cd in 1:CD){
    from <- 1 + (A*(cd-1))
    to <- A * cd
    these_rows <- from:to
    Fcd_rows[these_rows,] <- Fcd[,,cd]
  }

  #constraints
  constraints_Cons <- minmaxHelper(mat1 = F, #mat1 = rbind(F, Fk[,,1], Fk[,,2], Fk[,,3]),
                                   axis1 = axes[1], axis2 = axes[2])

  constraints_Groups <- minmaxHelper(mat1 = rbind(F, Fd_rows),
                                   axis1 = axes[1], axis2 = axes[2])


  constraints_PFS <- minmaxHelper(mat1 = rbind(F, Fcd_rows), #mat1 = rbind(F, Fk[,,1], Fk[,,2], Fk[,,3]),
                                  axis1 = axes[1], axis2 = axes[2])


  if(!is.null(main)){
    main <- paste0(main, ": ")
  }

  #Map 1: Grand Consensus
  prettyGraphs::prettyPlot(F[,axes],
                           #dev.new=FALSE, new.plot=TRUE,
                           display_names = FALSE, cex=1.5,
                           constraints = constraints_Groups,
                           #contributionCircles = TRUE, contributions = Ctrb,
                           xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
                           ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
                           col = 'black', #colors_AB,
                           pch=21,
                           # In future, change this to compute the within-group average, and use colors_B
                           main = paste0(main,'Grand Consensus'))

  text(F_Berries, col = colors_B, labels = colnames(DESIGN_rows))


  #Map 2: Group Consensus
  for(d in 1:D){

    PlotFd <- paste0("prettyGraphs::prettyPlot(Fd[,axes,",d,"],
                     #dev.new=FALSE, new.plot=TRUE,
                     display_names = FALSE, cex=1.5,
                     constraints=constraints_Groups,
                     #contributionCircles = TRUE, contributions = Ctrb,
                     # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
                     # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
                     xlab = paste0('Component ', axes[1]),
                     ylab = paste0('Component ', axes[2]),
                     col = colors_AB,
                     pch=21,
                     # In future, change this to compute the within-group average, and use colors_B
                     main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
    eval(parse(text = PlotFd))

    AndTheNames <- paste0("text(Fd_Berries[,,",d,"], col = colors_B, labels = colnames(DESIGN_rows))")
    eval(parse(text = AndTheNames))

  }

  #Map 3: Group Consensus with Consensus
  for(d in 1:D){
    
    PlotF <- paste0("prettyGraphs::prettyPlot(F[,axes],
                     #dev.new=FALSE, new.plot=TRUE,
                     display_names = FALSE, cex=1,
                     constraints=constraints_Groups,
                     #contributionCircles = TRUE, contributions = Ctrb,
                     # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
                     # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
                     xlab = paste0('Component ', axes[1]),
                     ylab = paste0('Component ', axes[2]),
                     col = 'black', #gray
                     pch=21,
                     # In future, change this to compute the within-group average, and use colors_B
                     main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
    eval(parse(text = PlotF))
    
    #AndTheNames <- paste0("text(Fd_Berries[,,",d,"], col = colors_B, labels = colnames(DESIGN_rows))")
    #eval(parse(text = AndTheNames))
    
    #color by colors_AB
    #ConnectTheDots <- paste0("segments(Fd[,axes[1],",d,"], Fd[,axes[2],",d,"], F[,axes[1]], F[,axes[2]], colors_AB)")
    #eval(parse(text = ConnectTheDots))
    
    #color by colors_D
    ConnectTheDots <- paste0("segments(Fd[,axes[1],",d,"], Fd[,axes[2],",d,"], F[,axes[1]], F[,axes[2]], colors_D[d])")
    eval(parse(text = ConnectTheDots))
    
    PlotFd <- paste0("prettyGraphs::prettyPlot(Fd[,axes,",d,"],
                     dev.new=FALSE, new.plot=FALSE,
                     display_names = FALSE, cex=1.5,
                     constraints=constraints_Groups,
                     #contributionCircles = TRUE, contributions = Ctrb,
                     # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
                     # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
                     xlab = paste0('Component ', axes[1]),
                     ylab = paste0('Component ', axes[2]),
                     col = colors_D[d], #colors_AB,
                     pch=21,
                     # In future, change this to compute the within-group average, and use colors_B
                     main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
    eval(parse(text = PlotFd))
    
  }
  #To explore these results,
  #1) Show that a single point in the grand comp is the (weighted) barycenter of the 3 group Consensuss
  #and set the Ctrb (the size of the points) as the weight of the group comp
  #though, this won't change much, because the group weights are all nearly 0.33

  #1.5) For completeness sake, show that for an individual stimulus,
  #the grand is the weighted barycenter of the group,
  #and the group is the wieghted barycenter of the individuals in that group

  #2) To actually interpret these data... Average rows of F (and rows of Fd) within composers.
  #Later, average rows within composer*pianist (12 data points)
  #Until I do inference, just include the tolerance intervals so that a composer (or composer*pianist) visually occupies it's spread.


  #Another question we'd like to ask... For each group of participants, are the composers or are the pianists for descriptive?


}
