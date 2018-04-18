#PlotDiSTATIS.R

#' Plot results of DiSTATIS
#'
#' @param res_DiSTATIS The output of EigenDiSTATIS
#' @param axes Axes to plot. By default, c(1,2)
#' @param main Title of factor map
#' @export

PlotDiSTATIS <- function(res_DiSTATIS, axes = c(1,2), main=NULL){

  DESIGN_rows   <- res_DiSTATIS$input$DESIGN$rows$Composers_mat
  DESIGN_tables <- res_DiSTATIS$input$DESIGN$tables$MusExp_mat
  t             <- res_DiSTATIS$eig$t
  Ctrb          <- res_DiSTATIS$eig$Ctrb


  # Compromise
  F <- res_DiSTATIS$eig$F  #* -1
  F_Berries <- Condense_Rows(F[,axes], DESIGN_rows)
  F_Berries_verbose <- Bary_Projector(DESIGN_rows) %*% F[,axes]


  # PFS
  Fk  <- res_DiSTATIS$ProjCP$F #*-1   #Note, "ProjGroup"
  Fk_Berries <- array(NA, dim=c(B, length(axes), K))
  for(k in 1:K){
    Fk_Berr <- paste0("Fk_Berries[,,",k,"]         <- Condense_Rows(Fk[,axes,",k,"], DESIGN_rows)")
    eval(parse(text = Fk_Berr))
  }
  dimnames(Fk_Berries) <- list(colnames(DESIGN_rows), colnames(F)[axes], rownames(DESIGN_tables))



  #Colors
  colors_D      <- DESIGN$tables$MusExp_colors_D
  colors_CD     <- DESIGN$tables$MusExp_colors_CD
  colors_K <- colors_CD

  colors_B      <- DESIGN$rows$Composer_colors_B
  colors_AB     <- DESIGN$rows$Composer_colors_AB


  #constraints
  Fk_rows <- matrix(NA, A*K, ncol(F))
  for(k in 1:K){
    from <- 1 + (A*(k-1))
    to <- A * k
    these_rows <- from:to
    Fk_rows[these_rows,] <- Fk[,,k]
  }

  constraints_Comp <- minmaxHelper(mat1 = F,
                                   axis1 = axes[1], axis2 = axes[2])

  constraints_PFS <- minmaxHelper(mat1 = rbind(F, Fk_rows), #mat1 = rbind(F, Fk[,,1], Fk[,,2], Fk[,,3]),
                                  axis1 = axes[1], axis2 = axes[2])

  if(!is.null(main)){
    main <- paste0(main, ": ")
  }

  #Map 1: Compromise
  prettyGraphs::prettyPlot(F[,axes],
                           #dev.new=FALSE, new.plot=TRUE,
                           display_names = TRUE, cex=1.5,
                           constraints = constraints_Comp,
                           #contributionCircles = TRUE, contributions = Ctrb,
                           xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
                           ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
                           col = colors_AB,
                           pch=21,
                           # In future, change this to compute the within-group average, and use colors_B
                           main = paste0(main,'Compromise'))

  text(F_Berries, col = colors_B, labels = colnames(DESIGN_rows))


  #Map 2: PFS
  k <- 3
  # for(k in 1:K){

    PlotFk <- paste0("prettyGraphs::prettyPlot(Fk[,axes,",k,"],
                     #dev.new=FALSE, new.plot=TRUE,
                     display_names = TRUE, cex=1.5,
                     constraints = constraints_PFS,
                     #contributionCircles = TRUE, contributions = Ctrb,
                     # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
                     # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
                     xlab = paste0('Component ', axes[1]),
                     ylab = paste0('Component ', axes[2]),
                     col = colors_AB,
                     pch=21,
                     # In future, change this to compute the within-group average, and use colors_B
                     main = paste0(main,'PFS for k=', '",rownames(DESIGN$tables$MusExp_mat)[k],"'))")
    eval(parse(text = PlotFk))

    AndTheNames <- paste0("text(Fk_Berries[,,",k,"], col = colors_B, labels = colnames(DESIGN_rows))")
    eval(parse(text = AndTheNames))

  # }


  #Just show that 1 observation in the compromise is the average of the PFS
  Ss <- c(20,33)
  prettyGraphs::prettyPlot(F[Ss,axes],
                           #dev.new=FALSE, new.plot=TRUE,
                           display_names = TRUE, cex=2,
                           constraints = constraints_PFS,
                           #contributionCircles = TRUE, contributions = Ctrb,
                           xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
                           ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
                           col = colors_AB[Ss],
                           pch=21,
                           # In future, change this to compute the within-group average, and use colors_B
                           main = paste0(main,'PFS (a=20)'))


  for(k in 1:K){
    prettyGraphs::prettyPlot(Fk[Ss, axes, k],
                             dev.new=FALSE, new.plot=FALSE,
                             display_names = FALSE,
                             col = colors_AB[Ss],
                             pch=21)
    segments(F[Ss,axes[1]], F[Ss,axes[2]],
             Fk[Ss, axes[1], k], Fk[Ss, axes[2], k],
             col = colors_AB[Ss])
  }
  
  
  
  # Currently broken
  # #4. Plot PFS_d  
  # #Getting messy in here....
  # res_DiSTATIS$Fd <- array(NA, dim=c(nrow(F), ncol(F), ncol(DESIGN$tables$MusExp_mat)))
  # res_DiSTATIS$Fd[,,1]  <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,1]==1)], c(1,2), mean)
  # res_DiSTATIS$Fd[,,2]  <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,2]==1)], c(1,2), mean)
  # res_DiSTATIS$Fd[,,3] <- apply(res_DiSTATIS$ProjCP$F[,,which(DESIGN$tables$MusExp_mat[,3]==1)], c(1,2), mean)
  # 
  # for(d in 1:ncol(DESIGN$tables$MusExp_mat)){
  # 
  # PlotFd <- paste0("prettyGraphs::prettyPlot(res_DiSTATIS$Fd[,axes,",d,"],
  #                    #dev.new=FALSE, new.plot=TRUE,
  #                    display_names = FALSE, cex=1.5,
  #                    constraints = constraints_PFS,
  #                    #contributionCircles = TRUE, contributions = Ctrb,
  #                    # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                    # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                    xlab = paste0('Component ', axes[1]),
  #                    ylab = paste0('Component ', axes[2]),
  #                    col = colors_AB,
  #                    pch=21,
  #                    # In future, change this to compute the within-group average, and use colors_B
  #                    main = paste0(main,'PFS for k=', '",rownames(DESIGN$tables$MusExp_mat)[k],"'))")
  # eval(parse(text = PlotFd))
  # 
  # AndTheNames <- paste0("text(Fk_Berries[,,",k,"], col = colors_B, labels = colnames(DESIGN_rows))")
  # eval(parse(text = AndTheNames))
  # 
  # # }


  #To explore these results,
  #1) Show that a single point in the grand comp is the (weighted) barycenter of the 3 group compromises
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
