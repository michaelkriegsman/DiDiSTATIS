# Run everything line by line, including the top inside of PlotHiDiSTATIS.R

# Here are some, not all, plots


#Grand Compromise with square berries
prettyGraphs::prettyPlot(F[,axes],
                         #dev.new=FALSE, new.plot=TRUE,
                         display_names = FALSE, cex=1.5,
                         constraints = constraints_Groups,
                         #contributionCircles = TRUE, contributions = Ctrb,
                         xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
                         ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
                         col = colors_AB,
                         pch=21,
                         # In future, change this to compute the within-group average, and use colors_B
                         main = paste0(main,'Grand Compromise'))

prettyGraphs::prettyPlot(F_Berries[,axes], new.plot = FALSE, dev.new = FALSE,
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B,
                         pch=22)


#The grand Daddy
prettyGraphs::prettyPlot(F_Berries[,axes], 
                         constraints = minmaxHelper(Fd_Berries[,,3]),
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B,
                         pch=22)

for(d in 1:D){
  
  #color by colors_D
  ConnectTheDots <- paste0("segments(Fd_Berries[,axes[1],",d,"], Fd_Berries[,axes[2],",d,"], F_Berries[,axes[1]], F_Berries[,axes[2]], colors_D[d])")
  eval(parse(text = ConnectTheDots))
  
  
  prettyPlot()
  # PlotFd <- paste0("prettyGraphs::prettyPlot(Fd_Berries[,axes,",d,"],
  #                  dev.new=FALSE, new.plot=FALSE,
  #                  display_names = FALSE, cex=1.5,
  #                  constraints=constraints_Groups,
  #                  #contributionCircles = TRUE, contributions = Ctrb,
  #                  # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                  # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                  xlab = paste0('Component ', axes[1]),
  #                  ylab = paste0('Component ', axes[2]),
  #                  col = colors_D[d], #colors_AB,
  #                  pch=21,
  #                  # In future, change this to compute the within-group average, and use colors_B
  #                  main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  # eval(parse(text = PlotFd))
  
}

