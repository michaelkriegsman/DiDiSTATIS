#'Plot results from SH cross-validation of Tables for HiDiSTATIS
#'
#'@param res_HiDiSTATIS Output from HiDiSTATIS with SH_tables==TRUE
#'@return Output
#'@export


PlotHiDiSTATIS_SH_Tables <- function(res_HiDiSTATIS){

  PlotConfusion(res_HiDiSTATIS$SH_Tables$Confusion_rand_norm)

  #Plot the last iteration, in it's LeftIn space
  XandY <- c(1,2)
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY],
             col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB,
             constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[,XandY,])),
             main = "The LeftIn Grand Compromise, for a single SH iteration")




  #Plot the last iteration, in it's LeftIn space, and the LeftIn groups
  XandY <- c(1,2)
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY],
             col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB,
             constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[,XandY,])),
             main = "The LeftIn Group Compromises, for a single SH iteration")
  for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[,XandY,d],
               dev.new = F, new.plot = F,
               display_names = F,
               col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    segments(x0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[1]],
             y0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[2]],
             x1 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[,XandY[1],d],
             y1 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[,XandY[2],d],
             col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])

  }



  #Plot the last iteration, in it's LeftIn space,
  #and project in the LeftOut tables
  XandY <- c(1,2)
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY],
             display_names = F,
             col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB,
             constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY,])),
             main = "The LeftIn Grand Compromises, with LeftOut tables")
  for(cd_out in 1:res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$CD_out){
    prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY,cd_out],
               dev.new = F, new.plot = F,
               display_names = F,
               pch = 19, cex = 2,
               col = add.alpha(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$colors_CD_LeftOut[cd_out], .4))
    # pause()
  }


  #"1 participant provides this info"
  #Plot the last iteration, in it's LeftIn space,
  #and project in a single LeftOut table
  XandY <- c(1,2)
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY],
             display_names = F,
             col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB,
             main = "The LeftIn Grand Compromises, with a single LeftOut table")
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY,1],
             dev.new = F, new.plot = F,
             pch = 19, cex = 2,
             col = add.alpha(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$colors_CD_LeftOut[1], .4))
  segments(x0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[1]],
           y0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[2]],
           x1 = res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY[1],1],
           y1 = res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY[2],1],
           col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])




  #"And we want to see, 1 stimulus at a time, which group perspective is closest to that participant's perspective"
  XandY <- c(1,2)
  ab <- 1
  cd_out <- 1
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[c(ab,ab),XandY],
             col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB[ab],
             constraints = minmaxHelper(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY]),
             main = "1 row of LeftIn Group and Grand Compromises, and 1 LeftOut participants")
  for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[c(ab,ab),XandY,d],
               dev.new = F, new.plot = F, display_names = F,
               col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    segments(x0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[ab,XandY[1]],
             y0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[ab,XandY[2]],
             x1 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[ab,XandY[1],d],
             y1 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$ProjGroup$F[ab,XandY[2],d],
             col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
  }
  prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[c(ab,ab), XandY, cd_out],
             dev.new = F, new.plot = F, display_names = F,
             cex = 2, col = res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$colors_CD_LeftOut[cd_out])



  # #Same plot, with segments
  #
  # for(cd_out in 1:res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$CD_out){
  #   prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F, col=res_HiDiSTATIS$input$DESIGN_rows$colors_AB)
  #
  #   prettyPlot(res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,,cd_out],
  #              dev.new = F, new.plot = F,
  #              pch = 19,
  #              col = add.alpha(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$colors_CD_LeftOut[cd_out], .4))
  #
  #   segments(x0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[1]],
  #            y0 = res_HiDiSTATIS$SH_Tables$Last_iter$res_GrandComp_LeftIn$eig$F[,XandY[2]],
  #            x1 = res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY[1],cd_out],
  #            y1 = res_HiDiSTATIS$SH_Tables$Last_iter$F_LeftOut_Tables[,XandY[2],cd_out],
  #            col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])
  # }










# This makes more sense for LOO... and makes more sense to plot with a for_loop
  # #plot individual participants, for a given SH iteration (i)
  # i <- 3
  # XandY <- c(1,2)
  # prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F)
  # # for(Cd_out in 1:res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat))
  # prettyPlot(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[1,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])
  # segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[1,i],i], y1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[1,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[1])
  # prettyPlot(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[2,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[2])
  # segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[2,i],i], y1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[2,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[2])
  # prettyPlot(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,,res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[3,i],i], cex=2, dev.new = F, new.plot = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[3])
  # segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[1]], y0 = res_HiDiSTATIS$res_GrandComp$eig$F[,XandY[2]], x1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[1],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[3,i],i], y1 = res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[,XandY[2],res_HiDiSTATIS$SH_Tables$Leave_out_these_tables[3,i],i], col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[3])
  #







  #I think this is just wrong... It's coded as if only 3 tables were left out, not the 18 tables
  #   #plot the 95% prediction intervals, around the Groups, for each stimulus
  #   XandY <- c(1,2)
  #   for(ab in 1:nrow(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond)){
  #     prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,XandY], col = "gray", constraints = minmaxHelper(rbind_array_2_matrix(apply(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,,], c(1,3), t))))
  #     prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),XandY], dev.new = F, new.plot = F)
  #     for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
  #       prettyPlot(t(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,d,]), dev.new = F, new.plot = F, col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
  #       hull.matrix <- t(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,d,])
  #       peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
  #       peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
  #       # pause()
  #     }
  #
  #   }
  #
  #
  #
  #
  #   #plot the 95% prediction intervals, around the Groups, for each Category
  #   XandY <- c(1,2)
  #   for(b in 1:nrow(res_HiDiSTATIS$SH_Tables$Pb_Fhat_LeftOut_Tables_Cond)){
  #     prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,XandY], col = "gray")
  #     prettyPlot((res_HiDiSTATIS$input$DESIGN_rows$Pb %*% res_HiDiSTATIS$res_GrandComp$eig$F)[c(b,b),XandY], dev.new = F, new.plot = F)
  #     for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
  #       prettyPlot(t(res_HiDiSTATIS$SH_Tables$Pb_Fhat_LeftOut_Tables_Cond[b,XandY,d,]), dev.new = F, new.plot = F, col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
  #       hull.matrix <- t(res_HiDiSTATIS$SH_Tables$Pb_Fhat_LeftOut_Tables_Cond[b,XandY,d,])
  #       peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
  #       peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
  #       # pause()
  #     }
  #
  #   }





  #Get to the questions we really care about... here, we care about the participants

  #Plot a given stimuli, for the grand compormise, and group compromises.
  #Then project in the perspectives of all participant
  #And put hulls around the groups of participant
  #(that is, project in every iteration that included that participant)
  XandY <- c(1,2)
  for(ab in 1:res_HiDiSTATIS$input$DESIGN_rows$AB){

    #Initialize the plot (fixed effects)
    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),XandY], col = "gray",
               constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[,XandY,,])),
               display_names = F,
               main=paste0("All LeftOut tables, for ", rownames(res_HiDiSTATIS$input$DESIGN_rows$mat)[ab]))


    #project in the LeftOut participants
    for(cd in 1:res_HiDiSTATIS$input$DESIGN_tables$CD){
      which_iterations <- which(!is.na(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[1,1,cd,]))
      prettyPlot(t(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[ab,XandY,cd,which_iterations]),
                 dev.new = F, new.plot = F, pch=19,
                 col = add.alpha(res_HiDiSTATIS$input$DESIGN_tables$colors_CD[cd], .3))
    }


    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      these_LeftOut_indices <- which(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat[,d]==1)
      hull.matrix <- rbind_array_2_matrix(apply(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY, these_LeftOut_indices, ], c(1,3), t))
      # res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,which(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat[,d]==1),]
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      #   # pause()
    }


    #fixed effects again, plot the group PFS for that stimulus
    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[c(ab,ab),XandY,d],
                 dev.new = F, new.plot = F, display_names = F, cex = 3,
                 col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[1]],
               y0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[2]],
               x1 = res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY[1],d],
               y1 = res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY[2],d],
               col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd = 2)
    }


  }


  #########################################################
  #Soemthing is off in the plot below...
  #Some of the points look too similar across the 3 plots...


  #Plot all Categories, for the grand compormise, and group compromises.
  #Then project in the perspectives of a given participant

  #I'm not sure if I want to plot everything I have, or compute the row average...


  #Plot all a given stimuli, for the grand compormise, and group compromises.
  #Then project in the perspectives of all participant
  #And put hulls around the groups of participant
  #(that is, project in every iteration that included that participant)
  XandY <- c(1,2)
  for(b in 1:res_HiDiSTATIS$input$DESIGN_rows$B){

    A_in_b <- which(res_HiDiSTATIS$input$DESIGN_rows$mat[,b]==1)

    #Initialize the plot (fixed effects)
    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[A_in_b,XandY], col = "gray",
               main=paste0("... for ", res_HiDiSTATIS$input$DESIGN_rows$labels[b]),
               constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[,XandY,,])))


    #project in the LeftOut participants
    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){

      #reCode - which rows
      # which_iterations <- which(!is.na(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables[1,1,cd,]))

      #reCode - which tables
      these_LeftOut_indices <- which(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat[,d]==1)

      #project in all the LeftOut data for all stimuli in a given Category
      #The stimuli from each category will be a separate plot
      prettyPlot(rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[A_in_b,XandY,these_LeftOut_indices,]),
                 dev.new = F, new.plot = F,
                 pch = 19,
                 col = add.alpha(res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], .2))
    }

    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      these_LeftOut_indices <- which(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat[,d]==1)
      hull.matrix <- rbind_array_2_matrix(res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[A_in_b,XandY, these_LeftOut_indices, ])
      # res_HiDiSTATIS$SH_Tables$Fhat_LeftOut_Tables_Cond[ab,XandY,which(res_HiDiSTATIS$SH_Tables$DESIGN_tables_LeftOut$mat[,d]==1),]
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      #   # pause()
    }


    #fixed effects again, plot the group PFS for that stimulus
    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot((res_HiDiSTATIS$input$DESIGN_rows$Pb %*% res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,XandY,d])[c(b,b),],
                 dev.new = F, new.plot = F, display_names = F, cex = 3,
                 col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
      # segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[1]],
      #          y0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,XandY[2]],
      #          x1 = res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY[1],d],
      #          y1 = res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY[2],d],
      #          col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd = 2)
    }


  }


}
