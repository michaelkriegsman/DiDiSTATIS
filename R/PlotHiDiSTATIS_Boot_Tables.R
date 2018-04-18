#'Plot results from bootstrap resampling of Tables for HiDiSTATIS
#'
#'@param res_HiDiSTATIS Output from HiDiSTATIS with Boot_tables==TRUE
#'@return Output
#'@export


PlotHiDiSTATIS_Boot_Tables <- function(res_HiDiSTATIS){

  #Show that I need the correction in order to isolate the error of interest
  #In other words, there are 2 sources of "error"
  # 1) The difference between each bootstrapped grand compromise and the fixed grand compromise
  # 2) The difference between each group's factor scores and the grand compromise factor scores

  # I am not interested in the 1st source of error, so I subtract it, to give the corrected:
  # Fab_boot_d_corrected

  #Select a given stimulus
  ab <- 5
  #Plot the fixed grand compromise factor scores
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),],
             constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$Fab..boot)))
  #Plot the bootstrapped grand compromise (to visualize the 1st error)
  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1:2,1]),
             new.plot = F, dev.new = F, col="grey")
  text(t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1:2,1]), col = "grey",
       labels = "Booted", pos=3)
  #Plot the bootstrap group compromise (whose barycenter is the booted grand compromise)
  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,1:2,,1]),
             new.plot = F, dev.new = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D)
  segments(x0 = t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1,1]),
           y0 = t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,2,1]),
           x1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,1,,1])),
           y1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,2,,1])),
           col = res_HiDiSTATIS$input$DESIGN_tables$colors_D)
  #And plot the corrected bootstrap group compromise (whose barycenter is the fixed grand compromise)
  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,1:2,,1]),
             new.plot = F, dev.new = F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D)
  segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,1],
           y0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,2],
           x1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,1,,1])),
           y1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,2,,1])),
           col = res_HiDiSTATIS$input$DESIGN_tables$colors_D)
  #This shows that I want to use the corrected group compromise factor scores to estimate the group effect
  #because the deviation between between each booted grand compromise and the fixed grand compromise
  #needs to be mentioned, but is not of interest.



  ### I'm not sure what the point was here.
  ### And not sure whether the hulls should be "corrected"
  ###
  # #Plot the row categories for the fixed grand compromise
  # # and the row categories for the bootstrapped grand compromise
  # XandY <- c(1,2)
  # prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat),
  #            constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,XandY,,])),
  #            col = res_HiDiSTATIS$input$DESIGN_rows$colors_B, display_names = T, cex=2,
  #            main=paste0("HiDiSTATIS: 95% Bootstrap CIs, for Row Categories"))
  #
  # for(b in 1:res_HiDiSTATIS$input$DESIGN_rows$B){
  #   hull.matrix <- t(res_HiDiSTATIS$Boot_Tables$F.b..boot[b,,])
  #   peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
  #   peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_rows$colors_B[b])
  # }



  #Also, plot the fixed grand compromise, and the fixed group compromises
  # And then put a 95% CI around the corrected group compromise factor scores.
  for(ab in 1:res_HiDiSTATIS$input$DESIGN_rows$AB){
    XandY <- c(1,2)
    prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),],
               constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[,XandY,,])),
               display_names = F,
               main=paste0("HiDiSTATIS: 95% Bootstrap CIs, for ", rownames(res_HiDiSTATIS$input$DESIGN_rows$mat)[ab]))

    prettyPlot(t(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[ab,XandY,]),
               col=res_HiDiSTATIS$input$DESIGN_tables$colors_D,
               dev.new = F, new.plot = F)

    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      hull.matrix <- t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }

  }



  #And, just to see, if we average together the stimuli from the B composers, can we aggregate an effect of the groups of tables?
  XandY <- c(1,2)
  prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat),
             constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,XandY,,])),
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_B, display_names = F,
             main=paste0("HiDiSTATIS: 95% Bootstrap CIs, for Row Categories"))

  for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,XandY,d], res_HiDiSTATIS$input$DESIGN_rows$mat),
               col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d],
               display_names = F,
               dev.new = F, new.plot = F)

    for(b in 1:res_HiDiSTATIS$input$DESIGN_rows$B){
      hull.matrix <- t(res_HiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }

  }

  prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat),
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_B, pch = 22, cex=2,
             dev.new = F, new.plot = F)





  #Separate each composer (row category) into its own group
  for(b in 1:res_HiDiSTATIS$input$DESIGN_rows$B){
    XandY <- c(1,2)
    prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$eig$F, res_HiDiSTATIS$input$DESIGN_rows$mat)[c(b,b),],
               constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,XandY,,])),
               main=paste0("HiDiSTATIS: 95% Bootstrap CIs, for ", colnames(res_HiDiSTATIS$input$DESIGN_rows$mat)[b]))

    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      prettyPlot(Condense_Rows(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,XandY,d], res_HiDiSTATIS$input$DESIGN_rows$mat)[c(b,b),],
                 col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d],
                 dev.new = F, new.plot = F)


      hull.matrix <- t(res_HiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,,d,])
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
      peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }

  }






}
