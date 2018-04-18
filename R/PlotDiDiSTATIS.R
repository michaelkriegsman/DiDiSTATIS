#PlotDiDiSTATIS.R

#' Plot results of DiDiSTATIS
#'
#' @param res_DiDiSTATIS The output of EigenHiDiSTATIS
#' @param axes Axes to plot. By default, c(1,2)
#' @param main Title of factor map
#' @export

PlotDiDiSTATIS <- function(res_DiDiSTATIS, axes = c(1,2), main=NULL){


  #Map 1: BaryGrandComp
  #constraints can be on any kind of input...
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
             col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond)),
             main = "Barycentric Grand Compromise")

  #Map 2: ...w/ Bary_GROUPComps
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond)),
             main = "Barycentric Group Compromises")

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
                     col = res_DiDiSTATIS$input$DESIGN_rows$colors_B, lwd=2)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],
               cex=2.5, pch = 19,
               new.plot = F, dev.new = F)
  }

  #Map 3: Disc Grand Comp
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, .8),
             cex = 3, pch=15,
             constraints = minmaxHelper(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..),
             main = "Disc Grand Compromise")
  Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                   col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
             dev.new = F, new.plot = F, pch=15, cex = 1.5)



  #Map 4a: Disc Group Comps v1
  alpha <- .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
             main = "Disc Group Compromises (version 1)")

  Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha))
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha),
             pch=15, cex = 1.5,
             dev.new = F, new.plot = F)

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8))
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
               # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
               cex=1, pch=16,
               new.plot = F, dev.new = F)

    # for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
    #   these_stimuli <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)
    #   hull.matrix <- res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[these_stimuli,,d]
    #   peeledHull(hull.matrix[,axes],percentage=.95,lwd=4)
    #   peeledHull(hull.matrix[,axes],percentage=.95,lwd=2,col=res_DiDiSTATIS$input$DESIGN_rows$colors_B[b])
    # }

  }






  #Map 4b: Disc Group Comps v2
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=22,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
             main = "Disc Group Compromises (version 2)")

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
                     col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd=3)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],
               cex=2.5, pch=19,
               new.plot = F, dev.new = F)
  }

  d <- 1
  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D[,,d], res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],1), lwd=1.75)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
               # col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
               cex=1, pch=15,
               new.plot = F, dev.new = F)
  }








  #Map 4a: Disc Group Comps v3
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
             main = "Disc Group Compromises (version 3)")

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
             col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
             pch=15, cex = 1.75,
             dev.new = F, new.plot = F)

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
    #                  col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8))
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
               cex=1.4, pch=16,
               new.plot = F, dev.new = F)

  }



  ab<-6
  #2, 3,
  #1
  Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                   t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,,]),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, .8), lwd=2.5)


  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[1:12,],
                     res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[1:12,,d],
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=2.5)
  }













  #Map 4d: Disc Group Comps v3
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D)),
             main = "Disc Group Compromises (version 3)")

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[1:12,],
             col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
             pch=15, cex = 1.75,
             dev.new = F, new.plot = F)

  d <- 1
  # for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    # Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
    #                  col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8))
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
               cex=1.4, pch=16,
               new.plot = F, dev.new = F)

  # }



    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
                     "origin",
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                     lwd=2.5)


  ab<-3
  #2, 3,
  #1
  Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                   t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,,d]),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=2.5)




  # for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                     res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[c(ab,ab),,d],
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=2.5)
  # }


    #AND NEXT PLOT....
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[c(1,1),,d],
                     res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[1:12,,d],
                     col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=2.5)












  #Map 5: Bary individuals v1
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3.5, pch=22,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond)),
               main = "Bary Participants (version 1) ")

    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
                       res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d],
                       col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, .8), lwd=3)
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d],
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                 cex=2.5, pch=16,
                 new.plot = F, dev.new = F)
    }

    # s27
    # d=3
    # Cd=8
    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      for(Cd in 1:colSums(res_DiDiSTATIS$input$DESIGN_tables$mat)[d]){
        these_Cd <- which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)

        Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d],
                         res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,these_Cd[Cd]],
                         col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=1)

        prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,these_Cd[Cd]],
                   # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                   cex=1, pch=16,
                   new.plot = F, dev.new = F)
        # pause()
      }
    }







    #Bary Individuals version2
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3.5, pch=22,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond)),
               main = "Bary Participants (version 2) ")
    for(cd in 1:res_DiDiSTATIS$input$DESIGN_tables$CD){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,cd],
                 dev.new = F, new.plot = F,
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd])

      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[,,cd],
                       res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
                       col = res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd], lwd=1)
    }




    #Disc Individuals
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3, pch=22,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd)),
               main = "Disc Individuals (version 1)")

    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                     col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
               pch=22, cex = 1.75,
               dev.new = F, new.plot = F)
    for(cd in 1:res_DiDiSTATIS$input$DESIGN_tables$CD){
      #plot all stimuli for 1 participant
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd],
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd],
                 dev.new = F, new.plot = F)
      # Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd],
      #                  res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
      #                  col = res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd])
    }
    ab<-1
    Segments_from_to(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,,]),
                                      res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                                      col = res_DiDiSTATIS$input$DESIGN_tables$colors_CD)






    #Map 5: Disc individuals

    cd <- 1
    for(cd in 1:res_DiDiSTATIS$input$DESIGN_tables$CD){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
               cex = 3.5, pch=22,
               constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd)),
               main = paste0("All stimuli, for participant ", rownames(res_DiDiSTATIS$input$DESIGN_tables$mat)[cd]))

      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                       col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
                 pch=22, cex = 1.75,
                 dev.new = F, new.plot = F)

      #plot all stimuli for 1 participant
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd],
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd],
                 dev.new = F, new.plot = F)
      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,cd],
                       res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                       col = res_DiDiSTATIS$input$DESIGN_tables$colors_CD[cd])


    }





    #plot 1 stimulus for all participants
    for(ab in 1:res_DiDiSTATIS$input$DESIGN_rows$AB){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
                 cex = 3.5, pch=22,
                 constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd)),
                 main = paste0("Stimulus '",rownames(res_DiDiSTATIS$input$DESIGN_rows$mat)[ab],"' for all participants"))

      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                       col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
                 pch=22, cex = 1.75,
                 dev.new = F, new.plot = F)

      prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,,]),
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_CD,
                 dev.new = F, new.plot = F)
      Segments_from_to(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,,]),
                       res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                       col = res_DiDiSTATIS$input$DESIGN_tables$colors_CD)

    }



    #plot 1 stimulus for all participants, WITH group barycenters
    for(ab in 1:res_DiDiSTATIS$input$DESIGN_rows$AB){
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
                 cex = 3.5, pch=22,
                 constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd)),
                 main = "Discriminant Participants")

      Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                       col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
      prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
                 pch=22, cex = 1.75,
                 dev.new = F, new.plot = F)


      for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
        Cd <- which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)
        Segments_from_to(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,,Cd]),
                         res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[c(ab,ab),,d],
                         col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])

        Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[c(ab,ab),,d],
                         res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[c(ab,ab),],
                         col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd=2)

      }

      prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,,]),
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_CD,
                 dev.new = F, new.plot = F)
      prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,,]),
                 col=res_DiDiSTATIS$input$DESIGN_tables$colors_D,
                 cex=2,
                 dev.new = F, new.plot = F)




    }
















# #Map 6: Discriminant individuals v1
#   prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
#              cex = 3.5, pch=22,
#              constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd)),
#              main = "Disc Participants (version 1) ")
#
#   Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
#                    col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
#   prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB,
#              pch=22, cex = 2,
#              dev.new = F, new.plot = F)
#
#   for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
#     Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc.., res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
#                      col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .6), lwd=3)
#     prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
#                # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
#                col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .6),
#                cex=1.75, pch=16,
#                new.plot = F, dev.new = F)
#   }
#
#   for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
#     for(Cd in 1:colSums(res_DiDiSTATIS$input$DESIGN_tables$mat)[d]){
#       these_Cd <- which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)
#
#       Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[,,d],
#                        res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,these_Cd[Cd]],
#                        col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8), lwd=1)
#
#       prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[,,these_Cd[Cd]],
#                  # col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
#                  col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
#                  cex=1, pch=16,
#                  new.plot = F, dev.new = F)
#       pause()
#     }
#   }










  # DESIGN_rows   <- res_DiDiSTATIS$input$DESIGN$rows$Composers_mat
  # DESIGN_tables <- res_HiDiSTATIS$input$DESIGN$tables$MusExp_mat
  # t             <- res_HiDiSTATIS$eig$t
  # Ctrb          <- res_HiDiSTATIS$eig$Ctrb
  #
  #
  # #Grand Compromise
  # F <- res_HiDiSTATIS$eig$F *-1
  # F_Berries <- Condense_Rows(F[,axes], DESIGN_rows)
  # F_Berries_verbose <- Bary_Projector(DESIGN_rows) %*% F[,axes]
  # ### remove "axes", to copy most recent version of MDS.
  #
  #
  # #Group Compromises
  # Fd  <- res_HiDiSTATIS$ProjGroup$F*-1   #Note, "ProjGroup"
  # Fd_Berries <- array(NA, dim=c(B, length(axes), D))
  # for(d in 1:D){
  #   Fd_Berr <- paste0("Fd_Berries[,axes,",d,"]         <- Condense_Rows(Fd[,axes,",d,"], DESIGN_rows)")
  #   eval(parse(text = Fd_Berr))
  # }
  # dimnames(Fd_Berries) <- list(colnames(DESIGN_rows), colnames(F)[axes], colnames(DESIGN_tables))
  #
  #
  # #Individual Data
  # Fcd           <- res_HiDiSTATIS$ProjCP$F*-1
  #
  #
  # #Colors
  # colors_D      <- DESIGN$tables$MusExp_colors_D
  # colors_CD     <- DESIGN$tables$MusExp_colors_CD
  #
  # colors_B      <- DESIGN$rows$Composer_colors_B
  # colors_AB     <- DESIGN$rows$Composer_colors_AB
  #
  # ###################################################################################
  # ####### Should make this a function...    #######
  # #commenting to copy HMFA's way of plotting on the constrainst of the compromise, below
  # ##constraints
  # #constraints <- minmaxHelper(mat1 = rbind(F, Fd[,,1], Fd[,,2], Fd[,,3]),
  # #                            axis1 = axes[1], axis2 = axes[2])
  # Fd_rows <- matrix(NA, A*D, ncol(F))
  # for(d in 1:D){
  #   from <- 1 + (A*(d-1))
  #   to <- A * d
  #   these_rows <- from:to
  #   Fd_rows[these_rows,] <- Fd[,,d]
  # }
  #
  # Fcd_rows <- matrix(NA, A*CD, ncol(F))
  # for(cd in 1:CD){
  #   from <- 1 + (A*(cd-1))
  #   to <- A * cd
  #   these_rows <- from:to
  #   Fcd_rows[these_rows,] <- Fcd[,,cd]
  # }
  #
  # #constraints
  # constraints_Cons <- minmaxHelper(mat1 = F, #mat1 = rbind(F, Fk[,,1], Fk[,,2], Fk[,,3]),
  #                                  axis1 = axes[1], axis2 = axes[2])
  #
  # constraints_Groups <- minmaxHelper(mat1 = rbind(F, Fd_rows),
  #                                    axis1 = axes[1], axis2 = axes[2])
  #
  # constraints_PFS <- minmaxHelper(mat1 = rbind(F, Fcd_rows), #mat1 = rbind(F, Fk[,,1], Fk[,,2], Fk[,,3]),
  #                                 axis1 = axes[1], axis2 = axes[2])
  # ###################################################################################
  #
  # if(!is.null(main)){
  #   main <- paste0(main, ": ")
  # }
  #
  # #Map 1: Grand Compromise
  # prettyGraphs::prettyPlot(F[,axes],
  #                          #dev.new=FALSE, new.plot=TRUE,
  #                          display_names = FALSE, cex=1.5,
  #                          constraints = constraints_Cons,
  #                          #contributionCircles = TRUE, contributions = Ctrb,
  #                          xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
  #                          ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
  #                          col = colors_AB,
  #                          pch=21,
  #                          # In future, change this to compute the within-group average, and use colors_B
  #                          main = paste0(main,'Grand Compromise'))
  #
  # text(F_Berries, col = colors_B, labels = colnames(DESIGN_rows))
  #
  #
  # #Map 1: Group Compromises
  # for(d in 1:D){
  #
  #   PlotFd <- paste0("prettyGraphs::prettyPlot(Fd[,axes,",d,"],
  #                                             #dev.new=FALSE, new.plot=TRUE,
  #                                             display_names = FALSE, cex=1.5,
  #                                             constraints = constraints_Groups,
  #                                             #contributionCircles = TRUE, contributions = Ctrb,
  #                                             # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                                             # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                                             xlab = paste0('Component ', axes[1]),
  #                                             ylab = paste0('Component ', axes[2]),
  #                                             col = colors_AB,
  #                                             pch=21,
  #                                             # In future, change this to compute the within-group average, and use colors_B
  #                                             main = paste0(main,'Group Compromise: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  #   eval(parse(text = PlotFd))
  #
  #   AndTheNames <- paste0("text(Fd_Berries[,,",d,"], col = colors_B, labels = colnames(DESIGN_rows))")
  #   eval(parse(text = AndTheNames))
  #
  #   PlotFd <- paste0("prettyGraphs::prettyPlot(Fd[,axes,",d,"],
  #                    dev.new=FALSE, new.plot=FALSE,
  #                    display_names = FALSE, cex=1.5,
  #                    constraints=constraints_Groups,
  #                    #contributionCircles = TRUE, contributions = Ctrb,
  #                    # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                    # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                    xlab = paste0('Component ', axes[1]),
  #                    ylab = paste0('Component ', axes[2]),
  #                    col = colors_D[d], #colors_AB,
  #                    pch=21,
  #                    # In future, change this to compute the within-group average, and use colors_B
  #                    main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  #   eval(parse(text = PlotFd))
  #
  # }
  #
  #
  # #Map 3: Group Consensus with Consensus
  #
  # #depending on what I want, print all groups on this 1, or recopy this background once for each group... put it in the loop
  #  # PlotF <- paste0("prettyGraphs::prettyPlot(F[,axes],
  #  #                  #dev.new=FALSE, new.plot=TRUE,
  #  #                  display_names = FALSE, cex=1,
  #  #                  constraints=constraints_Groups,
  #  #                  #contributionCircles = TRUE, contributions = Ctrb,
  #  #                  # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #  #                  # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #  #                  xlab = paste0('Component ', axes[1]),
  #  #                  ylab = paste0('Component ', axes[2]),
  #  #                  col = 'black', #gray
  #  #                  pch=21,
  #  #                  # In future, change this to compute the within-group average, and use colors_B
  #  #                  main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  #  #  eval(parse(text = PlotF))
  #
  #   for(d in 1:D){
  #
  #   PlotF <- paste0("prettyGraphs::prettyPlot(F[,axes],
  #                   #dev.new=FALSE, new.plot=TRUE,
  #                   display_names = FALSE, cex=1,
  #                   constraints=constraints_Groups,
  #                   #contributionCircles = TRUE, contributions = Ctrb,
  #                   # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                   # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                   xlab = paste0('Component ', axes[1]),
  #                   ylab = paste0('Component ', axes[2]),
  #                   col = 'black', #gray
  #                   pch=21,
  #                   # In future, change this to compute the within-group average, and use colors_B
  #                   main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  #   eval(parse(text = PlotF))
  #
  #   #AndTheNames <- paste0("text(Fd_Berries[,,",d,"], col = colors_B, labels = colnames(DESIGN_rows))")
  #   #eval(parse(text = AndTheNames))
  #
  #   #color by colors_AB
  #   #ConnectTheDots <- paste0("segments(Fd[,axes[1],",d,"], Fd[,axes[2],",d,"], F[,axes[1]], F[,axes[2]], colors_AB)")
  #   #eval(parse(text = ConnectTheDots))
  #
  #   #color by colors_D
  #   ConnectTheDots <- paste0("segments(Fd[,axes[1],",d,"], Fd[,axes[2],",d,"], F[,axes[1]], F[,axes[2]], colors_D[d])")
  #   eval(parse(text = ConnectTheDots))
  #
  #   PlotFd <- paste0("prettyGraphs::prettyPlot(Fd[,axes,",d,"],
  #                    dev.new=FALSE, new.plot=FALSE,
  #                    display_names = FALSE, cex=1.5,
  #                    constraints=constraints_Groups,
  #                    #contributionCircles = TRUE, contributions = Ctrb,
  #                    # xlab = paste0('Component ', axes[1],' variance = ', round(t[axes][1],3), '%'),
  #                    # ylab = paste0('Component ', axes[2],' variance = ', round(t[axes][2],3), '%'),
  #                    xlab = paste0('Component ', axes[1]),
  #                    ylab = paste0('Component ', axes[2]),
  #                    col = colors_D[d], #colors_AB,
  #                    pch=21,
  #                    # In future, change this to compute the within-group average, and use colors_B
  #                    main = paste0(main,'Group Consensus: ', '",colnames(DESIGN$tables$MusExp_mat)[d],"'))")
  #   eval(parse(text = PlotFd))
  #
  # }
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  # #To explore these results,
  # #1) Show that a single point in the grand comp is the (weighted) barycenter of the 3 group compromises
  #   #and set the Ctrb (the size of the points) as the weight of the group comp
  #   #though, this won't change much, because the group weights are all nearly 0.33
  #
  # #1.5) For completeness sake, show that for an individual stimulus,
  #   #the grand is the weighted barycenter of the group,
  #   #and the group is the wieghted barycenter of the individuals in that group
  #
  # #2) To actually interpret these data... Average rows of F (and rows of Fd) within composers.
  #   #Later, average rows within composer*pianist (12 data points)
  #   #Until I do inference, just include the tolerance intervals so that a composer (or composer*pianist) visually occupies it's spread.
  #
  #
  # #Another question we'd like to ask... For each group of participants, are the composers or are the pianists for descriptive?


}
