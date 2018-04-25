#ProposalDefense_MDS

#Run MDS_vig.Rmd to get things started. 
#Also need to run some stuff line by line, like the inside of PlotCP.R:
F               <- res_CP$eig$F
t               <- res_CP$eig$t
Ctrb            <- res_CP$eig$Ctrb
DESIGN_rows_mat <- res_CP$input$DESIGN$rows$rows$mat
colors_B          <- res_CP$input$DESIGN$rows$rows$colors_B
colors_AB         <- res_CP$input$DESIGN$rows$rows$colors_AB

axes <- c(1,2)
Berries <- Condense_Rows(F, DESIGN_rows_mat)
Berries_verbose <- Bary_Projector(DESIGN_rows_mat) %*% F


#Make plots
#Scree
dev.new()
barplot(t, space=0, xlab = paste0("Components (",length(t),")"), ylab = "% Var", main = "Scree Plot", 
        ylim = c(0,t[1] * 1.2)
        #names.arg = 1:length(t)
        )

#Factor Scores
prettyGraphs::prettyPlot(F[,axes],
                         contributionCircles = FALSE, #contributions = Ctrb,
                         cex=1.5, display_names = FALSE,
                         col = 'black',
                         xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
                         ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
                         main = paste0('MDS Factor Scores'))

#also strip out the title and axes, so these can stay constant slide over slide...

#Color factor scores
constraints <- minmaxHelper(F[,c(1,2)])
prettyPlot(F[,axes],
           contributionCircles = TRUE, #contributions = Ctrb,
           cex=1.5, display_names = FALSE,
           constraints = constraints,
           xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
           ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
           col = colors_AB,
           main = paste0('MDS Factor Scores'))

#With barycenters
#text(Berries, col = colors_B, labels = colnames(DESIGN_rows_mat))

#Find the segments to connect Bach to his barycenter

Bac  <- which(DESIGN_rows_mat[,1]==1)
Beet <- which(DESIGN_rows_mat[,2]==1)
Moz  <- which(DESIGN_rows_mat[,3]==1)

Him <- Bac
segments(F[Him,axes[1]],  F[Him,axes[2]],  Berries_verbose[Him,axes[1]], Berries_verbose[Him,axes[2]], col=colors_AB[Him])


#EVERYONE
#segments(F[,1],  F[,2],  Berries_verbose[,1], Berries_verbose[,2], col=colors_AB)

#And then replace Bach's segments with a square
prettyGraphs::prettyPlot(rbind(Berries[1,axes],Berries[1,axes]), new.plot = FALSE, dev.new = FALSE,
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B[1],
                         pch=22)

#Then place Beet
#rerun colored F
#and replace Mozart's square 
#and Bach's square
#And then Beethoven's segments
Him <- Beet
segments(F[Him,axes[1]],  F[Him,axes[2]],  Berries_verbose[Him,axes[1]], Berries_verbose[Him,axes[2]], col=colors_AB[Him])
#and his square
prettyGraphs::prettyPlot(rbind(Berries[2,axes],Berries[2,axes]), new.plot = FALSE, dev.new = FALSE,
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B[2],
                         pch=22)

#And finally, place Beethoven
#rerun colored F
#and replace Mozart's square
#then, place Bach's segments
Him <- Moz
segments(F[Him,axes[1]],  F[Him,axes[2]],  Berries_verbose[Him,axes[1]], Berries_verbose[Him,axes[2]], col=colors_AB[Him])
#and his square
prettyGraphs::prettyPlot(rbind(Berries[3,axes],Berries[3,axes]), new.plot = FALSE, dev.new = FALSE,
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B[3],
                         pch=22)






#And show the next components...
############ Computing Berries as in PlotCP...sloppy ###########whoops
axes <- c(34,35)

#Color factor scores
prettyPlot(F[,axes],
           contributionCircles = TRUE, #contributions = Ctrb,
           constraints = constraints,
           cex=1.5, display_names = FALSE,
           xlab = paste0("Component ", axes[1]," variance = ", round(t[axes][1],3), "%"),
           ylab = paste0("Component ", axes[2]," variance = ", round(t[axes][2],3), "%"),
           col = colors_AB,
           main = paste0('MDS Factor Scores'))
prettyGraphs::prettyPlot(Berries[,axes], new.plot = FALSE, dev.new = FALSE,
                         constraints = constraints,
                         contributionCircles = FALSE, #contributions = Ctrb,
                         display_names = FALSE,
                         cex=3,
                         col = colors_B,
                         pch=22)
