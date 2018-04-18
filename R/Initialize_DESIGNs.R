#'Create Row and Table DESIGN objects
#'
#'@param DESIGN_rows_mat DESIGN matrix for rows (stimuli)
#'@param DESIGN_tables_mat DESIGN matrix for tables (participants)
#'@export


Initialize_DESIGNs <- function(DESIGN_rows_mat=NULL, DESIGN_tables_mat=NULL){

  RETURNme <- list()

  #ROWS
  if(!is.null(DESIGN_rows_mat)){
    if(is.vector(DESIGN_rows_mat)){
      RETURNme$DESIGN_rows$labels <- unique(DESIGN_rows_mat)
      DESIGN_rows_mat <- makeNominalData(as.matrix(DESIGN_rows_mat))
      colnames(DESIGN_rows_mat) <- RETURNme$DESIGN_rows$labels
    }

    RETURNme$DESIGN_rows$labels <- colnames(DESIGN_rows_mat)
    RETURNme$DESIGN_rows$mat <- DESIGN_rows_mat
    RETURNme$DESIGN_rows$Pb_Full <- Bary_Projector(DESIGN_rows_mat)
    RETURNme$DESIGN_rows$Pb_Cond <- Bary_Projector_Cond(DESIGN_rows_mat)
    RETURNme$DESIGN_rows$B  <- ncol(DESIGN_rows_mat)
    RETURNme$DESIGN_rows$AB <- nrow(DESIGN_rows_mat)

    RETURNme$DESIGN_rows$colors_B  <- prettyGraphsColorSelection(RETURNme$DESIGN_rows$B, offset=3)
    RETURNme$DESIGN_rows$colors_AB <- rep(NaN, RETURNme$DESIGN_rows$AB)
    for(b in 1:RETURNme$DESIGN_rows$B){
      RETURNme$DESIGN_rows$colors_AB[which(DESIGN_rows_mat[,b]==1)] <- RETURNme$DESIGN_rows$colors_B[b]
    }
  }

  #TABLES
  if(!is.null(DESIGN_tables_mat)){
    if(is.vector(DESIGN_tables_mat)){
      RETURNme$DESIGN_tables$labels <- unique(DESIGN_tables_mat)
      DESIGN_tables_mat <- makeNominalData(as.matrix(DESIGN_tables_mat))
      colnames(DESIGN_tables_mat) <- RETURNme$DESIGN_tables$labels
    }

    RETURNme$DESIGN_tables$labels <- colnames(DESIGN_tables_mat)
    RETURNme$DESIGN_tables$mat <- DESIGN_tables_mat
    RETURNme$DESIGN_tables$Pd_Full <- Bary_Projector(DESIGN_tables_mat)
    RETURNme$DESIGN_tables$Pd_Cond <- Bary_Projector_Cond(DESIGN_tables_mat)
    RETURNme$DESIGN_tables$D  <- ncol(DESIGN_tables_mat)
    RETURNme$DESIGN_tables$CD <- nrow(DESIGN_tables_mat)

    RETURNme$DESIGN_tables$colors_D  <- prettyGraphsColorSelection(RETURNme$DESIGN_tables$D, offset=4)
    RETURNme$DESIGN_tables$colors_CD <- rep(NaN, RETURNme$DESIGN_tables$CD)
    for(d in 1:RETURNme$DESIGN_tables$D){
      RETURNme$DESIGN_tables$colors_CD[which(DESIGN_tables_mat[,d]==1)] <- RETURNme$DESIGN_tables$colors_D[d]
    }
  }

  return(RETURNme)
}
