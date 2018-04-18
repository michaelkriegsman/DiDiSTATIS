#The HiDiSTATIS function within DiDiSTATIS
#
#'Conduct Hierarchical DiSTATIS
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #sort, d_array, d2_array, CP_array
#'@param DESIGN_rows DESIGN info for the rows
#'@param DESIGN_tables DESIGN info for the tables
#'@param n2k Number (of components) to keep
#'@param Perm_tables Boolean to conduct permutation test
#'@param Perm_tables_niter Number of permutation iterations
#'@param Boot_tables Boolean to conduct bootstrap resampling
#'@param Boot_tables_niter Number of bootstrap iterations
#'@param LOO_tables Boolean to conduct Leave One Out (LOO) cross-validation
#'@param LOO_tables_multiplier Increase the number of LOO iterations
#'@param SH_tables Boolean to conduct Split Half (SH) cross-validation
#'@param SH_tables_niter Number of SH iterations
#'@return Factor maps, and a list of computational results
#'@export

HiDiSTATIS <- function(DATA, data_are = NULL,
                       DESIGN_rows,
                       DESIGN_tables, n2k=NULL,
                       Perm_tables = F, Perm_tables_niter = 1000,
                       Boot_tables = F, Boot_tables_niter = 1000,
                       LOO_tables = F,  LOO_tables_multiplier = 10,
                       SH_tables = F,   SH_tables_niter = 50){


  ##Step 0: Preprocess the data tables
  CP_array <- GetCP_array(DATA, data_are)

  ##Step 1: Identify table weights and compute the Group and Grand Compromises
  Hierarchy_of_tables <- GetGrandCompromise(CP_array = CP_array, DESIGN_tables = DESIGN_tables)

  ##Step 2: Decompose the Grand Compromise, project the groups, and individuals
  res_GrandComp <- EigenHiDiSTATIS(Hierarchy_of_tables, DESIGN_tables, n2k=n2k)
  names(res_GrandComp$input) <- "GrandCompromise"

  #And gather some materials to be returned
  input <- list()
  input$DATA          <- DATA
  input$data_are      <- data_are
  input$CP_array      <- CP_array
  input$DESIGN_rows   <- DESIGN_rows
  input$DESIGN_tables <- DESIGN_tables

  ##Step 3: Inference
  if(Perm_tables==TRUE){
    Perm_Tables <- HiDiSTATIS_perm_tables(input = input,
                                          Hierarchy_of_tables = Hierarchy_of_tables,
                                          res_GrandComp = res_GrandComp,
                                          niter=Perm_tables_niter)
  }#end Perm_tables

  if(Boot_tables==TRUE){
    Boot_Tables <- HiDiSTATIS_boot_tables(input = input,
                                          Hierarchy_of_tables = Hierarchy_of_tables,
                                          res_GrandComp = res_GrandComp,
                                          niter = Boot_tables_niter)
  }#end Boot_tables

  if(LOO_tables==TRUE){
    LOO_Tables <- HiDiSTATIS_LOO_tables(input = input,
                                        Hierarchy_of_tables = Hierarchy_of_tables,
                                        res_GrandComp = res_GrandComp,
                                        multiplier = LOO_tables_multiplier)
  }#end LOO_tables

  if(SH_tables==TRUE){
    SH_Tables <- HiDiSTATIS_SH_tables(input = input,
                                      Hierarchy_of_tables = Hierarchy_of_tables,
                                      res_GrandComp = res_GrandComp,
                                      niter = SH_tables_niter)
  }#end SH_tables










  #Prepare items to return
  returnME <- list(input = input,
                   Hierarchy_of_tables = Hierarchy_of_tables,
                   res_GrandComp = res_GrandComp)

  if(Perm_tables){
    returnME$Perm_Tables <- Perm_Tables #Permutation test of Table DESIGN vs Null (random) DESIGNs
  }
  if(Boot_tables){
    returnME$Boot_Tables <- Boot_Tables
  }
  if(LOO_tables){
    returnME$LOO_Tables <- LOO_Tables
  }
  if(SH_tables){
    returnME$SH_Tables <- SH_Tables
  }

  return(returnME)

}
