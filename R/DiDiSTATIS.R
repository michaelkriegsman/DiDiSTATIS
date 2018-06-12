
#The DiDiSTATIS function within DiDiSTATIS
#
#'Conduct Discriminant DiSTATIS
#'
#'@param DATA The data
#'@param data_are Flag to indicate data type #sort, sort_dist, d_array, d2_array, CP_array
#'@param DESIGN_rows DESIGN matrix for the rows
#'@param DESIGN_tables DESIGN matrix for the tables
#'@param MFA1_Flag #TRUE gives MFA-norm; FALSE gives no norm
#'@param RV1_Flag # TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/CD (1/N)
#'@param MFA2_Flag #TRUE gives MFA-norm; FALSE gives no norm
#'@param RV2_Flag # TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/D
#'@param Perm_omni_sort Boolean to conduct permutation test on omnibus (for sorting data)
#'@param Perm_omni_sort_niter Number of permutation iterations
#'@param Boot_tables Boolean to conduct bootstrap resampling on tables
#'@param Boot_tables_niter Number of bootstrap iterations
#'@param LOO_rows Boolean to conduct Leave One Out (LOO) cross-validation on tables
#'@param LOO_rows_multiplier Increase the number of LOO iterations
#'@param SH_rows Boolean to conduct Split Half (SH) cross-validation on tables
#'@param SH_rows_niter Number of SH iterations
#'@param n2k Number (of components) to keep
#'@return A list of computational results
#'@export

DiDiSTATIS <- function(DATA, data_are = 'sort', n2k = NULL,
                       DESIGN_rows = NULL, DESIGN_tables = NULL,
                       MFA1_Flag = TRUE, RV1_Flag = TRUE,
                       MFA2_Flag = TRUE, RV2_Flag = TRUE,
                       Perm_omni_sort = F, Perm_omni_sort_niter=1000,
                       Boot_tables = F, Boot_tables_niter = 1000,
                       LOO_rows = F, LOO_rows_multiplier=10,
                       SH_rows = F, SH_rows_niter=100){

  #Convert data to CP
  CP_array <- GetCP_array(DATA, data_are)

  ##Step 1: Identify table weights and compute the Group and Grand Compromises
  Hierarchy_of_tables <- GetBaryGrandComp(CP_array = CP_array,
                                          DESIGN_rows = DESIGN_rows,
                                          DESIGN_tables = DESIGN_tables,
                                          MFA1_Flag = MFA1_Flag, RV1_Flag = RV1_Flag,
                                          MFA2_Flag = MFA2_Flag, RV2_Flag = RV2_Flag)

  ##Step 2: Decompose the Barycentric Grand Compromise
  res_BaryGrand <- EigenDiDiSTATIS(Hierarchy_of_tables, DESIGN_rows, DESIGN_tables, n2k=n2k)

  input <- list()
  input$DATA          <- DATA
  input$data_are      <- data_are
  input$CP_array      <- CP_array
  input$DESIGN_rows   <- DESIGN_rows
  input$DESIGN_tables <- DESIGN_tables
  input$MFA1_Flag     <- MFA1_Flag
  input$RV1_Flag      <- RV1_Flag
  input$MFA2_Flag     <- MFA2_Flag
  input$RV2_Flag      <- RV2_Flag

  ##Step 3: Inference
  if(Perm_omni_sort==TRUE){
    if(data_are=="sort_dist") {input$DATA <- SortFromSortDist(DATA)}
    Perm_Omnibus <- DiDiSTATIS_perm_omni_sort(input = input,
                                              Hierarchy_of_tables = Hierarchy_of_tables,
                                              res_BaryGrand = res_BaryGrand,
                                              niter=Perm_omni_sort_niter)
  }#end Perm_tables

  if(Boot_tables==TRUE){
    Boot_Tables <- DiDiSTATIS_boot_tables(input = input,
                                          Hierarchy_of_tables = Hierarchy_of_tables,
                                          res_BaryGrand = res_BaryGrand,
                                          niter = Boot_tables_niter)
  }#end Boot_tables

  if(LOO_rows==TRUE){
    LOO_Rows <- DiDiSTATIS_LOO_rows(input = input,
                                    Hierarchy_of_tables = Hierarchy_of_tables,
                                    res_BaryGrand = res_BaryGrand,
                                    multiplier = LOO_rows_multiplier)
  }#end LOO_rows

  if(SH_rows==TRUE){
    SH_Rows <- DiDiSTATIS_SH_rows(input = input,
                                  Hierarchy_of_tables = Hierarchy_of_tables,
                                  res_BaryGrand = res_BaryGrand,
                                  niter = SH_rows_niter)
  }#end SH_tables










  returnME <- list(input = input,
                   Hierarchy_of_tables = Hierarchy_of_tables,
                   res_BaryGrand = res_BaryGrand
                   )

  if(Perm_omni_sort){
    returnME$Perm_Omnibus <- Perm_Omnibus #Permutation test of Table DESIGN vs Null (random) DESIGNs
  }
  if(Boot_tables){
    returnME$Boot_Tables <- Boot_Tables
  }
  if(LOO_rows){
    returnME$LOO_Rows <- LOO_Rows
  }
  if(SH_rows){
    returnME$SH_Rows <- SH_Rows
  }
  return(returnME)

}
