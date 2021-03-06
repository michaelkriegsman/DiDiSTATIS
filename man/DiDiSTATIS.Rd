% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiDiSTATIS.R
\name{DiDiSTATIS}
\alias{DiDiSTATIS}
\title{Conduct Discriminant DiSTATIS}
\usage{
DiDiSTATIS(DATA, data_are = "sort", n2k = NULL, DESIGN_rows = NULL,
  DESIGN_tables = NULL, MFA1_Flag = TRUE, RV1_Flag = TRUE,
  MFA2_Flag = TRUE, RV2_Flag = TRUE, Perm_omni_sort = F,
  Perm_omni_sort_niter = 1000, Boot_tables = F, Boot_tables_niter = 1000,
  LOO_rows = F, LOO_rows_multiplier = 10, SH_rows = F,
  SH_rows_niter = 100)
}
\arguments{
\item{DATA}{The data}

\item{data_are}{Flag to indicate data type #sort, sort_dist, d_array, d2_array, CP_array}

\item{n2k}{Number (of components) to keep}

\item{DESIGN_rows}{DESIGN matrix for the rows}

\item{DESIGN_tables}{DESIGN matrix for the tables}

\item{MFA1_Flag}{#TRUE gives MFA-norm; FALSE gives no norm}

\item{RV1_Flag}{# TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/CD (1/N)}

\item{MFA2_Flag}{#TRUE gives MFA-norm; FALSE gives no norm}

\item{RV2_Flag}{# TRUE gives RV; FALSE gives C; "skip" sets alpha-weights to 1/D}

\item{Perm_omni_sort}{Boolean to conduct permutation test on omnibus (for sorting data)}

\item{Perm_omni_sort_niter}{Number of permutation iterations}

\item{Boot_tables}{Boolean to conduct bootstrap resampling on tables}

\item{Boot_tables_niter}{Number of bootstrap iterations}

\item{LOO_rows}{Boolean to conduct Leave One Out (LOO) cross-validation on tables}

\item{LOO_rows_multiplier}{Increase the number of LOO iterations}

\item{SH_rows}{Boolean to conduct Split Half (SH) cross-validation on tables}

\item{SH_rows_niter}{Number of SH iterations}
}
\value{
A list of computational results
}
\description{
Conduct Discriminant DiSTATIS
}
