% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/definitions_shiny.R
\name{has.na_nan_inf}
\alias{has.na_nan_inf}
\title{Check dataframes for NA, NaN or Inf values.}
\usage{
has.na_nan_inf(df, print.which = F)
}
\arguments{
\item{df}{The data.frame to check.}

\item{print.which}{Print which columns have bad values to the console.}
}
\value{
Logical value: FALSE (check passed) or TRUE (check failed; nasty values found).
}
\description{
Check dataframes for NA, NaN or Inf values.
}
\keyword{internal}
