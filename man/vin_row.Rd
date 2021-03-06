% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieving_functions.R
\name{vin_row}
\alias{vin_row}
\title{vin_row: Retrieves rows for vin mpg lookup from fueleconomy.com}
\usage{
vin_row(year, make, model)
}
\arguments{
\item{year}{vehicle year}

\item{make}{vehicle make}

\item{model}{vehicle model}
}
\description{
Input make model and year to retrieve matching list with row ids
}
\examples{
vin_row("2015", "honda", "civic")
}
\keyword{vin}
