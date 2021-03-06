% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_RandSearchAlgorithm.R
\name{drawRandom}
\alias{drawRandom}
\title{Draw a random value for a given parameter}
\usage{
drawRandom(paramList, n)
}
\arguments{
\item{paramList}{A list object containing parameter ranges}

\item{n}{Number of values to draw from \link[stats]{runif}.}
}
\value{
A matrix containing the parameter grid to be tested
}
\description{
An auxiliary function used in \link{randomSearchOptim} to draw a random 
value of a parameter using an input range.
}
\examples{

params <- list("thresh"  = c(0.1,0.5),
               "minsize" = c(10,20))

drawRandom(paramList = params, n = 10)


}
\seealso{
\link[stats]{runif}
}
