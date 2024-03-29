% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_SimulateTestData-v1.R
\name{simRasterFeatures}
\alias{simRasterFeatures}
\title{Simulate raster features}
\usage{
simRasterFeatures(
  nr = 100,
  nc = 100,
  nlyrs = 5,
  distr = "rnorm",
  args = list(n = nr * nc, mean = 0, sd = 1)
)
}
\arguments{
\item{nr}{Number of rows for the simulated \code{SpatRaster} (default: 100).}

\item{nc}{Number of columns for the simulated \code{SpatRaster} (default: 100).}

\item{nlyrs}{Number of layers or bands to simulate (default: 5)}

\item{distr}{Name of the function to simulate data (default: random normal "rnorm")}

\item{args}{A list object with arguments to use in \code{do.call} passed to the 
function set in \code{distr} (default: \code{list(n = nr * nc, mean = 0, sd = 1)} 
for rnorm)}
}
\value{
A \code{SpatRaster} with simulated data
}
\description{
Simulate a \code{SpatRaster} with multiple layers similar to features used in 
segmentation or supervised classification.
}
