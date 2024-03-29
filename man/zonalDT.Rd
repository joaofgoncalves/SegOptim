% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_CalcSegmentStats.R
\name{zonalDT}
\alias{zonalDT}
\alias{zonalDT.default}
\alias{zonalDT.data.frame}
\alias{zonalDT.SpatRaster}
\alias{zonalDT.matrix}
\title{Calculate aggregated statistics by each image segment or region}
\usage{
zonalDT(x, z, fun, na.rm = TRUE, subset = NULL, ...)

\method{zonalDT}{default}(x, z, fun, na.rm = TRUE, subset = NULL, ...)

\method{zonalDT}{data.frame}(x, z, fun, na.rm = TRUE, subset = NULL, ...)

\method{zonalDT}{SpatRaster}(x, z, fun, na.rm = TRUE, subset = NULL, ...)

\method{zonalDT}{matrix}(x, z, fun, na.rm = TRUE, subset = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{data.frame}, \code{matrix} or \code{SpatRaster} 
containing data (e.g., classification features).}

\item{z}{An object of class \code{SpatRaster}, \code{vector}, or a one column \code{data.frame}/\code{matrix} 
containing segment/object/zone indices (as integers). Usually the result of a image segmentation algorithm.}

\item{fun}{An aggregation function (e.g., \code{mean}) applied to the elements within each segment. 
Either a function object or a function name.}

\item{na.rm}{A boolean defining whether or not to remove NA values (default: \code{TRUE}).}

\item{subset}{A vector defining which rows should be subset from z (zones or regions).}

\item{...}{Further parameters to pass to \code{lapply}.}
}
\value{
A data.table object containing statistics by segment.
}
\description{
Calculate statistics for each element (e.g., a segment, object or region) based on \code{data.table} objects 
(much faster processing!).
}
\note{
Both object must have exactly the same number of elements. For example, if \code{x} and \code{z} are 
raster datasets these must have equal number of rows/columns, extent and coordinate reference system. If  
\code{x} is a \code{data.frame} or \code{matrix} then the number of cells in \code{z} must equal the number 
of rows in \code{x}.
}
