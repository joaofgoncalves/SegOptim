% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_SegmentationFunctions.R
\name{segmentation_Terralib_MRGrow}
\alias{segmentation_Terralib_MRGrow}
\title{TerraLib Mean Region Growing segmentation algorithm}
\usage{
segmentation_Terralib_MRGrow(
  x,
  inputRstPath,
  outputSegmRst = NULL,
  Threshold = NULL,
  MinSize = NULL,
  verbose = TRUE,
  TerraLib.path = NULL
)
}
\arguments{
\item{x}{A vector with size two defining the segmentation parameters that will be optimized using 
genetic algorithms from GA package: 
\itemize{
  \item [1] Threshold (double), 
  \item [2] Minimum size of the segments (integer).
}
If you are using this function outside an optimization context it is better to directly define the 
segmentation parameters in \code{Threshold} and \code{MinSize}.}

\item{inputRstPath}{Path to the input raster file (typically a multi-layered raster dataset with 
segmentation features in each band).}

\item{outputSegmRst}{A path to the output segmented image (default: \code{NULL}; in this case the segmenter will 
internally determine a temporary output folder and file path inside the working directory)}

\item{Threshold}{Threshold (double)}

\item{MinSize}{Minimum size of the segments (integer)}

\item{verbose}{Print output messages? (default: TRUE).}

\item{TerraLib.path}{Path to the TerraLib binaries (locating \code{terralib_cli_rg} executable). 
If not set it uses the PATH env var to find it.}
}
\value{
A list object containing output file paths resulting from the segmentation run. 
These files will be cleaned after each GA iteration.
}
\description{
A function providing CLI access to TerraLib 5 Mean Region Growing segmentation and optimize 
its parameters using genetic algorithms.
}
\details{
By default all bands have the same weight (bw), i.e., bw = 1 / number of bands. Also, by default, 
the executable file for running this segmentation algorithm (named \code{terralib_cli_rg}) should 
be accessible by the PATH env variable.
}
\note{
Windows binaries for CLI access to TerraLib v5.1.1 Mean Region Growing segmenter are available at: 
\url{https://bitbucket.org/joao_goncalves/terralib5_cli_segmentation_mod/downloads} 
(unfortunetaley not yet tested in Linux environment...). Source could be used to build TerraLib for this 
effect, check source location \code{/examples/segmentationRegionGrowing} and TerraLib instructions 
(\href{http://www.dpi.inpe.br/terralib5/wiki/doku.php?id=wiki:terralib50_build}{link}).
}
