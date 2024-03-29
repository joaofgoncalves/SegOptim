% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_PrepFullDataset.R
\name{prepareCalData}
\alias{prepareCalData}
\title{Prepare calibration data for running a classification algorithm}
\usage{
prepareCalData(
  rstSegm,
  trainData,
  rstFeatures,
  thresh = 0.5,
  funs = c("mean", "sd"),
  minImgSegm = 30,
  bylayer = FALSE,
  tiles = NULL,
  verbose = TRUE,
  progressBar = FALSE
)
}
\arguments{
\item{rstSegm}{A path or a \code{SpatRaster} object containing the outputs of a segmentation algorithm 
with each object/segment identified by an integer index. Can also be the direct result of a segmentation 
function (e.g., \code{segmentation_OTB_LSMS}) as an object of class \code{SOptim.SegmentationResult}.}

\item{trainData}{Input train data used for classification. The input can be a \code{SpatRaster}, 
an integer vector (containing raster data after using \code{\link[terra]{values}} function) 
or a character string with a path to the raster layer. If x is an integer vector then it should 
have a length equal to the number of pixels in \code{rstSegm}.}

\item{rstFeatures}{A string defining the path to the raster features or a 
\code{SpatRaster} object.}

\item{thresh}{A threshold value defining the minimum proportion of the segment ]0, 1] that 
must be covered by a certain class to be considered as a training case. This threshold will 
only apply if \code{x} is a \code{SpatRaster} which means you are using train areas/pixels. 
If you are running a \emph{"single-class"} problem then this threshold only applies to the 
class of interest (coded as 1's). Considering this, if a given segment has a proportion cover 
of that class higher than \code{thresh} then it is considered a train case. In contrast, for 
the background class (coded as 0's), only segments/objects totaly covered by that class are 
considered as train cases.      
If you are running a \emph{"multi-class"} problem then \code{thresh} is applied differently. 
First, the train class is determined by a majority rule then if that class covers more than 
the value specified in \code{thresh} this case is kept in train data otherwise it will be 
filtered out. See also \code{useThresh}.}

\item{funs}{A character vector with the name(s) of the functions used to aggregate 
data (default: \code{c("mean", "sd")}).}

\item{minImgSegm}{Minimum number of image segments/objects necessary to generate train data.}

\item{bylayer}{Calculate statistics layer by layer instead of all at once? (slightly 
increases computation time but spares memory load; default: FALSE).}

\item{tiles}{Number of times that the image will be divided along the x and y axes. This means that 
the original raster data will be split into a number of blocks equal to tiles^2 (e.g., if 
\code{tiles = 5} this will generate 25 blocks/tiles) for reading. This number should be larger 
for large \code{SpatRaster} objects. This means that some fine tuning may be necessary to adjust 
this value according to available memory and raster input size.}

\item{verbose}{Print progress messages? (default: TRUE)}

\item{progressBar}{Boolean. Show progress bar? (default: FALSE).}
}
\value{
An object of class \emph{\strong{\code{SOptim.CalData}}} containing two elements:    
\enumerate{
\item \emph{\strong{calData}} - A data frame object containing calibration data for training and evaluating a 
classifier algorithm. The first column (named "SID") contains the ID of each segment, and the second 
column (named "train") holds the segment class (or label). The following n columns hold the classification 
features for training;
\item \emph{\strong{classifFeatData}} - A data frame containing all segments and features from inputs. The first 
column (named "SID") holds the unique identifier for each image segment. The following n columns are used 
as classification features. Typically this data set is used for predicting the target class after calibrating a 
certain classifier algorithm.
}
}
\description{
An auxiliary wrapper function used to generate train/evaluation data and calculating feature statistics by 
image segment. The output object can then be used in \code{\link{calibrateClassifier}} function 
for training a classification algorithm (with option \code{runFullCalibration=TRUE}).
}
