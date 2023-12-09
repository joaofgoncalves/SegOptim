

#' Prepare calibration data for running a classification algorithm
#' 
#' An auxiliary wrapper function used to generate train/evaluation data and calculating feature statistics by 
#' image segment. The output object can then be used in \code{\link{calibrateClassifier}} function 
#' for training a classification algorithm (with option \code{runFullCalibration=TRUE}).
#' 
#' @param rstSegm A path or a \code{SpatRaster} object containing the outputs of a segmentation algorithm 
#' with each object/segment identified by an integer index. Can also be the direct result of a segmentation 
#' function (e.g., \code{segmentation_OTB_LSMS}) as an object of class \code{SOptim.SegmentationResult}.
#' 
#' @param trainData Input train data used for classification. The input can be a \code{SpatRaster}, 
#' an integer vector (containing raster data after using \code{\link[terra]{values}} function) 
#' or a character string with a path to the raster layer. If x is an integer vector then it should 
#' have a length equal to the number of pixels in \code{rstSegm}.
#' 
#' @param verbose Print progress messages? (default: TRUE)
#' 
#' @param tiles Number of times that the image will be divided along the x and y axes. This means that 
#' the original raster data will be split into a number of blocks equal to tiles^2 (e.g., if 
#' \code{tiles = 5} this will generate 25 blocks/tiles) for reading. This number should be larger 
#' for large \code{SpatRaster} objects. This means that some fine tuning may be necessary to adjust 
#' this value according to available memory and raster input size. 
#' 
#' @inheritParams getTrainData
#' 
#' @inheritParams calculateSegmentStats
#' 
#' @return An object of class \emph{\strong{\code{SOptim.CalData}}} containing two elements:    
#' \enumerate{
#' \item \emph{\strong{calData}} - A data frame object containing calibration data for training and evaluating a 
#' classifier algorithm. The first column (named "SID") contains the ID of each segment, and the second 
#' column (named "train") holds the segment class (or label). The following n columns hold the classification 
#' features for training;
#' \item \emph{\strong{classifFeatData}} - A data frame containing all segments and features from inputs. The first 
#' column (named "SID") holds the unique identifier for each image segment. The following n columns are used 
#' as classification features. Typically this data set is used for predicting the target class after calibrating a 
#' certain classifier algorithm.
#' }
#' 
#' @export
#' @importFrom stats sd
#' @importFrom terra rast
#' 

prepareCalData <- function(rstSegm, trainData, rstFeatures, thresh = 0.5, 
                           funs = c("mean","sd"), minImgSegm = 30, 
                           bylayer = FALSE, tiles = NULL, 
                           verbose = TRUE, progressBar = FALSE){
  

  # Load train data ------------------------------------------------------------------------
  #
  #
  if(verbose) cat("-> [1/3] Loading train data into image segments...\n")
  
  if(inherits(rstSegm,"SOptim.SegmentationResult"))
    rstSegm <- terra::rast(rstSegm$segm)
  
  calibrationDF <- try(getTrainData(x         = trainData, 
                                    rstSegm   = rstSegm, 
                                    useThresh = TRUE, 
                                    thresh    = thresh, 
                                    minImgSegm = minImgSegm))
  
  if(inherits(calibrationDF,"try-error") || is.na(calibrationDF)[1]){ # A NA is passed to calibrationDF if the number of segments is low (< minNumImgSegm)!
    warning("An error occurred while generating train data! Check segmentation parameter ranges? Perhaps input train data?")
    return(NA)
  }
  
  nClassType <- attr(calibrationDF, "nClassType")
  if(verbose) cat("done.\n\n")
  
  
  # Extract feature data for the segments -----------------------------------------------------
  #
  #
  if(verbose) cat("-> [2/3] Calculating feature statistics for all image segments...\n")
  
  # Make raster tiles object?
  if(!is.null(tiles)){
    if(is.integer(tiles)){
      tiles <- createRasterTiles(rstSegm, nd = tiles)
    }
  }
  
  classificationFeaturesDF <- try(calculateSegmentStats(rstFeatures = rstFeatures, 
                                                        rstSegm     = rstSegm, 
                                                        funs        = funs, 
                                                        na.rm       = TRUE, 
                                                        bylayer     = bylayer, 
                                                        tiles       = tiles,
                                                        progressBar = progressBar))
  
  if(inherits(classificationFeaturesDF,"try-error")){
    warning("An error occurred while calculating segmentation statistics for feature data!")
    return(NA)
  }
  if(verbose) cat("done.\n\n")
  
  
  # Merge calibration data and feature data ----------------------------------------------------
  #
  #
  if(verbose) cat("-> [3/3] Merging train and feature data...\n")
  
  calibrationDF <- merge(calibrationDF, classificationFeaturesDF, by="SID", 
                         all.x = TRUE, all.y = FALSE) # Merge features with cal data
  
  calibrationDF <- NRV.omit(calibrationDF) # Remove no data values
 
  if(verbose) cat("done.\n\n")
  
  out <- list(calData = calibrationDF, classifFeatData = classificationFeaturesDF)
  attr(out, "nClassType") <- nClassType
  class(out) <- "SOptim.CalData"
  
  return(out)
  
}


#' Basic output functions for SOptim.CalData calibration data objects
#' 
#' Provides a set of generic output functions for summary, print, head and 
#' tail methods.
#' 
#' @name outputSOptimCalData
#' 
#' @param x,object An object of class \code{SOptim.CalData}.
#' 
#' @param n a single integer. If positive, size for the resulting object: number 
#' of rows for a matrix or data frame. If negative, all but the n last/first number 
#' of elements of x (default: 6L).
#' 
#' @param ... Additional arguments to pass to print functions.
#' 
#' @importFrom utils head
#' @importFrom utils tail
NULL

#' @rdname outputSOptimCalData
#' 
summary.SOptim.CalData <- function(object, ...) lapply(object, summary, ...)

#' @rdname outputSOptimCalData
#' 
print.SOptim.CalData <- function(x, ...) lapply(x, print, ...)

#' @rdname outputSOptimCalData
#' 
head.SOptim.CalData <- function(x, n=6L, ...) lapply(x, head, n=n, ...)

#' @rdname outputSOptimCalData
#' 
tail.SOptim.CalData <- function(x, n=6L, ...) lapply(x, tail, n=n, ...)

