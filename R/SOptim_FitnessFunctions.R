
## ---------------------------------------------------------------------------------------------------------------- ##
## --- FITNESS FUNCTION -------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------------- ##

#' Fitness function used in genetic, random and grid search optimization algorithms
#' 
#' The fitness function takes a candidate solution to the problem as input (in this case 
#' the segmentation parameters) and produces an output value measuring how "fit" or how "good" 
#' the solution is with respect to the problem in hand (i.e., classification performance results).
#' 
#' 
#' @param x Vector with segmentation parameters that will be optimized by the genetic algorithms from GA package.
#' 
#' @param rstFeatures Features used for supervised classification (typically a multi-layer raster with one feature 
#' per band). May be defined as a string with the path to a raster dataset or a \code{RasterStack} object.
#' 
#' @param trainData Input train data used for supervised classification. It can be a \code{RasterLayer} containing 
#' train areas (in raster format) or a \code{SpatialPointsDataFrame} object containing train points (in this case, 
#' the data must contain a column named "train" holding the train classes). For points you can use \link[rgdal]{readOGR} 
#' function to read input files (e.g., ESRI Shapefile) or use \link[sp]{SpatialPointsDataFrame} to create a valid 
#' data object within \code{R}.
#' 
#' @param ... Additional parameters passed to the segmentation functions that will not be optimized (see also: 
#' \code{\link{segmentationGeneric}}). It must also contain the input segmentation data (typically a multi-layer 
#' raster dataset with one input feature per band) depending one the algorithm selected. 
#' 
#' @param trainThresh A threshold value defining the minimum proportion of the segment ]0, 1] that must be covered 
#' by a certain class to be considered as a training case. This threshold will only apply if \code{x} is a \code{RasterLayer} 
#' which means you are using train areas/pixels. 
#' If you are running a \emph{"single-class"} problem then this threshold only applies to the class of interest (coded as 1's). 
#' Considering this, if a given segment has a proportion cover of that class higher than \code{thresh} then it is 
#' considered a train case. In contrast, for the background class (coded as 0's), only segments/objects 
#' totaly covered by that class are considered as train cases.      
#' If you are running a \emph{"multi-class"} problem then \code{thresh} is applied differently. First, the train class is 
#' determined by a majority rule then if that class covers more than the value specified in \code{thresh} this case is 
#' kept in train data otherwise it will be filtered out. See also \code{useThresh}. 
#' 
#' @param segmStatsFuns An aggregation function (e.g., \code{mean}) applied to the elements within each segment. 
#' Either a function object or a function name.
#' 
#' @param ndigits Number of decimal plates to consider for rounding the fitness function output. For example,
#' if \code{ndigits=2} then only improvements of 0.01 will be considered by the GA algorithm.
#' 
#' @param verbose Print output messages? (default: TRUE).
#' 
#' @inheritParams segmentationGeneric
#' @inheritParams calibrateClassifier
#' @inheritParams calculateSegmentStats
#' @inheritParams getTrainData
#' 
#' @return The fitness function value (depends on the option set in \code{evalMetric}).
#' 
#' @details 
#' \emph{"A fitness function is a particular type of objective function that is used to 
#' summarise, as a single figure of merit, how close a given design solution is to 
#' achieving the set aims"} (from \href{https://en.wikipedia.org/wiki/Fitness_function}{wikipedia}).      
#' In particular the fitness function also acts as a 'wrapper' function linking together several 
#' other in the following worflow sequence:    
#'  
#' \enumerate{
#'   \item Run segmentation and load the results;
#'   \item Load train data for the segmentation generated;
#'   \item Extract feature data for the segments (segment statistics calculation);
#'   \item Merge calibration and feature data;
#'   \item Do train/test data partitions;
#'   \item Perform data balancing (if required by user and only for the train and single-class);
#'   \item Perform classification using the selected algorithm;
#'   \item Do performance evaluation for each subset;
#'   \item Return evaluation score (fitness value);
#' }
#' 
#' @seealso 
#' Check the segmentation parameters and data used by each algorithm that must be 
#' defined in \code{...}:    
#' \itemize{
#'    \item \code{\link{segmentationGeneric}},      
#'    \item \emph{SAGA Seeded Region Growing}: \code{\link{segmentation_SAGA_SRG}},       
#'    \item \emph{GRASS Region Growing}: \code{\link{segmentation_GRASS_RG}},       
#'    \item \emph{ArcGIS Mean Shift}: \code{\link{segmentation_ArcGIS_MShift}},        
#'    \item \emph{TerraLib Baatz-Schaphe}: \code{\link{segmentation_Terralib_Baatz}},         
#'    \item \emph{Terralib Mean Region Growing}: \code{\link{segmentation_Terralib_MRGrow}},        
#'    \item \emph{RSGISLib Shepherd}: \code{\link{segmentation_RSGISLib_Shep}},
#'    \item \emph{OTB Large Scale Mean Shift}: \code{\link{segmentation_OTB_LSMS}},
#'    \item \emph{OTB Large Scale Mean Shift with two sets of parameters}: \code{\link{segmentation_OTB_LSMS2}}      
#' }
#' 
#' 
#' @import unbalanced
#' @import class
#' @import gbm
#' @import randomForest
#' @import e1071
#' @import mda
#' @importFrom raster raster
#' 
#' @export

fitFuncGeneric <- function(x,
                         rstFeatures,           # raster stack with classification features
                         trainData,             # raster layer with calibration data   
                         segmentMethod,
                         ...,                  # parameters passed to the segmentation generic (exception to x)
                         trainThresh = 0.5,
                         segmStatsFuns = c("mean","sd"),
                         bylayer = FALSE,
                         classificationMethod = "RF",
                         classificationMethodParams = NULL,
                         balanceTrainData = FALSE,
                         balanceMethod = "ubUnder",
                         evalMethod = "5FCV",
                         trainPerc = 0.8,
                         nRounds = 20,
                         evalMetric = "Kappa",
                         minTrainCases = 30,
                         minCasesByClassTrain = 10,
                         minCasesByClassTest = 10,
                         minImgSegm = 30,
                         ndigits = 2,
                         verbose = TRUE){
  
  
  # Run segmentation and load the results -----------------------------------------------------
  #
  #
  if(verbose) cat("-> Performing OBIA segmentation with method",segmentMethod,"...\n")
  
  # Run segmentation generic function
  # x contains parameters that will be optimized
  segm <- try(segmentationGeneric(segmentMethod=segmentMethod, x = x, ...))

  
  if(inherits(segm,"try-error")){
    warning("An error occurred while trying to perform the selected segmentation algorithm!")
    return(NA)
  }
  
  # Load raster segmentation data generated for a given solution
  rstSegm <- try(raster::raster(segm[["segm"]]))
  
  if(inherits(rstSegm,"try-error")){
    warning("An error occurred while trying to load the segmentation raster!")
    return(NA)
  }
  
  if(verbose) cat("done.\n\n")
  
  
  # On-exit remove temporary files from the working directory ---------------------------------
  #
  #
  filesToremove <- unlist(segm)
  on.exit(try(doCleanUpActions(filesToremove, recursive=TRUE)))
  
  
  # Load train data ---------------------------------------------------------------------------
  #
  #
  if(verbose) cat("-> Loading train data into new image segments...\n")
  
  # print(segm)
  # file.exists(segm[["segm"]])
  # print(rstSegm)
  # print(class(trainData))
  # print(class(rstSegm))
  
  calibrationDF <- try(getTrainData(x          = trainData, 
                                    rstSegm    = rstSegm, 
                                    thresh     = trainThresh, 
                                    minImgSegm = minImgSegm))
 
  if(inherits(calibrationDF,"try-error") || is.na(calibrationDF)){ # A NA is passed to calibrationDF if the number of segments is low (< minNumImgSegm)!
    warning("An error occurred while generating train data! Check segmentation parameter ranges? Perhaps input train data?")
    return(NA)
  }
  
  #nClassType <- attr(calibrationDF, "nClassType")
  
  if(verbose) cat("done.\n\n")
  
  
  # Extract feature data for the segments -----------------------------------------------------
  #
  # -> [MOD: 15/03/2017] Changed this part to calculate segment statistics only for the subset 
  # of train segments in calibrationDF$SID. Uses data.table subset features.
  #
  if(verbose) cat("-> Calculating feature statistics for the new image segments...\n")
  
  # featDF<-try(calculateSegmentStats(rstFeatures=rstFeatures, rstSegm=rstSegm, 
  #                                   funs=segmStatsFuns, na.rm=TRUE, subset=unique(calibrationDF$SID)))
  
  featDF <- try(calculateSegmentStats(rstFeatures = rstFeatures, 
                                      rstSegm     = rstSegm, 
                                      funs        = segmStatsFuns, 
                                      na.rm       = TRUE, 
                                      bylayer     = bylayer,
                                      subset      = unique(calibrationDF$SID)))
  
  if(inherits(featDF,"try-error")){
    warning("An error occurred while calculating segmentation statistics for feature data!")
    return(NA)
  }
  
  if(verbose) cat("done.\n\n")
  
  
  # Merge calibration data and feature data ----------------------------------------------------
  #
  #
  if(verbose) cat("-> Merging train and feature data...\n")
  
  calibrationDF <- merge(calibrationDF, featDF, by="SID", all.x = TRUE, all.y = FALSE)
  
  # Remove no data values
  calibrationDF <- NRV.omit(calibrationDF)
  
  if(verbose) cat("done.\n\n")
  

  # Perform classification and performance evaluation ------------------------------------------
  #
  #
  
  if(verbose) cat("-> Training classifier with method",classificationMethod,"...\n\n")
    
  out <- calibrateClassifier(calData                  = calibrationDF,
                           classificationMethod       = classificationMethod,
                           classificationMethodParams = classificationMethodParams,
                           balanceTrainData           = balanceTrainData,
                           balanceMethod              = balanceMethod,
                           evalMethod                 = evalMethod,
                           evalMetric                 = evalMetric,
                           trainPerc                  = trainPerc,
                           nRounds                    = nRounds,
                           #nClassType                = nClassType,
                           minTrainCases              = minTrainCases,
                           minCasesByClassTrain       = minCasesByClassTrain,
                           minCasesByClassTest        = minCasesByClassTest,
                           verbose                    = verbose
  )

  if(verbose) cat("done.\n\n\n")
  
  
  # Return the final result (objective function value) ------------------------------------------
  #
  #
  return(round(out, digits = ndigits))
  
}

