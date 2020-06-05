

#' Perform grid or random search optimization of segmentation/OBIA parameters
#' 
#' This function performs the optimization of segmentation parameters using a simple grid or random search algorithm. It also 
#' verifies if input data and parameters are parsable.
#' 
#' @param optimMethod A string defining which type of optimizing to use. Options are: \code{"random"} (default) 
#' for randomized search, or, \code{"grid"} for exhaustive grid search.
#' 
#' @param grid.searchSize This value will be used to extend parameter ranges. For example, if \code{gridSearchSize = 5} 
#' and a given parameter range is set to p=[0,1] this will generate the following regular sequence: 
#' \code{p = c(0.00, 0.25, 0.50, 0.75, 1.00)}. All combinations from extended parameters will then be used for grid search 
#' method.(default: 5)
#' 
#' @param rand.numIter Number of iteration for random search optimization (default: 250).
#' 
#' @param rand.nneigh Number of neighbors (or parameter combinations) to generate in the vicinity 
#' of the best (default: 5).
#' 
#' @param rand.initNeighs Number of parameter combinations to randomly draw from paramList at initialization 
#' (default: 5 * nneigh)
#' 
#' @param rand.neighSizeProp Size of the neighbourhood for a given parameter, i.e., a real value contained 
#' in ]0, 1] used to multiply the range size as: 
#' \eqn{neighSize = (max_{range} - min_{range}) \times neighSizeProp}(default: 0.025)
#'  
#' @param rand.iter Number of sucessive iterations used to stop the algorithm if no improvement is found 
#' (default: 25).
#' 
#' @param segmParamList A named list object containing the parameters that will be optimized for the selected 
#' segmentation method. Check parameter names with function \link{segmentationParamNames}. The list should contain two values 
#' with parameter ranges (min, max). For example, considering method \code{"GRASS_SRG"}, the 
#' list could be: \code{list(Threshold = c(0.1, 0.5), MinSize = c(10, 50))}.
#' 
#' @param parallel A logical argument specifying if parallel computing should be used (TRUE) or not (FALSE, default) for 
#' evaluating the fitness function. This argument could also be used to specify the number of cores to employ; by default, 
#' this is taken from \link[parallel]{detectCores}. Finally, the functionality of parallelization depends on system OS: on 
#' Windows only 'snow' type functionality is available, while on Unix/Linux/Mac OSX both 'snow' and 'multicore' (default) 
#' functionalities are available.
#' 
#' @param seed An integer value containing the random number generator state. This argument can be used to replicate the results 
#' of a grid search. Note that if parallel computing is required, the doRNG package must be installed
#' 
#' @inheritParams fitFuncGeneric
#' @inheritParams segmentationGeneric
#' @inheritParams calibrateClassifier
#' @inheritParams calculateSegmentStats
#' @inheritParams getTrainData
#' 
#' @return 
#' A data frame containing the segmentation parameters tested and the respective value of the evaluation metric (by default 
#' the table is ordered in decreasing order using this column).
#' 
#' @seealso 
#' Check the segmentation parameters and data used by each algorithm that must be defined in \code{...} and 
#' \code{segmParamList}:    
#' \itemize{
#'    \item \code{\link{segmentationGeneric}},      
#'    \item \emph{SAGA Seeded Region Growing}: \code{\link{segmentation_SAGA_SRG}},       
#'    \item \emph{GRASS Region Growing}: \code{\link{segmentation_GRASS_RG}},       
#'    \item \emph{ArcGIS Mean Shift}: \code{\link{segmentation_ArcGIS_MShift}},        
#'    \item \emph{TerraLib Baatz-Schaphe}: \code{\link{segmentation_Terralib_Baatz}},         
#'    \item \emph{Terralib Mean Region Growing}: \code{\link{segmentation_Terralib_MRGrow}},        
#'    \item \emph{RSGISLib Shepherd}: \code{\link{segmentation_RSGISLib_Shep}},
#'    \item \emph{RSGISLib OTB Large Scale Mean Shift}: \code{\link{segmentation_OTB_LSMS}}     
#' }       
#' \link{segmentationParamNames} can be used to output a short list of parameter names to include in \code{segmParamList} for 
#' optimization. 
#' 
#' @import unbalanced
#' @import class
#' @import gbm
#' @import randomForest
#' @import e1071
#' @import mda
#' @import parallel
#' @import foreach
#' @import doRNG
#' @importFrom raster raster
#' @importFrom GA startParallel
#' @importFrom stats runif
#' 
#' @export


searchOptimSegmentationParams <- function(rstFeatures,              # raster stack with classification features
                                           trainData,             # raster layer with calibration data   
                                           segmentMethod,         # Segmentation method to use
                                           ...,                   # Additional parameters passed to the segmentation method
                                           optimMethod = "random",
                                           segmParamList,
                                           grid.searchSize = 5,
                                           rand.numIter = 250, 
                                           rand.nneigh = 5, 
                                           rand.initNeighs = (5 * rand.nneigh), 
                                           rand.neighSizeProp = 0.025, 
                                           rand.iter = 25,
                                           trainThresh = 0.5,
                                           segmStatsFuns = c("mean","sd"),
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
                                           verbose = TRUE,
                                           parallel = FALSE,
                                           seed = NULL){
  
  
  ## Check parameters -------------------------------------------------------------
  
  if(verbose) cat("-> Checking input parameters... \n")
  
  if(!doInputVerification(trainThresh, "trainThresh")) 
    stop("trainThresh must be within ]0.00, 1.00[ range! Please check input parameters")
  
  if(!doInputVerification(segmStatsFuns, "segmStatsFuns")) 
    stop("Functions or function names in segmStatsFuns are not valid! Please check input parameters")
  
  if(!doInputVerification(segmentMethod, "segmMethod")) 
    stop("Segmentation method in segmentMethod is not valid! Please check input parameters")
  
  if(balanceTrainData){
    if(!doInputVerification(balanceMethod, "dataBalancing")) 
      stop("Data balancing method in balanceMethod is not valid! Please check input parameters")
  }
  
  if(!doInputVerification(classificationMethod, "clAlgo")) 
    stop("Classification method in classificationMethod is not valid! Please check input parameters")
  
  if(!is.null(classificationMethodParams)){
    if(!doInputVerification(classificationMethodParams, "classifierParams")) 
      stop("The parameters defined in classificationMethodParams are not valid! Please check input parameters")
  }
  
  if(evalMethod=="HOCV"){
    if(!doInputVerification(trainPerc, "trainPerc")) 
      stop("trainPerc must be within ]0.00, 1.00[ range! Please check input parameters")
  }
  
  if(!doInputVerification(evalMethod, "evalMethod")) 
    stop("Evaluation method in evalMethod is not valid! Please check input parameters")
  
  if(!(doInputVerification(evalMetric, "evalMetr_sc") || doInputVerification(evalMetric, "evalMetr_mc")))
    stop("Evaluation metric in evalMetric is not valid! Please check input parameters")
  
  if(inherits(trainData, "SpatialPointsDataFrame")){
    if(!("train" %in% colnames(trainData@data)))
      stop("To use SpatialPointsDataFrame as input data this object must contain a column named \"train\" 
           with train class labels (as integers)!")
  }
  
  ## Check segmParamList -------------------------------------------------------------------
  
  segmParamAlgo <- segmentationParamNames()[[segmentMethod]]
  
  if(!is.list(segmParamList)){
    stop("segmParamList must be an object of class list with the following elements/names: ",
         paste(segmParamAlgo,collapse=", "))
  }
  
  if(is.null(names(segmParamList))){
    stop("segmParamList must be a named list with the following elements: ",
         paste(segmParamAlgo,collapse=", "))
  }
  
  if(!all(segmParamAlgo %in% names(segmParamList))){
    stop("The segmParamList for segmentation method ",segmentMethod," must contain all the following elements: ",
         paste(segmParamAlgo,collapse=", "),". Please check input parameters and segmParamList names.")
  }
  
  if(!all(unlist(lapply(segmParamList, FUN = function(x) length(x) == 2))))
    stop("All elements in segmParamList must contain parameters min/max intervals with length = 2 (min_parameter_value, max_parameter_value)!")
  
  if(!all(unlist(lapply(segmParamList, FUN = function(x) x[2] > x[1]))))
    stop("All elements in segmParamList must be ordered as (min_value, max_value)!")

  if(verbose) cat("All OK!\n\n")
  
  # Re-order the segmParamList
  #
  segmParamList <- segmParamList[segmParamAlgo]
  
  
  if(verbose){
    cat("Generating random parameter combinations from:\n\n")
    print(segmParamList)
    cat("\n")
  }
  
  ## Check and get metadata ---------------------------------------------------------
  
  if(is.character(rstFeatures)){
    if(file.exists(rstFeatures)){
      
      if(verbose) cat("-> Loading raster metadata containing classification features... \n")
      rstFeatures<-try(raster::stack(rstFeatures))
      if(verbose) cat("done.\n\n")
      
      if(inherits(rstFeatures,"try-error")){
        stop("An error occurred while reading raster metadata from rstFeatures!")
      }
    }
  }
  
  if(is.character(trainData)){
    if(file.exists(trainData)){
      
      if(verbose) cat("-> Loading raster metadata containing training examples... \n")
      trainData<-try(raster::raster(trainData))
      if(verbose) cat("done.\n\n")
      
      if(inherits(trainData,"try-error")){
        stop("An error occurred while reading raster metadata from trainData!")
      }
    }
  }
  
  ## Check raster similarity -------------------------------------------------------
  
  if(inherits(trainData,"RasterLayer")){
    
    if(!raster::compareRaster(rstFeatures,trainData)){
      stop("Differences found between rasters in rstFeatures and trainData! Check raster extent, pixel size, 
            nr. of rows/cols, or coordinate reference system")
    }
  }
  
  ## Load raster data from file ----------------------------------------------------
  
  # [Jun/2020] Removed after changes in calculateSegmStats() function which now uses 
  # only a raster dataset as input
  
  # if(inherits(rstFeatures,"RasterStack")){
  #   
  #   if(verbose) cat("-> Loading raster values for classification features (in-memory load)... \n")
  #   
  #   rstFeatures <- try(raster::values(rstFeatures))
  #   
  #   if(verbose) cat("done.\n\n")
  #   
  #   if(inherits(rstFeatures,"try-error")){
  #     stop("An error occurred while reading data values from rstFeatures!")
  #   }
  # }
  
  if(inherits(trainData,"RasterLayer")){
    
    if(verbose) cat("-> Loading raster values for train data (in-memory load)... \n")
    
    trainData <- try(raster::values(trainData))
    
    if(verbose) cat("done.\n\n")
    
    if(inherits(trainData,"try-error")){
      stop("An error occurred while reading data values from trainData!")
    }
  }
  

  
  ## Run the optimization for the selected algorithm --------------------------------
  
  if(optimMethod == "grid"){
    
    
    ## Generate parameter grid with all combinations --------------------------------
    
    # Expand the segmParamList by gridSearchSize
    segmParamListExpand <- lapply(segmParamList, FUN = function(x) seq(x[1], x[2], length.out = grid.searchSize))
    
    # Generate all combinations of parameters
    paramGrid <- expand.grid(segmParamListExpand)
    nr <- nrow(paramGrid)
    
    
    
    # Start parallel computing (if needed) ------------------------------------------
    
    if(verbose && parallel) cat("-> Starting cluster for parallel execution...\n")
    
    if(is.logical(parallel)){ 
      # Start cluster with all available cores
      if(parallel){
        parallel <- GA::startParallel(parallel) 
      } # Don't use parallel
      else{
        parallel <- FALSE
      }  
    }
    else{ 
      # Integer with number of cores or cluster object
      parallel <- GA::startParallel(parallel) 
    }
    
    if(parallel) on.exit(parallel::stopCluster(attr(parallel, "cluster")))
    
    
    # set seed for reproducibility  
    if(!is.null(seed)) set.seed(seed)
    i. <- NULL # dummy to trick R CMD check 
    
    if(verbose && parallel) cat("done.\n\n")
    
    
    if(verbose) cat("### ---- [",nr,"] PARAMETER COMBINATIONS WERE GENERATED FOR OBIA ---- ###\n\n")
    
    ## Run fitness function in sequential or parallel ---------------------------------------------------------
    
    if(parallel){
      
      outEval <- foreach(i. = seq_len(nr), .combine = "c") %dorng% {
        
        ## Correct the order of segmentation parameters
        segmParams <- as.numeric(paramGrid[i., ])
        #names(segmParams) <- colnames(paramGrid)
        #segmParams <- segmParams[segmParamAlgo] 
        
        # Run the fitness function
        out <- fitFuncGeneric(x                          = segmParams,
                              rstFeatures                = rstFeatures,        
                              trainData                  = trainData,            
                              segmentMethod              = segmentMethod,
                              ...,      
                              trainThresh                = trainThresh,
                              segmStatsFuns              = segmStatsFuns,
                              classificationMethod       = classificationMethod,
                              classificationMethodParams = classificationMethodParams,
                              balanceTrainData           = balanceTrainData,
                              balanceMethod              = balanceMethod,
                              evalMethod                 = evalMethod,
                              trainPerc                  = trainPerc,
                              nRounds                    = nRounds,
                              evalMetric                 = evalMetric,
                              minTrainCases              = minTrainCases,
                              minCasesByClassTrain       = minCasesByClassTrain,
                              minCasesByClassTest        = minCasesByClassTest,
                              minImgSegm                 = minImgSegm,
                              verbose                    = verbose)
        out
      }
      
    }else{
      
      # Output vector with evaluation data
      #
      outEval <- vector(mode="numeric", length=nr)
      
      for(i in 1:nr){
        
        # Concatenate and sort parameters correctly (following the order of input x in segmentationParamNames function)
        #
        segmParams <- as.numeric(paramGrid[i, ])
        #names(segmParams) <- colnames(paramGrid)
        #segmParams <- segmParams[segmParamAlgo] 
        
        # Run the fitness function
        out <- fitFuncGeneric(x                          = segmParams,
                              rstFeatures                = rstFeatures,        
                              trainData                  = trainData,            
                              segmentMethod              = segmentMethod,
                              ...,      
                              trainThresh                = trainThresh,
                              segmStatsFuns              = segmStatsFuns,
                              classificationMethod       = classificationMethod,
                              classificationMethodParams = classificationMethodParams,
                              balanceTrainData           = balanceTrainData,
                              balanceMethod              = balanceMethod,
                              evalMethod                 = evalMethod,
                              trainPerc                  = trainPerc,
                              nRounds                    = nRounds,
                              evalMetric                 = evalMetric,
                              minTrainCases              = minTrainCases,
                              minCasesByClassTrain       = minCasesByClassTrain,
                              minCasesByClassTest        = minCasesByClassTest,
                              minImgSegm                 = minImgSegm,
                              verbose                    = verbose)
        # Eval metric value
        outEval[i] <- out
        
        if(verbose) cat("-> Finished round",i,"[",round((i/nr)*100, 1),"% ] completed.\n\n")
        
      }
    }
    
    # Combine results from segmentation and classification and parameters
    #
    paramGrid <- cbind(paramGrid, outEval)
    colnames(paramGrid)[ncol(paramGrid)] <- evalMetric
    
    paramGrid <- paramGrid[order(paramGrid[,ncol(paramGrid)], decreasing = TRUE), , drop = FALSE]
    
    if(all(is.na(outEval))){
      if(verbose) cat("All rounds returned NA values! This means that segmentation and/or classification routines recorded some problem. Please review input parameters.")
    } 
    else if(verbose){
      cat("-> BEST RESULTS WERE OBTAINED FOR THE FOLLOWING PARAMETERS:\n\n")
      print(paramGrid[1, , drop=FALSE])
    } 
    
    # Return the parameter grid with eval metric results ------------------------------------------
    #
    #
    return(paramGrid)
  }
  else if(optimMethod == "random"){
    
    outEval <- randomSearchOptim( fitFunc       = fitFuncGeneric, 
                                  paramList     = segmParamList, 
                                  numIter       = rand.numIter, 
                                  nneigh        = rand.nneigh, 
                                  initNeighs    = rand.initNeighs,
                                  neighSizeProp = rand.neighSizeProp, 
                                  maximize      = TRUE, 
                                  iter          = rand.iter, 
                                  parallel      = parallel, 
                                  seed          = seed, 
                                  ## //// PARAMETERS FOR THE FITNESS FUNCTION //// ##
                                  # x is passed directly in iterateFitnessFunc  
                                  # x                        = segmParams,
                                  rstFeatures                = rstFeatures,        
                                  trainData                  = trainData,            
                                  segmentMethod              = segmentMethod,
                                  ...,      # parameters passed to the segmentation function through fitFuncGeneric
                                  trainThresh                = trainThresh,
                                  segmStatsFuns              = segmStatsFuns,
                                  classificationMethod       = classificationMethod,
                                  classificationMethodParams = classificationMethodParams,
                                  balanceTrainData           = balanceTrainData,
                                  balanceMethod              = balanceMethod,
                                  evalMethod                 = evalMethod,
                                  trainPerc                  = trainPerc,
                                  nRounds                    = nRounds,
                                  evalMetric                 = evalMetric,
                                  minTrainCases              = minTrainCases,
                                  minCasesByClassTrain       = minCasesByClassTrain,
                                  minCasesByClassTest        = minCasesByClassTest,
                                  minImgSegm                 = minImgSegm,
                                  verbose                    = verbose)
    
    
    if(all(is.na(outEval))){
      if(verbose) cat("All rounds returned NA values! This means that segmentation and/or classification routines recorded some problem. Please review input parameters.")
    } 
    else if(verbose){
      cat("-> BEST RESULTS WERE OBTAINED FOR THE FOLLOWING PARAMETERS:\n\n")
      print(outEval)
    } 
    
    return(outEval)
    
  }
  else{
    stop("Unknown option in optimMethod! Please review input parameters.")
  }
  
  
  
}



