

## ---------------------------------------------------------------------------------------------------------------- ##
## --- GA OPTIMIZATION FUNCTIONS ------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------------- ##


#' Optimization of segmentation parameters using genetic algorithms
#' 
#' This function makes some data checks and then performs the optimization of segmentation parameters 
#' using genetic algorithms.
#' 
#' @inheritParams fitFuncGeneric
#' @inheritParams segmentationGeneric
#' @inheritParams calibrateClassifier
#' @inheritParams calculateSegmentStats
#' @inheritParams getTrainData
#' @inheritParams GA::ga
#' 
#' 
#' 
#' @return An object with \pkg{GA} optimization results. See \link[GA]{ga-class} for a description of available 
#' slots information.
#' 
#' @details 
#' 
#' -- INTRODUCTION --
#'         
#' Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological 
#' evolution and natural selection. GAs simulate the evolution of living organisms, where the fittest individuals 
#' dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, 
#' crossover and mutation. The GA package is a collection of general purpose functions that provide a flexible 
#' set of tools for applying a wide range of genetic algorithm methods (from package \pkg{GA}).
#' By default \pkg{SegOptim} uses genetic algorithm optimization for "real-valued" type, i.e., 
#' optimization problems where the decision variables (i.e., segmentation parameters) are floating-point 
#' representations of real numbers.     
#'         
#'         
#' -- INPUT DATA PREPARATION --
#'      
#' TODO: ...
#'        
#' -- COMPUTING TIME AND COMPLEXITY --
#'        
#' Depending on the size of the raster dataset and the amount of segmentation features/layers used, take into 
#' consideration that running this function may take quite some time!! Therefore it is crucial to use only a 
#' relevant subset (or subsets) of your data to run this procedure. Also, choosing an appropriate parameterization 
#' of the genetic algorithm is key to decrease computing time. For example, if \code{popSize} is set to 30 
#' and \code{maxiter} to 100, then a maximum number of 3000 image segmentation runs would be required 
#' to stop the optimization! (usually, running the segmentation is the most time-consuming task of the optimization 
#' procedure). 
#' However, if \code{run} is set to 20, this means that if that number of iterations records no 
#' improvement in fitness (i.e., the classification score) then the optimization stops and returns the best set 
#' of parameters. Bottom-line is that setting this parameters appropriately is fundamental to get good results 
#' in a admissible ammount of time. 
#' On another hand, classification algorithms are also working in the background of the fitness function. Using the 
#' previous example and admitting that we set \code{evalMethod} to "HOCV" and \code{nRounds} to 20 this means that 
#' classification would run a maximum of \eqn{3000 \times 20 = 60000} times!! So keep this in mind when setting \code{nRounds} 
#' value or select a more conservative cross-validation method such as 10- or 5-fold CV. The time required for training 
#' the classifier also depends on the input data thus a segmentation solution with larger number of objects will 
#' take longer. The number of classification features (or variables) also affects computation time, higher number of 
#' these will make classification algorithms running slower.
#'           
#'  -- SETTING PARAMETERS FOR OPTIMIZATION --
#'       
#' TODO: Controlling for 'biased' classification performance due to class inbalance
#' 
#' TODO: Setting appropriate ranges for segmentation algorithms 
#'  
#' TODO: Setting appropriate genetic algorithm parametrization  
#'  
#'  
#' @seealso 
#' \itemize{
#'   \item \strong{Genetic algorithms}: \code{\link[GA]{ga}}     
#'   \item \strong{Control parameters in GA}: \code{\link[GA]{gaControl}}     
#'   \item \strong{Fitness function}: \code{\link{fitFuncGeneric}} (this function controls much of the processes 
#'   behind the optimization)
#' }
#' 
#' @note 
#' Do not use parallel option when performing image segmentation with OTB LSMS algorithm! Since the software uses a 
#' parallel implementation, this will probably freeze the system by consuming all CPU resources.
#' 
#' @references 
#' Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 
#' 53(4), 1-37, \url{http://www.jstatsoft.org/v53/i04/}.       
#'           
#' Scrucca L. (2016). On some extensions to GA package: hybrid optimisation, parallelisation and 
#' islands evolution. Submitted to R Journal. Pre-print available at: \url{http://arxiv.org/abs/1605.01931}.
#' 
#' @import GA
#' @importFrom terra rast
#' @importFrom terra compareGeom
#' @importFrom terra values
#' 
#' @export

gaOptimizeSegmentationParams<-function(rstFeatures,
                                           trainData,
                                           segmentMethod,
                                           trainThresh = 0.5,
                                           segmStatsFuns = c("mean","sd"),
                                           bylayer = FALSE,
                                           tiles = NULL,
                                           classificationMethod = "RF",
                                           classificationMethodParams = NULL,
                                           balanceTrainData = TRUE,
                                           balanceMethod = "ubUnder",
                                           evalMethod = "5FCV",
                                           trainPerc = 0.8,
                                           nRounds = 10,
                                           evalMetric = "Kappa",
                                           minTrainCases = 30,
                                           minCasesByClassTrain = 10,
                                           minCasesByClassTest = 10,
                                           minImgSegm = 30,
                                           ndigits = 2,
                                           verbose = TRUE,
                                           ...,
                                           lower,
                                           upper,
                                           population = GA::gaControl("real-valued")$population,
                                           selection  = GA::gaControl("real-valued")$selection,
                                           crossover  = GA::gaControl("real-valued")$crossover, 
                                           mutation   = GA::gaControl("real-valued")$mutation,
                                           popSize    = 20, 
                                           pcrossover = 0.8, 
                                           pmutation  = 0.1,
                                           elitism    = base::max(1,round(popSize*0.05)),
                                           maxiter    = 100, 
                                           run        = 20,
                                           maxFitness = 1,
                                           keepBest   = TRUE,
                                           parallel   = FALSE,
                                           seed       = NULL){
  
  
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
  
  if(is.character(evalMetric)){
    if(!(doInputVerification(evalMetric, "evalMetr_sc") || doInputVerification(evalMetric, "evalMetr_mc")))
      stop("Evaluation metric in evalMetric is not valid! Please check input parameters")
  }else if(is.function(evalMetric)){
    if(!checkEvalFun(evalMetric))
      stop("Invalid function defined in evalMetric!")
  }else{
    stop("Invalid object type in evalMetric")
  }

  if(inherits(trainData, "SpatialPointsDataFrame")){
    if(!("train" %in% colnames(trainData@data)))
      stop("To use SpatialPointsDataFrame as input data this object must contain a column named \"train\" 
           with train class labels (as integers)!")
  }
  
  if(verbose) cat("All OK!\n\n")
  
  ## Check and get metadata ---------------------------------------------------------
  
  if(is.character(rstFeatures)){
    if(file.exists(rstFeatures)){
      
      if(verbose) cat("-> Loading raster data containing classification features... \n")
      rstFeatures <- try(terra::rast(rstFeatures))
      if(verbose) cat("done.\n\n")
      
      if(inherits(rstFeatures,"try-error")){
        stop("An error occurred while reading raster metadata from rstFeatures!")
      }
    }
  }
  
  if(is.character(trainData)){
    if(file.exists(trainData)){
      
      if(verbose) cat("-> Loading raster file metadata containing train data... \n")
      trainData <- try(terra::rast(trainData))
      if(verbose) cat("done.\n\n")
     
     if(inherits(trainData,"try-error")){
       stop("An error occurred while reading raster metadata from trainData!")
     }
    }
  }
  
  ## Check raster similarity -------------------------------------------------------
  
  if(inherits(trainData,"SpatRaster")){
    
    if(!terra::compareGeom(rstFeatures, trainData, stopOnError=FALSE, messages=TRUE)){
      stop("Differences found between rasters in rstFeatures and trainData! Check raster extent, pixel size, 
            nr. of rows/cols, or coordinate reference system")
    }
  }
  
  
  ## Load raster data from file ----------------------------------------------------
  

  ## Run GA optimization ----------------------------------------------------------- 
  ##
  ##
  out.ga<-try(GA::ga(type      = "real-valued",   # static definition - (almost) all optimized parameters are real valued (minimum segment sizes are actually integers)
                 fitness       = fitFuncGeneric,  # generic fitness function 
                 rstFeatures   = rstFeatures,     # SpatRaster with classification features
                 trainData     = trainData,       # SpatRaster with calibration - train/test data   
                 segmentMethod = segmentMethod,   # segmentation method used
                 
                 # start specific parameters passed to the fitness function:
                 # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
                 
                 trainThresh                = trainThresh,
                 segmStatsFuns              = segmStatsFuns,
                 bylayer                    = bylayer,
                 tiles                      = tiles,
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
                 ndigits                    = ndigits,
                 verbose                    = verbose,
                 
                 # ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
                 # end parameters for the fitness function
                 
                 ...,                       # remaining parameters passed to the segmentation function
                 lower         = lower,
                 upper         = upper,
                 population    = population,
                 selection     = selection,
                 crossover     = crossover, 
                 mutation      = mutation,
                 popSize       = popSize, 
                 pcrossover    = pcrossover, 
                 pmutation     = pmutation,
                 elitism       = elitism,
                 maxiter       = maxiter, 
                 run           = run,
                 keepBest      = keepBest,
                 maxFitness    = maxFitness,
                 names         = segmentationParamNames()[[segmentMethod]],
                 parallel      = parallel,
                 seed          = seed))
  
  
  if(inherits(out.ga,"try-error")){
    stop("An error occurred while running ga function!")
  }
  else{
    
    # TODO: Run final solution with full classifier??
    
    return(out.ga)
  }
}

