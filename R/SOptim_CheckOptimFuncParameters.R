
## ---------------------------------------------------------------------------------------------- ##
## Admissable parameters ------------------------------------------------------------------------ ##
## ---------------------------------------------------------------------------------------------- ##


#' Get classification algorithm codes
#' 
#' Outputs the codes/acronyms of classification algorithms.
#' 
#' @return A vector with codes/acronyms of classification algorithms.

classificationAlgorithms<-function(){
  c("RF",
    "KNN",
    "GBM",
    "FDA",
    "SVM")
}

#' Get segmentation algorithm codes
#' 
#' Outputs the codes/acronyms of segmentation algorithms.
#' 
#' @return A vector with codes/acronyms of segmentation algorithms.

segmentationMethods<-function(){
  c("SAGA_SRG",
    "ArcGIS_MShift",
    "GRASS_RG",
    "Terralib_Baatz",
    "Terralib_MRGrow",
    "RSGISLib_Shep",
    "OTB_LSMS",
    "OTB_LSMS2")
}

#' Get evaluation metrics (single-class)
#' 
#' Outputs the codes/acronyms of evaluation metrics (single-class).
#' 
#' @return A vector with codes/acronyms of evaluation metrics (single-class).

evaluationMetrics_singleClass<-function(){
  c("Kappa",
    "PSS",
    "GSS",
    "AUC")
}

#' Get evaluation metrics (multi-class)
#' 
#' Outputs the codes/acronyms of evaluation metrics (multi-class).
#' 
#' @return A vector with codes/acronyms of evaluation metrics (multi-class).

evaluationMetrics_multiClass<-function(){
  c("Kappa",
    "PSS",
    "GSS",
    "Accuracy")
}

#' Classification algorithm codes
#' 
#' Outputs the codes/acronyms of classification algorithms.
#' 
#' @return A vector with codes/acronyms of classification algorithms.

dataBalancingMethods<-function(){
  c("ubOver",
    "ubUnder")
}

#' Get data balancing options
#' 
#' Outputs the codes/acronyms of data balancing options.
#' 
#' @return A vector with codes/acronyms of data balancing options.

evaluationMethods<-function(){
  c("OOB",
    "5FCV",
    "10FCV",
    "HOCV")
}

#' Get the evaluation methods names
#' 
#' Outputs the codes/acronyms of evaluation methods.
#' 
#' @return A vector with codes/acronyms of evaluation methods.

classifierParameters<-function(){
  unlist(lapply(generateDefaultClassifierParams(data.frame(x=NA,y=NA)), names), use.names = FALSE)
}


#' Checks if input parameters are valid
#' 
#' Validate options and ranges in user-defined inputs. This is meant to validate the parameters of 
#' \code{\link{gaOptimizeSegmentationParams}} general optimization function.
#' 
#' @param input The input value of the parameter to check.
#' 
#' @param what A string defining which parameter to check. Available options are:         
#' \itemize{
#'     \item "clAlgo" - classification algorithm;
#'     \item "classifierParams" - classification parameters;
#'     \item "segmMethod" - segmentation method;
#'     \item "evalMethod" - classifier evaluation methods;
#'     \item "evalMetr_sc" - evaluation metrics (single-class);
#'     \item "evalMetr_mc" - evaluation metric (multi-class);
#'     \item "dataBalancing" - data balancing methods;
#'     \item "trainThresh" - minimum proportion cover of a segment that decides the train class;
#'     \item "trainPerc" - proportion of data used for classifier training (only applies to HOCV);
#'     \item "segmStatsFuns" - checks if the functions used for generating segment statistics actually exist; 
#' }
#' 
#' @return A boolean. TRUE if OK, FALSE if not

doInputVerification<-function(input, what){
  
  if(what=="clAlgo"){
    if(input %in% classificationAlgorithms()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="segmMethod"){
    if(input %in% segmentationMethods()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="evalMetr_sc"){
    if(input %in% evaluationMetrics_singleClass()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="evalMetr_mc"){
    if(input %in% evaluationMetrics_multiClass()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="dataBalancing"){
    if(input %in% dataBalancingMethods()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="evalMethod"){
    if(input %in% evaluationMethods()){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="classifierParams"){
    if(all(unlist(lapply(input, names)) %in% classifierParameters())){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what=="segmStatsFuns"){
    if(all(sapply(input, FUN = function(x) is.function(get0(x))))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(what == "trainThresh" || what == "trainPerc"){
    if(input>0 && input<1){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
}


#' Check a custom performance evaluation function
#' 
#' Checks if a given custom evaluation function, using two vectors (respectively, for observed and 
#' predicted values), is able to produce a valid result. See details for more info.
#'
#' @param funToTest A function to test
#' @param nClassType A string defining if the classification is \code{"single-class"} or \code{"multi-class"}
#' @param verbose Print messages?
#' 
#' @details
#' To be considered valid the input function for single-class must have:
#' 
#' \itemize{
#'    \item Have at least two inputs arguments;
#'    \item Produce a non-null and valid numerical result;
#'    \item A scalar output;
#'    \item An attribute named \code{'thresh'} defining the numerical threshold to 
#'    binarize  the classifier predictions (i.e., to convert from continuous probability 
#'    to discrete {0,1}).
#' }
#' 
#' Valid multi-class functions' must have:
#' 
#' \itemize{
#'    \item Have at least two inputs arguments;
#'    \item Produce a non-null and valid numerical result;
#'    \item A scalar output.
#' }
#'
#' @return Logical. If \code{TRUE} the function is considered valid.
#'
#' 
#' @examples
#' 
#' accMultiClass <- function(obs, pred){
#' d <- sum(diag(as.matrix(table(obs, pred))))
#' s <- length(obs)
#' return(d/s)
#' }
#'
#' accMultiClass(1:10, 1:10)
#' 
#' checkEvalFun(funToTest=accMultiClass, nClassType="multi-class", verbose=TRUE)
#' 
#' @export
#' 

checkEvalFun <- function(funToTest, nClassType="undefined", verbose=TRUE){
  
  if(length(formals(funToTest)) < 2){
    message("The user-defined evaluation function must have at least two arguments!")
    return(FALSE)
  }
  
  # Test single-class function
  #
  if(nClassType == "single-class"){
    
    obs_sc <- sample(0:1, 1000, replace = TRUE)
    pred_sc <- runif(1000, 0, 1)
    res_sc <- try(funToTest(obs_sc, pred_sc), silent = TRUE)
    
    if(!inherits(res_sc,"try-error") && is.numeric(res_sc) && 
       length(res_sc)==1 && !is.null(attr(res_sc,"thresh"))){
      return(TRUE)
    }else{
      if(verbose) message("The user-defined evaluation function did not provide a valid result!")
      return(FALSE)
    }
    
    # Test multi-class function
    #
  }else if(nClassType == "multi-class"){
    
    obs_mc <- sample(1:10, 1000, replace = TRUE)
    pred_mc <- sample(1:10, 1000, replace = TRUE)
    res_mc <- try(funToTest(obs_mc, pred_mc), silent = TRUE)
    
    if(!inherits(res_mc,"try-error") && is.numeric(res_mc) && length(res_mc)==1){
      return(TRUE)
    }else{
      if(verbose) message("The user-defined evaluation function did not provide a valid result!")
      return(FALSE)
    }
    
    # A "softer" test to run outside the classifier calibration function
    #
  }else if(nClassType == "undefined"){
    
    test_sc <- checkEvalFun(funToTest, "single-class", verbose=FALSE)
    test_mc <- checkEvalFun(funToTest, "multi-class", verbose=FALSE)
    
    if(test_sc | test_mc){
      return(TRUE)
    }else{
      return(FALSE)
    }
    
  }else{
    stop("Invalid option in checkEvalFun!")
  }
}

