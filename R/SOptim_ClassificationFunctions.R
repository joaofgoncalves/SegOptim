

## --------------------------------------------------------------------- ##
##
## Main functions used for performing object-based image classification
##
## --------------------------------------------------------------------- ##



#' Generates default parameters for classification algorithms
#' 
#' This is an auxiliary function used for generating a list of default parameters used for the available 
#' classification algorithms: Random Forests (RF), K-nearest neighbour (KNN), Flexible Discriminant 
#' Analysis (FDA), Support Vector Machines (SVM) and Generalized Boosted Model (GBM).
#' 
#' @param x The dataset used for classification (this is required to calculate some classifier hyperparameters based on the number of 
#' columns/variables in the data).
#' 
#' @return A nested list object of class \code{classificationParamList} with parameters for the available 
#' algorithms, namely:
#'          
#'          \itemize{
#'            
#'            \item RF - Random Forest parameters:
#'              \itemize{
#'                \item \emph{mtry} is equal to \code{floor(sqrt(ncol(x)-2))} and defines the number of variables randomly sampled 
#'                as candidates at each split
#'                \item \emph{ntree} equals 250 (by default) and is the number of trees to grow
#'              }
#'          
#'            \item KNN - K-nearest neighbour parameters:
#'              \itemize{
#'                \item \emph{k} is equal to 5 and is the number of neighbours considered
#'              }
#'              
#'            \item FDA - Flexible Discriminant Analysis with MDA-MARS parameters:
#'              \itemize{
#'                \item \emph{degree} equals 1 defining an optional integer specifying maximum interaction degree 
#'                \item \emph{penalty} is equal to 2 and sets an optional value specifying the cost per degree of freedom charge
#'                \item \emph{prune} is set to TRUE and defines an optional logical value specifying whether the model should be 
#'                pruned in a backward stepwise fashion
#'              }
#'              
#'            \item SVM - Support Vector Machine (with radial-basis kernel) parameters:
#'              \itemize{
#'                \item \emph{gamma} equals \code{1/(ncol(x)-2)} and sets the parameter needed for all kernels except linear
#'                \item \emph{cost} equal to 1 defines the cost of constraints violation - it is the 'C'-constant of the regularization 
#'                term in the Lagrange formulation
#'                \item \emph{probability} is equal to TRUE and defines the output type
#'              }
#'          
#'            \item GBM - Generalized Boosted Modeling parameters:
#'              \itemize{
#'                \item \emph{n.trees} set to 250 defining the total number of trees to fit
#'                \item \emph{interaction.depth} equal to 1 which defines he maximum depth of variable interactions. 1 implies an 
#'                additive model, 2 implies a model with up to 2-way interactions, etc  
#'                \item \emph{shrinkage} set to 0.01 is a shrinkage parameter applied to each tree in the expansion. Also known as the 
#'                learning rate or step-size reduction.
#'                \item \emph{bag.fraction} set to 0.5 and equals the fraction of the training set observations randomly selected to 
#'                propose the next tree in the expansion)
#'                \item \emph{distribution} set to bernoulli (if single-class) or multinomial (if multi-class) this parameter defines 
#'                the applicable distribution used for classification
#'              }
#'              
#'          }
#'          
#' @examples 
#' 
#' DF <- data.frame(SID=1:5, train=sample(0:1,5,replace=TRUE), Var_1=rnorm(5), Var_2=rnorm(5))
#' generateDefaultClassifierParams(DF)
#' 
#' 
#' @seealso \code{\link{replaceDefaultClassificationParams}}
#' @export


generateDefaultClassifierParams<-function(x){
  
  paramList<-
  
  list(
    
    ## Random Forest ---------------------------------------------- ##
    ##
    `RF`=list(
      mtry=floor(sqrt(ncol(x)-2)), # Number of variables randomly sampled as candidates at each split
      ntree=250 # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
      ),
    
    ## K-nearest neighbour ---------------------------------------- ##
    ##
    `KNN`=list(
      k = floor(sqrt(nrow(x))) # number of neighbours considered
      ),
    
    ## Flexible Discriminant Analysis with MDA-MARS --------------- ##
    ##
    `FDA`=list(
      degree=1,  # an optional integer specifying maximum interaction degree (default is 1).
      penalty=2, # an optional value specifying the cost per degree of freedom charge (default is 2).
      prune=TRUE # an optional logical value specifying whether the model should be pruned in a backward stepwise fashion (default is TRUE)
      
    ),
    
    ## Support Vector Machine with Radial-basis kernel function --- ##
    ##
    `SVM`=list(
      gamma=1/(ncol(x)-2), # parameter needed for all kernels except linear (default: 1/(data dimension))
      cost=1,               # cost of constraints violation (default: 1) - it is the 'C'-constant of the regularization term in the Lagrange formulation
      probability=TRUE
    ),
    
    ## Generalized Boosted Regression Modeling -------------------- ##
    ##
    `GBM`=list(
      n.trees=250,               # the total number of trees to fit
      interaction.depth = 1,     # The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions, etc  
      shrinkage = 0.01,          # a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.
      bag.fraction = 0.5,        # the fraction of the training set observations randomly selected to propose the next tree in the expansion)
      distribution = "bernoulli") # distribution used for classification
  )
  
  class(paramList) <- append(class(paramList),"classificationParamList")
  return(paramList)
  
}

#' Replace classification parameters
#' 
#' An auxiliary function to replace default parameters of classification algorithms.
#' 
#' @param oldParamSet A parameter list object of class \code{classificationParamList}.  
#' @param newParamSet A nested list with names equal to the parameters defined in \code{\link{generateDefaultClassifierParams}}.
#' 
#' @return A nested list object of class \code{classificationParamList} with parameters for the available algorithms.  
#' 
#' @seealso \code{\link{generateDefaultClassifierParams}} to get the list of parameters that can be changed for each 
#' classification algorithm.
#' 
#' @examples
#' 
#' # x is the classification dataset with one column with the segment ID, 
#' # other for train/test data inputs, and the remaining columns with classification features
#' 
#' x<-data.frame(SID=1:10, train=sample(c(0,1),10, replace=TRUE), feature1=rnorm(10), 
#' feature2=rnorm(10,5,2), feature3=rnorm(10,12,3))
#' 
#' oldParams<-generateDefaultClassifierParams(x)
#' 
#' newParams<-list(RF=list(mtry=3),
#'                GBM=list(n.trees=3000))
#' 
#' replaceDefaultClassificationParams(oldParams, newParams)
#' 
#' @export

replaceDefaultClassificationParams <- function(oldParamSet, newParamSet){
  
  if(!inherits(oldParamSet,"classificationParamList"))
    stop("The object in oldParamSet must be of class classificationParamList")
  
  for(clMethodName in names(newParamSet)){
    for(parName in names(newParamSet[[clMethodName]])){
      oldParamSet[[clMethodName]][[parName]] <- newParamSet[[clMethodName]][[parName]]
    }
  }
  return(oldParamSet)
}

  

#' Generate formula
#' 
#' Generates a formula object to use in classification algorithms.
#' 
#' @param x A calibration dataset with the first two columns containing the segment ID (SID) and train/test 
#' inputs (named as train). The remaining columns will be used as classification features.
#' 
#' @note By default removes the first 2 columns corresponding to the segment identifier (SID) and and the train columns.
#' 
#' @return A \code{formula} object with a similar structure to: \code{train ~ feature_1 + ... + feature_n}.   
#' 
#' @importFrom stats as.formula         
#' 
#' @examples 
#' 
#' DF <- data.frame(SID=1:10, train = 1:10, x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10))
#' generateFormula(DF)
#' 
#' @export

generateFormula <- function(x){
  
  # Get column names and remove SID and train cols
  cn <- colnames(x)[-c(1:2)] 
  
  # Assemble formula
  form <- stats::as.formula(paste("train ~",paste(cn,collapse="+")))
  return(form)
}



#' Create a data split for classification evaluation
#' 
#' An auxiliary/internal function used to create a data split intended to evaluate classification performance. 
#' Used internally in the \code{\link{calibrateClassifier}} function.
#' 
#' @param y A vector containing train inputs.
#' 
#' @param evalMethod A character string defining the evaluation method. The available methods are \code{"10FCV"} (10-fold 
#' cross-validation; the default), \code{"5FCV"}, (5-fold cross-validation), \code{"HOCV"} (holdout cross-validation with 
#' the training percentage defined by \code{trainPerc} and the number of rounds defined in \code{nRounds}), and, \code{"OOB"} 
#' (out-of-bag evaluation; only applicable to random forests algorithm).
#' 
#' @param nRounds Number of evaluation rounds. This is only used for HOCV method.
#' 
#' @param trainPerc Proportion of data used for training the algorithm (by deafult eaul to 0.8). This is 
#' only used for the \code{"HOCV"} method.
#' 
#' @details Uses function \link[caret]{createDataPartition} from \pkg{caret} package as the 'workhorse' to generate 
#' data splits.
#' 
#' @return A list like object of class 'dataSplit' to be used in calibrateClassifier() function. Each element on the list 
#' contains the indices used for training. 
#' 
#' @examples 
#' createDataSplits(y=rnorm(100), evalMethod = "10FCV", nRounds = 20, trainPerc = 0.8)
#' 
#' @export
#' @importFrom caret createFolds
#' @importFrom caret createDataPartition
#' 

createDataSplits <- function(y, evalMethod = "10FCV", nRounds = 20, trainPerc = 0.8){
  
  # Just a workaround?!... - OOB does not creates data splits externally
  if(evalMethod == "OOB"){
      dp<-list(resample01=1:length(y))
  }
  
  else if(evalMethod == "5FCV"){
    dp<-caret::createFolds(y, k = 5, list = TRUE, returnTrain = TRUE)
  }
  
  else if(evalMethod == "10FCV"){
    dp<-caret::createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)
  }
  
  else if(evalMethod =="HOCV"){
    dp<-caret::createDataPartition(y, times = nRounds,p = trainPerc,list = TRUE)
  }
  
  else{
    stop("Invalid method set in evaluation method for creating data splits! Please review input parameters")
  }

  # Return output
  attr(dp,"evalMethod")<-evalMethod
  class(dp)<-append(class(dp),"dataSplit")
  return(dp)
  
}

#' Predict k-nearest neighbour output
#' 
#' An auxiliary function used to predict the probabilistic outcome from the knn classifier for 
#' single or multi-class problems.
#' 
#' @param object An object generated from \link[class]{knn} function.
#' 
#' @param type Type of response generated, if \code{"prob"} the probabilistic output for all classes 
#' will be returned. If \code{"response"} then the class labels are returned.
#' 
#' @param newdata A dataset used to predict the class labels.
#' 
#' @param traindata Train data used to calibrate the \link[class]{knn} classifier.
#' 
#' @param cl Factor of true classifications of training set.
#' 
#' @param k Number of neighbours considered.
#' 
#' @param ... Additional arguments passed to \link[class]{knn}.
#' 
#' @inheritParams calibrateClassifier
#' 
#' @return A matrix object with knn output.
#' 
#' @method predict knn
#' 
#' @importFrom class knn
#' 
#' @export

predict.knn <- function(object, type="response", newdata=NULL, traindata = NULL, cl=NULL, k=NULL, 
                      balanceTrainData=FALSE, balanceMethod="ubOver", ...){
  
  if(!is.null(newdata) && !is.null(traindata) && !is.null(k) && !is.null(cl)){
    
    if(balanceTrainData){
      traindata <- cbind(cl,traindata)
      colnames(traindata)[1] <- "train"
      traindata <- dataBalancing(x = traindata, method = balanceMethod)
      object <- class::knn(train = traindata[,-1], test = newdata, cl = as.factor(traindata[,"train"]), k = k, prob = TRUE, ...)
    }else{
      object <- class::knn(train = traindata, test = newdata, cl = cl, k = k, prob = TRUE, ...)
    }

  }
  
  if(type=="prob"){
    lv<-levels(object)
    prob<-attr(object,"prob")
    pred<-as.character(object)
    out<-matrix(0,length(object),length(lv),dimnames = list(1:length(object),lv))
    
    for(i in 1:length(prob)){
      posCol<-(1:length(lv))[lv==pred[i]]
      out[i,posCol]<-prob[i]
      out[i,-posCol]<-(1-prob[i])/(length(lv)-1) ## Assumes equal probability for the remaining classes?!
    }
    return(out)

  }else if(type=="response"){
    
    return(object)
  }
}


#' Train and evaluate a classification algorithm
#' 
#' Main function used for classifier training and evaluation for both single and multi-class problems.     
#' 
#' 
#' @param calData An input calibration dataset used for classification. It can be either an object of class \code{matrix}, 
#' \code{data.frame} or \code{SOptim.CalData} generated by \code{prepareCalData} (to use with option \code{runFullCalibration=TRUE}; 
#' see below for more details). If the input is a \code{matrix} or \code{data.frame} then it must contain one column named \code{SID} with 
#' segment IDs, one column named \code{"train"} defining the labels, classes or categories (either {0,1} for single-class problems, 
#' or, {1, 2, ..., n} for multi-class problems) followed by n columns containing features (or variables) used for training the classifier.
#'   
#' @param classificationMethod An input string defining the classification algorithm to be used. Available options are: 
#' \code{"RF"} (random forests), \code{"GBM"} (generalized boosted models), \code{"SVM"} (support vector machines), \code{"KNN"} 
#' (k-nearest neighbour), and, \code{"FDA"} (flexible discriminant analysis).     
#' 
#' @param classificationMethodParams A list object with a customized set of parameters to be used for the classification algorithms 
#' (default = NULL). See also \link{generateDefaultClassifierParams} to see which parameters can be changed and how to structure the 
#' list object.
#' 
#' @param balanceTrainData Defines if data balancing is to be used (only available for single-class problems; default: TRUE). 
#'   
#' @param balanceMethod A character string used to set the data balancing method. Available methods are based on under-sampling 
#' \code{"ubUnder"} or over-sampling \code{"ubOver"} the target class. 
#'  
#' @param evalMethod A character string defining the evaluation method. The available methods are \code{"10FCV"} (10-fold 
#' cross-validation; the default), \code{"5FCV"} (5-fold cross-validation), \code{"HOCV"} (holdout cross-validation with 
#' the training percentage defined by \code{trainPerc} and the number of rounds defined in \code{nRounds}), and, \code{"OOB"} 
#' (out-of-bag evaluation; only applicable to random forests).
#' 
#' @param evalMetric A character string setting the evaluation metric or a function that calculates the performance score 
#' based on two vectors one for observed and the other for predicted values (see below for more details). 
#' This option defines the outcome value of the genetic algorithm fitness function and the output of grid or random search 
#' optimization routines. Check \code{\link{evalPerformanceGeneric}} for available options. When \code{runFullCalibration=TRUE} 
#' this metric will be calculated however other evaluation metrics can be quantified using \link{evalPerformanceClassifier}. 
#' 
#' @param trainPerc A decimal number defining the training proportion (default: 0.8; if \code{"HOCV"} is used).
#' 
#' @param nRounds Number of training rounds used for holdout cross-validation (default: 20; if \code{"HOCV"} is used).
#' 
#' @param minTrainCases The minimum number of training cases used for calibration (default: 20). If the number of rows 
#' in \code{x} is below this number then \code{calibrateClassifier} will not run.
#' 
#' @param minCasesByClassTrain Minimum number of cases by class for each train data split so that the classifier 
#' is able to run.
#' 
#' @param minCasesByClassTest Minimum number of cases by class for each test data split so that the classifier 
#' is able to run.  
#'  
#' @param runFullCalibration Run full calibration? Check \strong{details} section (default: FALSE).  
#' 
#' @param verbose Print progress messages? (default: TRUE)
#' 
#' @details 
#' 
#' Two working modes can be used:   
#' \enumerate{
#'    \item i) for "internal" GA optimization or grid/random search: \code{runFullCalibration = FALSE}, or,     
#'    \item ii) for performing a full segmented image classification: \code{runFullCalibration = TRUE}.
#' }
#' Tipically, the first option is used internally for optimizing segmentation parameters in 
#' \code{\link{gaOptimizeSegmentationParams}} where the output value from the selected evaluation metric 
#' is passed as the fitnes function outcome for \pkg{GA} optimization.        
#' The second option, should be used to perform a final image classification and to get full evaluation 
#' statistics (slot: 'PerfStats'), confusion matrices (slot: 'ConfMat'), train/test partion sets (slot: 'TrainSets'), 
#' classifier objects (slot: 'ClassObj') and parameters (slot: 'ClassParams'). In addition to the evaluation rounds 
#' (depending on the evaluation method selected) this option will also run a "full" round where all the data (i.e., 
#' no train/test split) will be used for training. Results from this option can then be used in \code{\link{predictSegments}}.  
#' This function can also perform data balancing for single-class problems (check out option \code{balanceTrainData} and 
#' \code{balanceMethod}). 
#' 
#' Check \link[unbalanced]{ubBalance} function for further details regarding data balancing.    
#' 
#' For more details about the classification algorithms check out the following functions: 
#' \itemize{
#'   \item \link[randomForest]{randomForest} for random forest algorithm, 
#'   \item \link[gbm]{gbm} for generalized boosted modelling, 
#'   \item \link[e1071]{svm} for details related to support vector machines, 
#'   \item \link[class]{knn} for k-nearest neighbour classification, and, 
#'   \item \link[mda]{fda} for flexible discriminant analysis. 
#' }
#' 
#' @section Defining custom performance evaluation functions:
#' 
#' In argument \code{evalMetric} it is possible to define a custom function. This must take two vectors: one containing 
#' observed/ground-truth values (first argument) and other with predicted values by the trained classifier (second argument) 
#' and both for the test set (from holdout or k-fold CV). If the classification task is single-class (e.g., 1:forest/0:non-forest, 
#' 1:water/0:non-water) then the predicted values will be probabilities (ranging in [0,1]) for the interest class (coded as 
#' 1's). If the task is multi-class, then the predicted values will be integer codes for each class.
#' 
#' To be considered valid, the evaluation function for single-class must have:
#' 
#' \itemize{
#'    \item Have at least two inputs arguments (observed and predicted);
#'    \item Produce a non-null and valid numerical result;
#'    \item A scalar output;
#'    \item An attribute named \code{'thresh'} defining the numerical threshold to 
#'    binarize  the classifier predictions (i.e., to convert from continuous probability 
#'    to discrete {0,1}). The calculation of this threshold is necessary to maximize the 
#'    value of the performance metric instead of using a naive 0.5 cutoff value.
#' }
#' 
#' Here goes an example function used to calculate the maximum value for the overall 
#' accuracy based on multiple threshold values:
#' 
#' \preformatted{
#' 
#' calcMaxAccuracy <- function(obs, pred){
#' 
#'    accuracies <- c()
#'    i <- 0
#'    N <- length(obs)
#'    thresholds <- seq(0, 1, 0.05)
#' 
#'    for(thresh in thresholds){
#'       i <- i + 1   
#'       pred_bin <- as.integer(pred > thresh)
#'       confusionMatrix <- as.matrix(table(obs, pred_bin))
#'       accuracies[i] <- diag(confusionMatrix) / N
#'    }
#' 
#'    bestAccuracy <- max(accuracies)
#'    attr(bestAccuracy, "thresh") <- thresholds[which.max(accuracies)]
#' 
#'    return(bestAccuracy)
#' }
#' 
#' x <- sample(0:1,100,replace=TRUE)
#' y <- runif(100)
#' calcMaxAccuracy(obs = x, pred = y)
#' 
#' } 
#' 
#' Valid multi-class functions' must have:
#' 
#' \itemize{
#'    \item Have at least two inputs arguments (observed and predicted);
#'    \item Produce a non-null and valid numerical result;
#'    \item A scalar output.
#' }
#' 
#' An example of a valid custom function to calculate the overall accuracy:
#' 
#' \preformatted{
#' 
#' calcAccuracy <- function(obs, pred){
#' 
#'    N <- length(obs)
#'    confusionMatrix <- as.matrix(table(obs, pred))
#'    acc <- diag(confusionMatrix) / N
#'    return(acc)
#' }
#' 
#' x <- sample(0:1,100,replace=TRUE)
#' y <- sample(0:1,100,replace=TRUE)
#' calcAccuracy(obs = x, pred = y)
#' 
#' } 
#'
#' @return 
#' 
#' If \code{runFullCalibration = FALSE} then a single average value (across evaluation replicates/folds) for the 
#' selected evaluation metric will be returned (typically used for GA optimization).       
#' If \code{runFullCalibration = TRUE} then an object of class \code{SOptim.Classifier} is returned with the 
#' following elements:      
#' \itemize{
#'    \item \bold{AvgPerf} - average value of the evaluation metric selected;
#'    \item \bold{PerfStats} - numeric vector with performance statistics (for the selected metric) for each 
#'    evaluation round plus one more round using the "full" train dataset;
#'    \item \bold{Thresh} - for single-class problems only; numeric vector with the threshold values (one for 
#'    each round plus the "full" dataset) that maximize the selected evaluation metric;
#'    \item \bold{ConfMat} - a list object with confusion matrices generated at each round; for single-class problems 
#'    this matrix is generated by dichotomizing the probability predictions (into {0,1}) using the threshold that 
#'    optimizes the selected evaluation metric (see 'Thresh' above);
#'    \item \bold{obsTestSet} - observed values for the test set (one integer vector for each evaluation round plus 
#'    the full evaluation round);
#'    \item \bold{predTestSet} - predicted values for the test set (one integer or numeric vector for each evaluation 
#'    round plus the full evaluation round);
#'    \item \bold{TrainSets} - a list object with row indices identifying train splits for each test round;
#'    \item \bold{ClassObj} - a list containing classifier objects for each round;
#'    \item \bold{ClassParams} - classification parameters used for running \code{\link{calibrateClassifier}}. 
#' }
#'
#' @note
#' 1) By default, if 25\% or more of the calibration/evaluation rounds must produce valid results otherwise the 
#' optimization algorithm will return \code{NA}.   
#' 
#' 2) Data balancing is only performed on the train dataset to avoid bias in performance evaluation derived from 
#' this procedure. 
#' 
#' @export 
#' 
#' @import class
#' @import gbm
#' @import unbalanced
#' @import randomForest
#' @import e1071
#' @import mda
#' @importFrom stats predict
#' 

calibrateClassifier <- function(calData,
                                classificationMethod = "RF",
                                classificationMethodParams = NULL,
                                balanceTrainData = FALSE,
                                balanceMethod = "ubOver",
                                evalMethod = "HOCV",
                                evalMetric = "Kappa",
                                trainPerc = 0.8,
                                nRounds = 20,
                                minTrainCases = 30,
                                minCasesByClassTrain = 10,
                                minCasesByClassTest = 10,
                                runFullCalibration = FALSE,
                                verbose = TRUE){
  
  
  # Check if the object in calData was generated by prepareCalData if runFullCalibration is TRUE
  if(runFullCalibration){
    if(!inherits(calData, "SOptim.CalData")){
      stop("When in runFullCalibration mode then calData must be an object of class SOptim.CalData generated by prepareCalData!")
    }
    else{
      nClassType <- attr(calData, "nClassType")
      calData <- calData[["calData"]] #Use only the first component of the object for calibration
    }
  }
  
  if(inherits(calData, "SOptim.CalData") && !runFullCalibration){
    stop("Seems that you have inputed a SOptim.CalData object! 
         In this case set option runFullCalibration = TRUE to generate a complete set of results")
  }
  
  if((!is.matrix(calData)) && (!is.data.frame(calData))){
    if(is.na(calData)){
      warning("Input train data in calData cannot be used for running the classification 
              (wrong object type or not enough image objects/segments?)")
      return(NA)
    }
  }

  if(nrow(calData) < minTrainCases){
    warning("The number of training cases is below the minimum value set in minTrainCases!")
    return(NA)
  } 
  
  ## Set nClassType ------------------------------------------------------------------------
  ##
  if(!inherits(calData, "SOptim.CalData")){
    if(is.null(attr(calData, "nClassType"))){
      nClassNum <- length(unique(calData[,"train"]))
      nClassType <- ifelse(nClassNum == 2, "single-class", "multi-class")
    }else{
      nClassType <- attr(calData, "nClassType")
    }
  }

  ## Validate input parameters --------------------------------------------------------------
  ##
  if(nClassType=="multi-class" & balanceTrainData){
    stop("Data balancing is only implemented for single-class! Set balanceTrainData to FALSE to proceed.")
  }
  
  # Validate if evalMetric is a valid function when performing a full calibration
  if(runFullCalibration & is.function(evalMetric)){
    if(!checkEvalFun(evalMetric, nClassType))
      stop("Invalid function defined in evalMetric!")
  }
  
  if(is.function(evalMetric)){
    evalMetricName <- "UserDefEvalMetric"
  }else{
    evalMetricName <- evalMetric
  }

  # Generate default parametrization
  p <- generateDefaultClassifierParams(calData)

  # Replace values in the default parametrization using user-defined values
  if(!is.null(classificationMethodParams)){
    p <- replaceDefaultClassificationParams(p, classificationMethodParams)
  }
    
  
  ## Create CV folds or data partitions depending on the method selected --------------------
  ##
  ##

  if(evalMethod=="OOB" && classificationMethod != "RF")
    stop("Evaluation method OOB is only available for Random Forest algorithm (RF)")
  
  dp <- createDataSplits(y=calData[,"train"], evalMethod, nRounds, trainPerc)
  

  ## Calibrate classifier --------------------------------------------------------------------
  ##
  ##
  
  # Convert input train column to factor (except for GBM..)
  if(classificationMethod != "GBM"){
    calData[,"train"] <- as.factor(calData[,"train"])
  }

  # If runFullCalibration is TRUE then start the list with all trained classifier objects among other outputs
  #
  if(runFullCalibration){
      
      # Number of evaluation rounds plus one full round 
      nRounds <- length(dp) + 1
      
      # Start the vector holding performance scores and other outputs
      perfStats <- vector(length = nRounds, mode = "numeric")
      names(perfStats) <- c(paste("TestSet_",1:(nRounds-1),sep=""),"FULL")
      
      threshVals <- vector(length = nRounds, mode = "numeric")
      names(threshVals) <- c(paste("TestSet_",1:(nRounds-1),sep=""),"FULL")
      
      # Test set containing observed and predicted vectors
      predTestSet <- list()
      obsTestSet <- list()
      
      # List to hold final classifier objects and confusion matrices
      classifierObj <- list()
      confusionMatrices <- list()
      
    }else{
      nRounds <- length(dp)
      
      # Start the vector holding performance scores
      perfStats <- vector(length = nRounds, mode = "numeric")
      names(perfStats) <- paste("TestSet_",1:(nRounds),sep="")
    }
  

  
  
  ## Iterate through data partitions/folds --------------------------------------------------
  ##

  for(i in 1:nRounds){
    
    if(verbose){
      cat("## ---",ifelse((runFullCalibration && (i==nRounds)), "FULL DATASET TRAINING ROUND",
          paste("TRAINING ROUND",i)),"--- ##\n\n")
    }
    
    # Generate temp datasets for train and test
    #
    if(runFullCalibration && i==nRounds){
      # ..TRAIN dataset
      trainDF <- calData
      # ..TEST dataset
      testDF <- calData
    }else{
      # ..TRAIN dataset
      trainDF <- calData[dp[[i]],]
      # ..TEST dataset
      if(evalMethod != "OOB"){
        testDF <- calData[-dp[[i]],]
      }
    }
    
    # Check number of classes in train and test --------------------------------------------
    #
    if(length(unique(trainDF$train)) < 2){
      warning("The number of classes in train data is below 2! Unable to perform classifier training. 
                Check trainThresh and evalMethod to change calibration train/test sets?")
      warning(".. Evaluation round #",i," (ERROR: The number of classes in train data is below 2!)","\n\n", sep="")
      
      perfStats[i]<-0 # Changed to reflect bad performance in this type of situations
      next
    }
    
    if(length(unique(testDF$train)) < 2){
      warning("The number of classes in test data is below 2! Unable to do performance evaluation. 
                Check trainThresh and evalMethod to change calibration train/test sets?")
      warning(".. Evaluation round #",i," (ERROR: The number of classes in test data is below 2!)","\n\n", sep="")
      
      perfStats[i]<-0
      next
    }
    
    # Verify the number of train cases by class ----------------------------------------------
    # If this is too low training or evaluation will fail...
    #
    tb_train <- table(trainDF$train)
    tb_test  <- table(testDF$train)
    
    if(verbose){
      cat(".. Frequency table by class for train data",ifelse(balanceTrainData," (before data balancing) ",""),":\n",sep="")
      print(tb_train)
      
      cat("\n.. Frequency table by class for test data:\n")
      print(tb_test)
      cat("\n")
    }
    
    if(any(as.vector(tb_train) < minCasesByClassTrain)){
      warning("The number of train cases by class is below minCasesByClassTrain! Unable to perform classifier training.
                Check trainThresh and evalMethod to change calibration train/test sets?")
      warning(".. Evaluation round #",i," (ERROR: The number of train cases by class is below minCasesByClassTrain!)","\n\n", sep="")
      
      if(!runFullCalibration)
        perfStats[i] <- 0 # Changed to reflect bad performance in this type of situations (only for optimization)
      else
        perfStats[i] <- NA
      
      next
    }
    
    if(any(as.vector(tb_test) < minCasesByClassTest)){
      warning("The number of train cases by class is below minCasesByClassTest! Unable to do performance evaluation. 
                Check trainThresh and evalMethod to change calibration train/test sets?")
      warning(".. Evaluation round #",i," (ERROR: The number of test cases by class is below minCasesByClassTest!)","\n\n", sep="")
      
      if(!runFullCalibration)
        perfStats[i] <- 0 # Changed to reflect bad performance in this type of situations (only for optimization)
      else
        perfStats[i] <- NA
      
      next
    }
    
    
    # Perform data balancing on train data only ----------------------------------------------
    # (except for the full calibration round!...)
    #
    if(balanceTrainData){
      
      trainDF <- try(dataBalancing(x=trainDF, method=balanceMethod))
      
      if(inherits(trainDF,"try-error")){
        warning("An error occurred while performing data balancing!")
        warning(".. Evaluation round #",i," (An error occurred while performing data balancing)","\n", sep="")
        perfStats[i]<-NA
        next
        
      }else{
        # When using balance data this must be equal for the full evaluation round
        if(runFullCalibration && (i==nRounds)){
          testDF <- trainDF
        }
      }
    }

    
    ## Random Forest classification ----------------------------------------------------------
    
    if(classificationMethod=="RF"){
      clObj<-try(randomForest::randomForest(y     = trainDF$train,
                                            x     = trainDF[,-c(1,2)],
                                            mtry  = p$RF$mtry,
                                            ntree = p$RF$ntree))

      ## Make predictions for RF
      if(!inherits(clObj,"try-error")){

        if(nClassType=="single-class"){

          if(evalMethod=="OOB" || (runFullCalibration && i==nRounds)){ # Gets OOB results for the full calibration round
            pred<-try(stats::predict(clObj, type="prob"))
          }
          else{ 
            pred<-try(stats::predict(clObj, newdata = testDF, type = "prob"))
          }
        }
        else if(nClassType=="multi-class"){
          
          if(evalMethod=="OOB" || (runFullCalibration && i==nRounds)){ # Gets OOB results for the full calibration round
            pred<-try(stats::predict(clObj,type="response"))
          }
          else{ 
            pred<-try(stats::predict(clObj, newdata = testDF, type = "response"))
          }
        } 
        else{
          stop("Invalid value in nClassType! Must be either single-class or multi-class")
        }
      }else{
        warning("An error occured while performing ",classificationMethod," classifier training!")
        return(NA)
      }
      
      if(inherits(pred,"try-error")){
        warning("An error occurred while predicting for test dataset with classification method: ",classificationMethod,"!")
        return(NA)
      }
      
    }
    
    
    ## K-Nearest Neighbour classification ----------------------------------------------------
    ##
    else if(classificationMethod=="KNN"){
      
      clObj<-try(class::knn(train      = trainDF[,-c(1:2)], 
                             test      = testDF[,-c(1:2)], 
                             cl        = trainDF$train, 
                             k         = p$KNN$k, 
                             prob      = TRUE))
      
      ## Make predictions for KNN
      if(!inherits(clObj,"try-error")){
  
        # Create object of class knn
        class(clObj)<-append(class(clObj),"knn")
        
        
        if(nClassType=="single-class"){
          pred<-try(stats::predict(clObj, type = "prob"))
          
        }
        else if(nClassType=="multi-class"){
          pred<-try(stats::predict(clObj, type = "response"))
        }
        else{
          stop("Invalid value in nClassType! Must be either single-class or multi-class")
        }
      }
      else{
        warning("An error occured while performing ",classificationMethod," classifier training!")
        return(NA)
      }
      
      if(inherits(pred,"try-error")){
        warning("An error occurred while predicting for test dataset with classification method: ",classificationMethod,"!")
        return(NA)
      }
      class(clObj) <- c(class(clObj),"knn")
    }
    
    
    ## Flexible Discriminant Analysis classification ------------------------------------------
    ##
    else if(classificationMethod=="FDA"){
     
      form<-generateFormula(calData)
      
      clObj<-try(mda::fda(formula       = form, 
                           data         = trainDF, 
                           method       = mars, 
                           keep.fitted  = FALSE,
                           degree       = p$FDA$degree,
                           penalty      = p$FDA$penalty,
                           prune        = p$FDA$prune))
      
      ## Make predictions for FDA
      if(!inherits(clObj,"try-error")){
        
        if(nClassType=="single-class"){
          pred<-try(stats::predict(clObj, newdata = testDF, type = "posterior")) # posterior type for probability
          
        }
        else if(nClassType=="multi-class"){
          pred<-try(stats::predict(clObj, newdata = testDF, type = "class"))
        } 
        else{
          stop("Invalid value in nClassType! Must be either single-class or multi-class")
        }
      }
      else{
        warning("An error occured while performing ",classificationMethod," classifier training!")
        return(NA)
      }
       
      if(inherits(pred,"try-error")){
        warning("An error occurred while predicting for test dataset with classification method: ",classificationMethod,"!")
        return(NA)
      }
      
    }
    
    ## Generalized Boosted Regression Modeling ------------------------------------------------
    ##
    else if(classificationMethod=="GBM"){
      
      # Set family to multinomial if it is a multi-class problem
      #
      if(nClassType=="multi-class"){
        p<-replaceDefaultClassificationParams(p,list(GBM=list(distribution="multinomial")))
      }
        
      form<-generateFormula(calData)
      
      clObj<-try(gbm::gbm(formula      = form, 
                     distribution      = p$GBM$distribution,
                     data              = trainDF, 
                     keep.data         = FALSE,
                     n.trees           = p$GBM$n.trees,
                     interaction.depth = p$GBM$interaction.depth,
                     shrinkage         = p$GBM$shrinkage,
                     bag.fraction      = p$GBM$bag.fraction))
      
      ## Make predictions for FDA
      if(!inherits(clObj,"try-error")){
        
        if(nClassType=="single-class"){
          pred<-try(stats::predict(clObj, newdata = testDF, type = "response", n.trees=p$GBM$n.trees)) 
        }
        else if(nClassType=="multi-class"){
          pred<-try(stats::predict(clObj, newdata = testDF, type = "response", n.trees=p$GBM$n.trees)[,,1]) # Gets array -> 1 matrix per n.trees value
          pred<-try(as.integer(colnames(pred))[apply(pred,1,which.max)])
        } 
        else{
          stop("Invalid value in nClassType! Must be either single-class or multi-class")
        }
      }
      else{
        warning("An error occured while performing ",classificationMethod," classifier training!")
        return(NA)
      }

      if(inherits(pred,"try-error")){
        warning("An error occurred while predicting for test dataset with classification method: ",classificationMethod,"!")
        return(NA)
      }
    }
      
    
    ## Support Vector Machines classification ------------------------------------------------
    ##
    else if(classificationMethod=="SVM"){
     
      form<-generateFormula(calData)
      
      
      if(nClassType=="multi-class"){
        p<-replaceDefaultClassificationParams(p,list(SVM=list(probability = FALSE)))
      }
      
      clObj<-try(e1071::svm(formula  = form, 
                     data            = trainDF, 
                     type            = "C-classification",
                     kernel          = "radial",
                     gamma           = p$SVM$gamma,
                     cost            = p$SVM$cost,
                     cachesize       = 512,
                     probability     = p$SVM$probability,
                     fitted          = FALSE))
      
      ## Make predictions for SVM
      if(!inherits(clObj,"try-error")){
        
        if(nClassType=="single-class"){
          # Prob matrix has column with names equal to factor levels, e.g., "0" / "1"
          #
          pred<-try(attr(stats::predict(clObj, newdata = testDF, probability = TRUE), "probabilities")) # Get prob matrix
        }
        else if(nClassType=="multi-class"){
          pred<-try(stats::predict(clObj, newdata = testDF))
        } 
        else{
          stop("Invalid value in nClassType! Must be either single-class or multi-class")
        }
      }
      else{
        warning("An error occured while performing ",classificationMethod," classifier training!")
        return(NA)
      }
      if(inherits(pred,"try-error")){
        warning("An error occurred while predicting for test dataset with classification method: ",classificationMethod,"!")
        return(NA)
      }
    }
    
    #
    # Bad classification method string
    #
    else{
      stop("Unknown classification method defined! Please review input parameters")
    }
    
    
    ## Perform evaluation -------------------------------------------------------------------
    ##
    ## OK!
    if(!inherits(pred,"try-error")){

      ## Observed data for the test set ----------- ##
      ##
      ##
      if(evalMethod=="OOB"){
        obs<-trainDF$train
      }else{
        obs<-testDF$train
      }

      ## Predicted data for the test set --------- ##
      ##
      
      if((nClassType=="single-class") && (is.matrix(pred) || is.data.frame(pred)) && ncol(pred) > 1){
        
        if(classificationMethod=="SVM"){
          # Altered this line to ensure that the right column is found for evaluation
          # when using SVM probability output
          pred <- pred[,"1"]
        }else{
          pred <- as.numeric(pred[,2]) # Use positives probability column only (single-class problems)
        }
        
      }
      
      # Convert factors into integers for making evaluations
      # Works both with single- or multi-class problems
      if(is.factor(obs)){
        # if(nClassType == "single-class"){
        #   obs.int <- as.integer(levels(obs)[as.integer(obs)])
        # }
        # else if(nClassType == "multi-class"){
        #   obs.int <- f2int(obs)
        # }
        
        # Converts factors into integers
        obs.int <- as.integer(levels(obs)[as.integer(obs)])
        
      }else{
        obs.int <- as.integer(obs)
      }
      
      # Convert predicted values to integer if is a factor (required for eval)
      if(is.factor(pred)){
        #pred <- as.integer(pred)
        pred <- as.integer(levels(pred)[as.integer(pred)])
      }
      
      # Remove NA's from predicted and observed values (FDA produces NaN's in some cases...) 
      nv_idx  <- !(is.na(pred) | is.nan(pred) | is.infinite(pred))
      pred    <- pred[nv_idx]
      obs.int <- obs.int[nv_idx]

      # Check if observed and predicted have the same size (FDA sometimes gives problems here!...)
      if((length(pred) != length(obs.int)) || length(pred) == 0){
        
        warning(".. Evaluation round #",i,
            " (ERROR: Different sizes between observed and predicted vectors during performance evaluation or 0-length prediction vector!)","\n\n", sep="")
        
        perfStats[i] <- NA
        next
      }

      # Evaluate performance using the selected evaluation metric
      #
      if(is.character(evalMetric)){
        # Internal metrics
        evalStatValue <- try(evalPerformanceGeneric(obs.int, pred ,stat=evalMetric, nClassType=nClassType))
      }else if(is.function(evalMetric)){
        # User-defined metric
        evalStatValue <- try(evalMetric(obs.int, pred))
      }else{
        stop("Invalid object type in evalMetric!")
      }
      
      
      if(inherits(evalStatValue,"try-error") || is.na(evalStatValue)){ # Invalid result
        
        # On error set NA as the result in performance statistics vector (or other results)
        if(runFullCalibration){
          
          # Save the object generated by the classifier
          classifierObj[[i]] <- NA
          
          # Save observed and predicted values
          obsTestSet[[i]] <- NA
          predTestSet[[i]] <- NA
          
          # Single-class problems with optimized thresholds
          if(nClassType=="single-class"){
            threshVals[i] <- NA
            confusionMatrices[[i]] <- NA
          }
          # Multi-class problems
          if(nClassType=="multi-class"){
            confusionMatrices[[i]] <- NA
          }
        }
        # If evaluation went wrong save results as NA
        perfStats[i] <- NA
      }
      # Save values if all went OK
      #
      else{
        
        if(runFullCalibration){
          
          # Save the object generated by the classifier
          classifierObj[[i]] <- clObj
          
          # Save observed and predicted values
          obsTestSet[[i]] <- obs.int
          predTestSet[[i]] <- pred
          
          # Single-class problems with optimized thresholds
          if(nClassType=="single-class"){
            thresh <- attr(evalStatValue,"thresh")
            threshVals[i] <- thresh
            confusionMatrices[[i]] <- generateConfusionMatrix(obs.int, as.integer(pred > thresh))
          }
          # Multi-class problems
          if(nClassType=="multi-class"){
            confusionMatrices[[i]] <- generateConfusionMatrix(obs.int, pred)
          }
        }
        
        # Valid performance results
        perfStats[i] <- evalStatValue 
        
        if(verbose){
          # Print results per round
          cat(".. Evaluation round #", i, " | N(train) = ",nrow(trainDF), " | N(test) = ",nrow(testDF)," | ",
              evalMetricName," = ",round(evalStatValue,3),"\n\n", sep="") 
        }
        
      }
    }
    # -- KO!
    else{
      perfStats[i] <- NA
    }
  } ## --- End loop
  

  # Check the % number of successful evaluation rounds, if less than 75% then an error occurs
  if((sum(is.na(perfStats))/length(dp)) >= 0.75){
    
    warning("Unable to evaluate classification performance! Less than 25% of the train/test rounds produced invalid values.
         Please review input parameters and calibration data")
    return(NA)
  }
  else{
    
    if(runFullCalibration){
      
      ## Average performance in cross-validation rounds excluding the full evaluation round
      avgPerformance <- mean(perfStats[-nRounds], na.rm=TRUE)
      if(verbose) cat(".. Average ",evalMetricName," = ",round(avgPerformance,3),"\n\n",sep="")
      
      # Define names for the objects contained in lists: classifierObj and confusionMatrices
      names(classifierObj) <- c(paste("TestSet_",1:(nRounds - 1),sep=""),"FULL")
      names(confusionMatrices) <- c(paste("TestSet_",1:(nRounds - 1),sep=""),"FULL")
      names(predTestSet) <- c(paste("TestSet_",1:(nRounds - 1),sep=""),"FULL")
      names(obsTestSet) <- c(paste("TestSet_",1:(nRounds - 1),sep=""),"FULL")
      
      # Build output object data and metadata
      #
      out <- list(AvgPerf     = avgPerformance, 
                  PerfStats   = perfStats, 
                  Thresh      = ifelse(nClassType == "single-class", threshVals, NA),
                  ConfMat     = confusionMatrices,
                  TrainSets   = dp,
                  ClassObj    = classifierObj,
                  obsTestSet  = obsTestSet,
                  predTestSet = predTestSet,
                  ClassParams = list(classificationMethod       = classificationMethod,
                                     classificationMethodParams = p,
                                     balanceTrainData           = balanceTrainData,
                                     balanceMethod              = balanceMethod,
                                     evalMethod                 = evalMethod,
                                     evalMetric                 = evalMetric,
                                     evalMetricName             = evalMetricName,
                                     trainPerc                  = trainPerc,
                                     nRounds                    = nRounds,
                                     nClassType                 = nClassType,
                                     minTrainCases              = minTrainCases,
                                     minCasesByClassTrain       = minCasesByClassTrain,
                                     minCasesByClassTest        = minCasesByClassTest))
      
      class(out) <- "SOptim.Classifier" # Output class when runFullCalibration = TRUE
      return(out)
    }
    else{
      
      ## Average performance in cross-validation rounds
      avgPerformance <- mean(perfStats, na.rm=TRUE)
      if(verbose) cat(".. Average ",evalMetricName," (cross-validation) = ",round(avgPerformance,3),"\n\n",sep="")
      
      return(avgPerformance)
    }
  }
}


#' Summary for SOptim.Classifier objects
#' 
#' Produces a summary of classification performance results for a 
#' \code{SOptim.Classifier} object.
#' 
#' @param object An object of class \code{SOptim.Classifier} created by 
#' \code{\link{calibrateClassifier}} with option \code{runFullCalibration=TRUE}.
#' 
#' @param ... Additional arguments affecting the summary produced.
#' 
#' @return A summary of the object of class \code{SOptim.ClassifierSummary} 
#' which can be used in print
#' 
#' @seealso  \link{calibrateClassifier}
#' 
#' @method summary SOptim.Classifier
#' 
#' @export

summary.SOptim.Classifier <- function(object, ...){
  
  # Calculate all performance evaluation metrics based on object
  #
  evalMatrix <- evalPerformanceClassifier(object)
  nr <- nrow(evalMatrix)
  
  evalMatrix <- rbind(evalMatrix,
                      apply(evalMatrix[-nr,], 2, mean),
                      apply(evalMatrix[-nr,], 2, sd))
  
  rownames(evalMatrix) <- c(rownames(evalMatrix)[-c(nr+1,nr+2)],
                            paste("AVG (",object$ClassParams$evalMethod,")",sep=""),
                            paste("ST-DEV (",object$ClassParams$evalMethod,")",sep=""))
   
  cm <- object$ConfMat$FULL
  
  if(object$ClassParams$nClassType == "multi-class"){
    
    # Calculate user and producer accuracies
    #
    nc <- ncol(cm)
    user.acc <- vector(length = nc)
    prod.acc <- vector(length = nc)
    
    for(i in 1:ncol(cm)){
      col.sum <- sum(cm[,i])
      row.sum <- sum(cm[i,])
      diag.val <- cm[i,i]
      
      prod.acc[i] <- diag.val / col.sum
      user.acc[i] <- diag.val / row.sum
    }

    acc <- data.frame(ClassLabel = colnames(cm), 
                      UserAccuracy      = round(user.acc,3), 
                      ProducerAccuracy  = round(prod.acc,3))

  }else{
    acc <- NA
  }

  out <- list(evalMatrix  = evalMatrix,
            ConfMat       = cm,
            ProdUserAcc   = acc)
  class(out) <- "SOptim.ClassifierSummary"
  
  return(invisible(out))
  
}

#' Print classification summary
#' 
#' Prints an object of class \code{SOptim.ClassifierSummary} generated by 
#' \code{\link{summary.SOptim.Classifier}}.
#' 
#' @param x \code{SOptim.ClassifierSummary} generated by 
#' \code{\link{summary.SOptim.Classifier}}.
#' 
#' @return None
#' 
#' @method print SOptim.ClassifierSummary
#' 
#' @export
#' 

print.SOptim.ClassifierSummary <- function(x){
  
  cat("\n\n|| Supervised classification summary ||\n\n")
  
  cat(".: Classification performance:\n\n",sep="")
  print(round(x[["evalMatrix"]], 2))
  
  cat("\n\n.: Confusion matrix for full dataset calibration:\n\n")
  print(x[["ConfMat"]])
  
  if(!is.data.frame(x[["ProdUserAcc"]])){
    cat("\n\n.: Producer/user accuracies for full dataset:\n\n")
    print(x[["ProdUserAcc"]])
  }
}
