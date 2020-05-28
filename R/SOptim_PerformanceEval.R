

## ---------------------------------------------------------------------------------------------------------------- ##
## --- PERFORMANCE EVALUATION FUNCTIONS ----
## ---------------------------------------------------------------------------------------------------------------- ##


#' Generate a confusion matrix
#' 
#' A function used to generate a confusion matrix for classification evaluation.
#' 
#' @param obs A factor or integer vector with observed values (class labels).
#' @param pred A factor or integer vector with predicted values (class labels).
#' 
#' @return A matrix object with columns correspoding to observed and rows to 
#' predicted values/classes.
#' 
#' @examples
#'  
#' obs<-sample(0:1, 20, replace = TRUE)
#' pred<-sample(0:1, 20, replace = TRUE)
#' generateConfusionMatrix(obs, pred)
#' 
#' @export

generateConfusionMatrix<-function(obs, pred){
  
  # Subset NA values
  ind <- !(is.na(obs) & is.na(pred))
  obs <- obs[ind]
  pred <- pred[ind]
  # Do crosstab
  f <- factor(union(unique(obs), unique(pred)))
  obs <- factor(obs, levels = levels(f))
  pred <- factor(pred, levels = levels(f))
  cm <- as.matrix(table(Predicted=pred, Observed=obs))
  
  return(cm)
  
}


## ---------------------------------------------------------------------------------------------------------------- ##
## Single-class evaluation functions ----
## ---------------------------------------------------------------------------------------------------------------- ##


#' Calculate the maximum Cohen Kappa index
#' 
#' A  function used to calculate the maximum value of the Cohen Kappa index 
#' when the predicted output is given in probability. This function applies only to 
#' single-class problems (e.g., presence/absence of a vegetation type, species).
#' 
#' 
#' @param obs Integer vector with observed values (class labels, tipically 0 or 1).
#' @param pred Numeric with predicted probabilities.
#' 
#' @return A list with two elements:
#' \itemize{
#'   \item values A data frame containing the thresholds and the calculated Kappa values; 
#'   \item maxKappa The maximum Kappa value.
#' }
#' 
#' @details This function will calculate the Kappa value from 0 to 1 with intervals 
#' of 0.01. The maximum value is returned.
#' 
#' @examples
#' 
#' obs<-sample(c(0,1),100,replace = TRUE)
#' pred<-runif(100,0,1)
#' kappaSingleClass(obs,pred)
#' 
#' @importFrom stats runif
#' @export

kappaSingleClass<- function(obs, pred){
  
  i<-0
  k<-vector(mode="numeric", length = 101)
  thresh.vals<-vector(mode="numeric", length = 101)
  
  for(thresh in seq(0,1,0.01)){
    
    i<-i+1	
    k[i]<-evaluatePerformance(obs,as.integer(pred > thresh))$Metrics$Kappa[1]
    thresh.vals[i]<-thresh
  }
  
  DF<-data.frame(thresh=thresh.vals,Kappa=k)
  return(list(values=DF, maxKappa=DF[which.max(DF[,2]),]))
}


#' Calculate the maximum Peirce skill score (true skill statistic or Hanssen and 
#' Kuipers discriminant)
#' 
#' A  function used to calculate the maximum value of the Peirce skill score (PSS)
#' when the predicted output is given in probability. This function applies only to 
#' single-class problems (e.g., presence/absence of a vegetation type, species).
#' 
#' 
#' @param obs Integer vector with observed values (class labels, tipically 0 or 1).
#' @param pred Numeric vector with predicted probabilities.
#' 
#' @return A list with two elements:
#' \itemize{
#'   \item values A data frame containing the thresholds and the calculated PSS values; 
#'   \item maxPSS The maximum PSS value.
#' }
#' 
#' @details This function will calculate the PSS value from 0 to 1 with intervals 
#' of 0.01. The maximum value is returned.
#' 
#' @examples
#' obs<-sample(c(0,1),100,replace = TRUE)
#' pred<-runif(100,0,1)
#' pssSingleClass(obs,pred)
#'
#' @importFrom stats runif
#' @export

pssSingleClass <- function(obs, pred){
  
  i<-0
  k<-vector(mode="numeric", length = 101)
  thresh.vals<-vector(mode="numeric", length = 101)
  
  for(thresh in seq(0,1,0.01)){
    i<-i+1	
    k[i]<-evaluatePerformance(obs,as.integer(pred > thresh))$Metrics$PeirceSkillScore[1]
    thresh.vals[i]<-thresh
  }
  
  DF<-data.frame(thresh=thresh.vals,PSS=k)
  return(list(values=DF,maxPSS=DF[which.max(DF[,2]),]))
}


#' Calculate the maximum Gerrity skill score
#' 
#' A  function used to calculate the maximum value of the Gerrity skill score (GSS) 
#' when the predicted output is given in probability. This function applies only to 
#' single-class problems (e.g., presence/absence of a vegetation type, species). 
#' 
#' 
#' @param obs Integer vector with observed values (class labels, tipically 0 or 1).
#' @param pred Numeric vector with predicted probabilities.
#' 
#' @return A list with two elements:
#' \itemize{
#'   \item values A data frame containing the thresholds and the calculated GSS values; 
#'   \item maxGSS The maximum GSS value.
#' }
#' 
#' @details This function will calculate the GSS value from 0 to 1 with intervals 
#' of 0.01. The maximum value is returned.      
#' This statistic uses all entries in the confusion matrix, so it does not depend 
#' on the forecast distribution, and is equitable (i.e., random and constant forecasts 
#' score a value of 0). 
#' GSS does not reward conservative forecasting but rather rewards forecasts for correctly 
#' predicting the less likely categories. Smaller errors are penalized less than 
#' larger forecast errors. This is achieved through the use of the scoring matrix. 
#' 
#' @examples
#' obs<-sample(c(0,1),100,replace = TRUE)
#' pred<-runif(100,0,1)
#' gssSingleClass(obs,pred)
#' 
#' @importFrom stats runif
#' @export

gssSingleClass <- function(obs, pred){
  
  i<-0
  k<-vector(mode="numeric", length = 101)
  thresh.vals<-vector(mode="numeric", length = 101)
  
  for(thresh in seq(0,1,0.01)){
    i<-i+1	
    k[i]<-GerritySkillScore(obs, as.integer(pred > thresh))
    thresh.vals[i]<-thresh
  }
  
  DF<-data.frame(thresh=thresh.vals,GSS=k)
  return(list(values=DF,maxGSS=DF[which.max(DF[,2]),]))
}


#' Calculate the Area Under the ROC Curve (AUC)
#' 
#' A  function used to calculate the Area Under the (Receiver Operating Characteristic) 
#' Curve (AUC) when the predicted output is given in probability. This function applies 
#' only to single-class problems (e.g., presence/absence of a vegetation type, species).
#' 
#' 
#' @param obs Integer vector with observed values (class labels, tipically 0 or 1).
#' @param pred Numeric vector with predicted probabilities.
#' 
#' @return Numeric. The AUC value calculated.
#' 
#' @details Uses the \pkg{ROCR} package for calculations.
#' 
#' @seealso \code{\link[ROCR]{prediction}}, \code{\link[ROCR]{performance}} 
#' 
#' @examples
#' obs<-sample(c(0,1),100,replace = TRUE)
#' pred<-runif(100,0,1)
#' aucSingleClass(obs,pred)
#' 
#' @export
#' @import ROCR
#' @importFrom stats runif
#' 

aucSingleClass <- function(obs, pred){
  
  # An internal function used to calculate the distance between one point (a) 
  # and a set of points (b) defined in a matrix or data frame
  EucDist <- function(a, b){
    x1 <- a[1]
    y1 <- a[2]
    dd<-vector(length=nrow(b), mode="numeric")
    for(i in 1:nrow(b)){
      x2 <- b[i,1]
      y2 <- b[i,2]
      dd[i] <- sqrt((x2-x1)^2 + (y2-y1)^2) 
    }
    return(dd)
  }
  
  # Calculate the AUC value
  p <- ROCR::prediction(pred, obs)
  AUC <- ROCR::performance(p, "auc")
  
  # This part is used to extract the threshold/cutoff value that minimizes 
  # the distance between the ROC and the (0,1) point
  perf<-ROCR::performance(p, "spec", "sens")
  TMP_DF <- data.frame(cut = perf@alpha.values[[1]],
                   spec_FPR = 1 - perf@x.values[[1]],
                   sens_TPR = perf@y.values[[1]])
  dist01 <- EucDist(c(0,1), TMP_DF[,2:3])
  return(c(AUC = AUC@y.values[[1]][1], thresh = TMP_DF[which.min(dist01), "cut"]))
}


## ---------------------------------------------------------------------------------------------------------------- ##
## Multi-class evaluation functions ----
## ---------------------------------------------------------------------------------------------------------------- ##


#' Calculate the Gerrity Skill score
#' 
#' A  function used to calculate the Gerrity skill score (GSS). This statistic uses all entries 
#' in the confusion matrix, so it does not depend on the forecast distribution, and is equitable 
#' (i.e., random and constant forecasts score a value of 0). GSS does not reward conservative forecasting 
#' but rather rewards forecasts for correctly predicting the less likely categories. Smaller errors are 
#' penalized less than larger forecast errors. This is achieved through the use of the scoring matrix. 
#' 
#' 
#' @param obs Integer vector with observed values (class labels).
#' @param pred Integer vector with predicted values (class labels).
#' 
#' @return Numeric. The GSS value calculated.
#' 
#' @details
#' Adapted from verification package at 
#' URL: \url{https://github.com/cran/verification/blob/32e864afca8895391f179c2a97707e126ca31f20/R/multi.cont.R}
#' 
#' @examples
#' 
#' obs<-sample(c(1:5),100,replace = TRUE)
#' pred<-sample(c(1:5),100,replace = TRUE)
#' GerritySkillScore(obs, pred)
#' 
#'
#' @export

GerritySkillScore<-function(obs, pred){
  
  # Generate confusion matrix
  cm<-generateConfusionMatrix(obs, pred)

  P.cm   <- cm/sum(cm)
  p.obs  <- apply(P.cm, 2, sum)
  K      <- nrow(cm)
  S      <- matrix(NA, ncol = nrow(cm), nrow = nrow(cm))
  p.base <- p.obs
  
  ## uses notation from Joliffe p. 89-90.
  b <- 1/(K-1)
  a <- numeric()
  
  for(k in 1:K){
    a[k] <- (1-sum(p.base[1:k]))/sum(p.base[1:k])
  }
  
  ### fill diagonal
  for(i in 1:K){
    if(i ==1){
      S[i,i]<- b* sum(a[1:(K-1)])
    }
    else if(i==K){
      S[i,i]<- b*(sum(1/a[1:(i-1)]))
    } 
    else{
      S[i,i]<- b*(sum(1/a[1:(i-1)]) +  sum(a[i:(K-1)])) 
    }
  }
  
  ### fill off diagonal   
  for(i in 1:(K-1)){
    
    for(j in (i+1):K){
      
      if(i == 1 & j == K){
        S[j,i] <- S[i,j] <- b*(-1*(j-i))
      }
      else{
        if(i == 1){    
          S[j,i] <- S[i,j] <- b*(-1* (j-i) + sum(a[j:(K-1)]))
        }
        else{
          if(j == K){
            S[j,i] <- S[i,j] <- b*(sum(1/a[1:(i-1)] ) - (j-i))
          }
          else{
            S[j,i] <- S[i,j] <- b*(sum(1/a[1:(i-1)] )- (j-i) + sum(a[j:(K-1)]))
          }
        }
      }
    }
  }
  GSS <- sum(P.cm * S)
  return(GSS)
}


#' Calculates several performance statistics
#' 
#' A  function used to calculate several performance evaluation metrics, namely:
#' accuracy, Peirce skill score, and Cohen Kappa (as aggregated performance metrics, 
#' for all classes) as well as precision, recall and the F1-statistic (for each class).
#' 
#' 
#' @param obs Integer vector with observed values (class labels).
#' @param pred Integer vector with predicted values (class labels).
#' @param cm A confusion matrix generated by \code{\link{generateConfusionMatrix}} function.
#' 
#' @return A list object containing:
#'    \itemize{
#'      
#'      \item Numeric matrix. Holds the confusion matrix;
#'      
#'      \item Numeric data frame with several metrics by column:
#'         \itemize{
#'          \item Class - class names/numbers,
#'          \item Accuracy - overall accuracy,
#'          \item PeirceSkillScore - Peirce skill score (Hanssen and Kuipers discriminant 
#'          or true skill statistic),
#'          \item Kappa - Cohen Kappa statistic,
#'          \item Precision - precision (by class)
#'          \item Recall - recall (by class)
#'          \item F1 - F1-statistic (by class).
#'         }} 
#' 
#' @details
#' Adapted from \url{https://github.com/saidbleik/Evaluation/blob/master/eval.R}.
#' 
#' @examples
#' obs<-sample(c(1:5),100,replace = TRUE)
#' pred<-sample(c(1:5),100,replace = TRUE)
#' evaluatePerformance(obs, pred)
#' 
#' @importFrom stats sd
#' @export

evaluatePerformance<-function(obs=NULL, pred=NULL, cm=NULL){
  
  if(is.null(cm)) {
    # Tabulate to get the correct math notation used
    cm <- t(generateConfusionMatrix(obs, pred))
  }
  
  n <- sum(cm) # number of instances
  nc <- nrow(cm) # number of classes
  diag <- diag(cm) # number of correctly classified instances per class 
  rowsums <- apply(cm, 1, sum) # number of instances per class
  colsums <- apply(cm, 2, sum) # number of predictions per class
  p <- rowsums / n # distribution of instances over the observed classes
  q <- colsums / n # distribution of instances over the predicted classes
  
  # Accuracy
  accuracy <- sum(diag) / n
  
  # Peirce Skill Score
  a<-sum(colsums * rowsums) / n^2
  b<-sum(rowsums^2) / n^2
  pss <- (accuracy - a) / (1 - b)
  
  # per class
  recall <- diag / rowsums
  precision <- diag / colsums
  f1 <- 2 * precision * recall / (precision + recall)

  # random accuracy
  expAccuracy <- sum(p*q)
  
  # Kappa
  kappa <- (accuracy - expAccuracy) / (1 - expAccuracy)
  
  classNames <- names(diag)
  if(is.null(classNames)) classNames <- paste("C",(1:nc),sep="")
  
  evalMetricsDF <- data.frame(
    Class = classNames,
    # Aggregated metrics ----- #
    Accuracy = accuracy,
    PeirceSkillScore = pss,
    Kappa = kappa,
    # By class metrics ------ #
    Precision = precision,
    Recall = recall,
    F1 = f1)
  
    return(list(
      ConfusionMatrix = cm,
      Metrics = evalMetricsDF
    ))
}


#' A generic function for calculating performance metrics
#' 
#' A single wrapper function used to calculate several performance evaluation metrics both 
#' for single- or multi-class problems.
#' 
#' 
#' @param obs Integer vector with observed values (class labels).
#' 
#' @param pred Integer vector with predicted values (class labels) or numeric vector with 
#' predicted probabilities.
#' 
#' @param stat (Character) defining the statistic name ("Kappa", "PSS", "GSS", "AUC" - 
#' single-class only or "Accuracy" - multi-class only).
#' 
#' @param nClassType (Character) defining the type of classification problem, either: 
#' "single-class" or "multi-class".
#' 
#' @return Numeric with the performance metric value
#' 
#' @examples
#' obs<-sample(1:5, 100, replace = TRUE)
#' pred<-sample(1:5, 100, replace = TRUE)
#' evalPerformanceGeneric(obs, pred, stat = "Kappa", nClassType = "multi-class")
#' 
#' @export


evalPerformanceGeneric<-function(obs, pred, stat, nClassType){
  
  
  if(nClassType=="single-class"){ ## --- Single-class ------------------------------- ##
    
    if(stat=="Kappa"){
      evalStatTmp <- kappaSingleClass(obs, pred)[[2]]
      out <- evalStatTmp[1,2]
      attr(out,"thresh") <- evalStatTmp[1,1]
      return(out) # Get the maxKappa value only
    }
    else if(stat=="PSS"){
      evalStatTmp <- pssSingleClass(obs, pred)[[2]]
      out <- evalStatTmp[1,2]
      attr(out,"thresh") <- evalStatTmp[1,1]
      return(out) # Get the maximum PSS value only
    }
    else if(stat=="GSS"){
      evalStatTmp <- gssSingleClass(obs, pred)[[2]]
      out <- evalStatTmp[1,2]
      attr(out,"thresh") <- evalStatTmp[1,1]
      return(out) # Get the maximum GSS value only
    }
    else if(stat=="AUC"){
      evalStatTmp <- aucSingleClass(obs, pred)
      out <- evalStatTmp[1]
      attr(out,"thresh") <- evalStatTmp[2]
      return(out) # Calculate ROC-AUC
    }
    else{
      stop("Invalid option for the evaluation metric in single-class mode! Check input parameters")
    }
  }
  else if(nClassType=="multi-class"){ ## --- Multi-class ---------------------------- ##
    
    if(stat=="Accuracy"){
      evaluatePerformance(obs, pred)$Metrics$Accuracy[1]
    }
    else if(stat=="PSS"){
      evaluatePerformance(obs, pred)$Metrics$PeirceSkillScore[1]
    }
    else if(stat=="GSS"){
      return(GerritySkillScore(obs, pred))
    }
    else if(stat=="Kappa"){
      evaluatePerformance(obs, pred)$Metrics$Kappa[1]
    }
    else{
      stop("Invalid option for the evaluation metric in multi-class mode! Check input parameters")
    }
  }
  else{
    stop("The value defined in nClassType is not valid!")
  }
}


#' A function for calculating performance metrics for SOptim.Classifier objects
#' 
#'This function can be used to calculate all available  performance evaluation metrics both 
#' for single- or multi-class problems using a \code{SOptim.Classifier} objects (check out 
#' \code{\link{calibrateClassifier}} for more details).
#' 
#' @param obj An object of class \code{SOptim.Classifier} generated by \code{\link{calibrateClassifier}} 
#' with option \code{runFullCalibration = TRUE}.
#' 
#' @return A numeric matrix with the evaluation metrics. For single-class problems an additional column 
#' by metric (with suffix "_Thresh") is added to the matrix with threshold values used to maximize each 
#' evaluation metric.
#' 
#' @details 
#' 
#' List of available evaluation metrics:           
#' \strong{--- SINGLE-CLASS ---}               
#' \itemize{
#'     \item \strong{Kappa} - Cohen Kappa (maximum)
#'     \item \strong{PSS} - Peirce Skill score (maximum)
#'     \item \strong{GSS} - Gerrity Skill score (maximum)
#'     \item \strong{AUC} - Area Under the Receiver Operating Curve
#' }
#' \strong{--- MULTI-CLASS ---}         
#' \itemize{
#'     \item \strong{Kappa} - Cohen Kappa
#'     \item \strong{PSS} - Peirce Skill score
#'     \item \strong{GSS} - Gerrity Skill score
#'     \item \strong{Accuracy} - Overall accuracy
#' }
#' 
#' 
#' @export

evalPerformanceClassifier <- function(obj){
  
  if(!inherits(obj,"SOptim.Classifier")){
    stop("The input in obj must be an object of class SOptim.Classifier generated 
         by calibrateClassifier with option runFullCalibration = TRUE!")
  }
    
  nClassType <- obj$ClassParams$nClassType
  nSets <- length(obj$predTestSet)
  setNames <- names(obj$predTestSet)
  
  if(nClassType == "single-class"){
    evalMetricNames <- evaluationMetrics_singleClass()
    out <- matrix(NA,nrow=nSets,ncol=length(evalMetricNames)*2 , 
                  dimnames = list(setNames, paste(rep(evalMetricNames,each=2),c("","_Thresh"),sep="")) )
  }
  else if(nClassType == "multi-class"){
    evalMetricNames <- evaluationMetrics_multiClass()
    out <- matrix(NA,nrow=nSets,ncol=length(evalMetricNames),dimnames = list(setNames,evalMetricNames))
  }

  for(nSet in 1:nSets){
    
    for(evalMetric in evalMetricNames){

      if(nClassType=="multi-class"){
        evalMetricVal <- evalPerformanceGeneric(obs = obj$obsTestSet[[nSet]], 
                                                pred = obj$predTestSet[[nSet]], 
                                                stat=evalMetric, 
                                                nClassType=nClassType)
        out[nSet,evalMetric] <- evalMetricVal
      }
      else if(nClassType=="single-class"){
        evalMetricVal <- evalPerformanceGeneric(obs = obj$obsTestSet[[nSet]], 
                                                pred = obj$predTestSet[[nSet]], 
                                                stat=evalMetric, 
                                                nClassType=nClassType)
        out[nSet,evalMetric] <- evalMetricVal
        out[nSet,paste(evalMetric,"_Thresh",sep="")] <- attr(evalMetricVal, "thresh")
      }
    }
  }
  return(out)
}

