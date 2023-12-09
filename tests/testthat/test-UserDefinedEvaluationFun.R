
context("Test user-defined performance/accuracy functions")

library(SegOptim)
library(terra)


test_that("User-defined accuracy function provides correct outputs",{
  

  calData <- list(calData = data.frame(
    train = sample(0:1,1000,replace = TRUE),
    v1    = rnorm(1000),
    v2    = rnorm(1000), 
    v3    = rnorm(1000)), 
    classifFeatData = data.frame(v1 = rnorm(1000),
                                 v2 = rnorm(1000),
                                 v3 = rnorm(1000)))
  
  class(calData) <- "SOptim.CalData"
  
  # Define the specific function to test
  accuracy <- function(obs, pred){

    threshs <- seq(0.025, 1, by = 0.025)
    accs <- vector(mode="numeric",length = length(threshs))

    i <- 0
    for(thresh in threshs){
      i<-i+1
      confMat <- generateConfusionMatrix(obs, as.integer(pred >= thresh))
      accs[i] <- sum(diag(confMat)) / sum(confMat)
    }
    out <- max(accs)
    attr(out,which = "thresh") <- threshs[which.max(accs)]
    return(out)
  }

  classifObj <- calibrateClassifier( calData              = calData,
                                     classificationMethod = "RF",
                                     balanceTrainData     = FALSE,
                                     balanceMethod        = "ubOver",
                                     evalMethod           = "5FCV",
                                     evalMetric           = accuracy,
                                     minTrainCases        = 20,
                                     minCasesByClassTrain = 10,
                                     minCasesByClassTest  = 5,
                                     runFullCalibration   = TRUE,
                                     verbose              = FALSE)

  expect_is(classifObj, "SOptim.Classifier")

})

