
context("Test supervised classification main function")

library(SegOptim)

test_that("Test RF classifier (single-class)",{
  
  DF <- data.frame(SID   = 1:250, 
                   train = sample(0:1, 250, replace=TRUE),
                   v1 = rnorm(250),
                   v2 = rnorm(250),
                   v3 = rnorm(250))
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"
  
  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "RF",
                            classificationMethodParams = NULL,
                            balanceTrainData = FALSE,
                            balanceMethod = "ubOver",
                            evalMethod = "HOCV",
                            evalMetric = "Kappa",
                            trainPerc = 0.8,
                            nRounds = 10,
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)
  
  expect_is(cl,"SOptim.Classifier")
  
})

test_that("Test GBM classifier (single-class)",{
  
  DF <- data.frame(SID   = 1:250, 
                   train = sample(0:1, 250, replace=TRUE),
                   v1 = rnorm(250),
                   v2 = rnorm(250),
                   v3 = rnorm(250))
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"
  
  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "GBM",
                            classificationMethodParams = NULL,
                            balanceTrainData = FALSE,
                            balanceMethod = "ubOver",
                            evalMethod = "HOCV",
                            evalMetric = "Kappa",
                            trainPerc = 0.8,
                            nRounds = 10,
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)
  
  expect_is(cl,"SOptim.Classifier")
  
})

## FDA NOT WORKING!!!

# test_that("Test FDA classifier",{
#   
#   DF <- data.frame(SID   = 1:250, 
#                    train = sample(0:1, 250, replace=TRUE),
#                    v1 = rnorm(250),
#                    v2 = rnorm(250),
#                    v3 = rnorm(250))
#   
#   calDataObj <- list(calData = DF, classifFeatData = DF)
#   attr(calDataObj, "nClassType") <- "single-class"
#   class(calDataObj) <- "SOptim.CalData"
#   
#   cl <- calibrateClassifier(calData = calDataObj,
#                             classificationMethod = "FDA",
#                             classificationMethodParams = NULL,
#                             balanceTrainData = FALSE,
#                             balanceMethod = "ubOver",
#                             evalMethod = "HOCV",
#                             evalMetric = "Kappa",
#                             trainPerc = 0.8,
#                             nRounds = 10,
#                             minTrainCases = 5,
#                             minCasesByClassTrain = 5,
#                             minCasesByClassTest = 5,
#                             runFullCalibration = TRUE,
#                             verbose = FALSE)
#   
#   expect_is(cl,"SOptim.Classifier")
#   
# })

test_that("Test KNN classifier (single-class)",{
  
  DF <- data.frame(SID   = 1:250, 
                   train = sample(0:1, 250, replace=TRUE),
                   v1 = rnorm(250),
                   v2 = rnorm(250),
                   v3 = rnorm(250))
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"
  
  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "KNN",
                            classificationMethodParams = NULL,
                            balanceTrainData = FALSE,
                            balanceMethod = "ubOver",
                            evalMethod = "HOCV",
                            evalMetric = "Kappa",
                            trainPerc = 0.8,
                            nRounds = 10,
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)
  
  expect_is(cl,"SOptim.Classifier")
  
})

test_that("Test SVM classifier (single-class)",{
  
  DF <- data.frame(SID   = 1:250, 
                   train = sample(0:1, 250, replace=TRUE),
                   v1 = rnorm(250),
                   v2 = rnorm(250),
                   v3 = rnorm(250))
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"
  
  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "SVM",
                            classificationMethodParams = NULL,
                            balanceTrainData = FALSE,
                            balanceMethod = "ubOver",
                            evalMethod = "HOCV",
                            evalMetric = "Kappa",
                            trainPerc = 0.8,
                            nRounds = 10,
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)
  
  expect_is(cl,"SOptim.Classifier")
  
})

