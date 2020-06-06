
context("Test supervised classification main function")

library(SegOptim)


test_that("Test generateDefaultClassifierParams",{
  
  DF <- data.frame(SID   = 1:100, 
                   train = sample(0:1, 100, replace=TRUE),
                   v1 = rnorm(100),
                   v2 = rnorm(100),
                   v3 = rnorm(100))
  
  defParams <- generateDefaultClassifierParams(DF)
  
  expect_is(defParams,"list")
  expect_equal(names(defParams), c("RF","KNN","FDA","SVM","GBM"))
  
})

test_that("Test replaceDefaultClassificationParams",{
  
  DF <- data.frame(SID   = 1:100, 
                   train = sample(0:1, 100, replace=TRUE),
                   v1 = rnorm(100),
                   v2 = rnorm(100),
                   v3 = rnorm(100))
  
  defParams <- generateDefaultClassifierParams(DF)
  newParams <- replaceDefaultClassificationParams(defParams, 
                                                  list(RF=list(mtry=2, 
                                                               ntree=500)))
  
  expect_is(defParams,"list")
  expect_equal(names(defParams), c("RF","KNN","FDA","SVM","GBM"))
  
  expect_is(newParams,"list")
  expect_equal(newParams$RF$mtry,2)
  expect_equal(newParams$RF$ntree,500)
})

test_that("Test generateFormula",{
  
  DF <- data.frame(SID   = 1:100, 
                   train = sample(0:1, 100, replace=TRUE),
                   v1 = rnorm(100),
                   v2 = rnorm(100),
                   v3 = rnorm(100))
  
  form <- generateFormula(DF)
  expect_is(form, "formula")
  
})

test_that("Test createDataSplits",{
  
  train <- sample(0:1, 100, replace=TRUE)
  
  
  ds <- createDataSplits(train, evalMethod = "OOB")
  expect_is(ds,c("list", "dataSplit"))
  expect_equal(length(ds), 1)
  
  ds1 <- createDataSplits(train, evalMethod = "10FCV")
  expect_is(ds1,c("list", "dataSplit"))
  expect_equal(length(ds1), 10)
  
  ds2 <- createDataSplits(train, evalMethod = "5FCV")
  expect_is(ds2,c("list", "dataSplit"))
  expect_equal(length(ds2), 5)
  
  ds3 <- createDataSplits(train, evalMethod = "HOCV", 
                          nRounds = 20, trainPerc = 0.8)
  expect_is(ds3,c("list", "dataSplit"))
  expect_equal(length(ds3), 20)
  
})


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


test_that("Test summary of RF classifier (single-class)",{
  
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
                            evalMethod = "10FCV",
                            evalMetric = "Kappa",
                            trainPerc = 0.8,
                            nRounds = 10,
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)
  
  summaryObj <- summary(cl)
  expect_is(summaryObj, "list")
  expect_equal(names(summaryObj), c("evalMatrix", "ConfMat", "ProdUserAcc"))
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

