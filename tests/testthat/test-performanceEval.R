
context("Test supervised classification performance evaluation functions")

library(SegOptim)

test_that("Test generateConfusionMatrix",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(generateConfusionMatrix(obs, pred),"table")
  expect_equal(dim(generateConfusionMatrix(obs, pred)), c(2,2))
})

test_that("Test kappaSingleClass",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(kappaSingleClass(obs, pred),"list")
  expect_equal(length(kappaSingleClass(obs, pred)), c(2))
})

test_that("Test pssSingleClass",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(pssSingleClass(obs, pred),"list")
  expect_equal(length(pssSingleClass(obs, pred)), c(2))
})

test_that("Test gssSingleClass",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(gssSingleClass(obs, pred),"list")
  expect_equal(length(gssSingleClass(obs, pred)), c(2))
})

test_that("Test aucSingleClass",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(aucSingleClass(obs, pred),"numeric")
  expect_equal(length(aucSingleClass(obs, pred)), c(2))
  expect_equal(names(aucSingleClass(obs, pred)), c("AUC", "thresh"))
})

test_that("Test GerritySkillScore",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  expect_is(GerritySkillScore(obs, pred),"numeric")
  expect_equal(length(GerritySkillScore(obs, pred)), c(1))
})

test_that("Test evaluatePerformance",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  evp <- evaluatePerformance(obs, pred)
  
  expect_is(evp,"list")
  expect_is(evp$ConfusionMatrix,"table")
  expect_is(evp$Metrics,"data.frame")
  expect_equal(length(evp), c(2))
  expect_equal(names(evp), c("ConfusionMatrix","Metrics"))
  expect_equal(dim(evp$ConfusionMatrix), c(2,2))
})


test_that("Test evalPerformanceGeneric 1",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "Kappa", nClassType = "single-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
  expect_is((attributes(evp))[["thresh"]], "numeric")
})

test_that("Test evalPerformanceGeneric 2",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "PSS", nClassType = "single-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
  expect_is((attributes(evp))[["thresh"]], "numeric")
})

test_that("Test evalPerformanceGeneric 3",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "GSS", nClassType = "single-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
  expect_is((attributes(evp))[["thresh"]], "numeric")
})

test_that("Test evalPerformanceGeneric 4",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "AUC", nClassType = "single-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
  expect_is((attributes(evp))[["thresh"]], "numeric")
})

test_that("Test evalPerformanceGeneric 5",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "Accuracy", nClassType = "multi-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
})

test_that("Test evalPerformanceGeneric 6",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "PSS", nClassType = "multi-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
})

test_that("Test evalPerformanceGeneric 7",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "GSS", nClassType = "multi-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
})

test_that("Test evalPerformanceGeneric 8",{
  
  obs  <- sample(c(0,1), 100, replace = TRUE)
  pred <- sample(c(0,1), 100, replace = TRUE)
  
  evp <- evalPerformanceGeneric(obs, pred, "Kappa", nClassType = "multi-class")
  
  expect_is(evp,"numeric")
  expect_equal(length(evp), c(1))
})

test_that("Test evalPerformanceClassifier - single-class",{
  
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
  
  evp <- evalPerformanceClassifier(cl)
  expect_is(evp, "matrix")
  expect_equal(dim(evp), c(11, 8))
})

test_that("Test evalPerformanceClassifier - multi-class",{
  
  DF <- data.frame(SID   = 1:250, 
                   train = sample(1:3, 250, replace=TRUE),
                   v1 = rnorm(250),
                   v2 = rnorm(250),
                   v3 = rnorm(250))
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "multi-class"
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
  
  evp <- evalPerformanceClassifier(cl)
  expect_is(evp, "matrix")
  expect_equal(dim(evp), c(11, 4))
})


