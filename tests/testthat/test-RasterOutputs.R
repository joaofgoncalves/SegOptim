
context("Test raster output functions")

library(SegOptim)
library(raster)
library(NLMR)
library(igraph)

test_that("Test getTrainRasterSegments outputs",{
  
  rstSegm <- simRasterSegments2()
  rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  
  trainSegs <- getTrainRasterSegments(rstTrain, rstSegm, filename = NULL, 
                                      useThresh = TRUE, thresh = 0.5, 
                                      na.rm = TRUE, dup.rm = TRUE, minImgSegm = 10, 
                                      ignore = FALSE, verbose = FALSE)
  
  expect_is(trainSegs,"RasterLayer")
  expect_equal(cellStats(trainSegs,"min"), 0)
  expect_equal(cellStats(trainSegs,"max"), 1)
})


## ---------------------------------------------------------------------- ##
## Single-class tests
## ---------------------------------------------------------------------- ##


test_that("Test predictSegments for RF classifier (single-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")

  
  DF <- rbind(data.frame(SID   = 1:250,
                         train = 0,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              
              data.frame(SID   = 251:500,
                         train = 1,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"

  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "RF",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                                calData       = calDataObj,
                                rstSegm       = rstSegm,
                                predictFor    = "all",
                                filename      = NULL,
                                verbose       = FALSE,
                                na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  #expect_equal(cellStats(predSegms,"min"), 0)
  #expect_equal(cellStats(predSegms,"max"), 1)

})


test_that("Test predictSegments for GBM classifier (single-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")

  DF <- rbind(data.frame(SID   = 1:250,
                         train = 0,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              
              data.frame(SID   = 251:500,
                         train = 1,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"

  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "GBM",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  # expect_equal(cellStats(predSegms,"min"), 0)
  # expect_equal(cellStats(predSegms,"max"), 1)

})

test_that("Test predictSegments for SVM classifier (single-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")

  DF <- rbind(data.frame(SID   = 1:250,
                         train = 0,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              
              data.frame(SID   = 251:500,
                         train = 1,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"

  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "SVM",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  #expect_equal(cellStats(predSegms,"min"), 0)
  #expect_equal(cellStats(predSegms,"max"), 1)

})

# test_that("Test predictSegments for KNN classifier (single-class)",{
# 
# 
#   rstSegm <- simRasterSegments2()
#   rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
#   rstFeat <- simRasterFeatures()
# 
#   calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
#                                minImgSegm = 10, verbose = FALSE)
# 
#   expect_is(calDataObj, "SOptim.CalData")
#   expect_equal(names(calDataObj), c("calData", "classifFeatData"))
#   expect_is(calDataObj$calData, "data.frame")
#   expect_is(calDataObj$classifFeatData, "data.frame")
# 
#   cl <- calibrateClassifier(calData = calDataObj,
#                             classificationMethod = "KNN",
#                             balanceTrainData = FALSE,
#                             evalMethod = "5FCV",
#                             evalMetric = "Kappa",
#                             minTrainCases = 5,
#                             minCasesByClassTrain = 5,
#                             minCasesByClassTest = 5,
#                             runFullCalibration = TRUE,
#                             verbose = FALSE)
# 
#   expect_is(cl,"SOptim.Classifier")
# 
#   predSegms <- predictSegments(classifierObj  = cl,
#                                calData       = calDataObj,
#                                rstSegm       = rstSegm,
#                                predictFor    = "all",
#                                filename      = NULL,
#                                verbose       = FALSE,
#                                na.rm         = TRUE)
# 
#   expect_is(predSegms,"RasterLayer")
#   expect_equal(cellStats(predSegms,"min"), 0)
#   expect_equal(cellStats(predSegms,"max"), 1)
# 
# })

test_that("Test predictSegments for FDA classifier (single-class)",{

  rstSegm <- simRasterSegments2()

  DF <- rbind(data.frame(SID   = 1:250,
                   train = 0,
                   v1 = rnorm(250,10),
                   v2 = rnorm(250,100),
                   v3 = rnorm(250,20)),

              data.frame(SID   = 251:500,
                         train = 1,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5))
  )

  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"

  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "FDA",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  #expect_equal(cellStats(predSegms,"min"), 0)
  #expect_equal(cellStats(predSegms,"max"), 1)

})


## ---------------------------------------------------------------------- ##
## Multi-class tests
## ---------------------------------------------------------------------- ##


test_that("Test predictSegments for RF classifier (multi-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(classes = c(1:3,NA), probs = c(0.3,0.3,0.3,0.1))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")
  
  DF <- rbind(data.frame(SID   = 1:250,
                         train = 1,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              data.frame(SID   = 251:500,
                         train = 2,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5)),
              data.frame(SID   = 501:750,
                         train = 3,
                         v1 = rnorm(250,1000),
                         v2 = rnorm(250,500),
                         v3 = rnorm(250,1300))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "multi-class"
  class(calDataObj) <- "SOptim.CalData"


  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "RF",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  expect_equal(cellStats(predSegms,"min"), 1)
  expect_equal(cellStats(predSegms,"max"), 3)

})

test_that("Test predictSegments for GBM classifier (multi-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(classes = c(1:3,NA), probs = c(0.3,0.3,0.3,0.1))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")
  DF <- rbind(data.frame(SID   = 1:250,
                         train = 1,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              data.frame(SID   = 251:500,
                         train = 2,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5)),
              data.frame(SID   = 501:750,
                         train = 3,
                         v1 = rnorm(250,1000),
                         v2 = rnorm(250,500),
                         v3 = rnorm(250,1300))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "multi-class"
  class(calDataObj) <- "SOptim.CalData"


  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "GBM",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  expect_equal(cellStats(predSegms,"min"), 1)
  expect_equal(cellStats(predSegms,"max"), 3)

})

test_that("Test predictSegments for SVM classifier (multi-class)",{


  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(classes = c(1:3,NA), probs = c(0.3,0.3,0.3,0.1))
  # rstFeat <- simRasterFeatures()
  # 
  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")
  
  DF <- rbind(data.frame(SID   = 1:250,
                         train = 1,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              data.frame(SID   = 251:500,
                         train = 2,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5)),
              data.frame(SID   = 501:750,
                         train = 3,
                         v1 = rnorm(250,1000),
                         v2 = rnorm(250,500),
                         v3 = rnorm(250,1300))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "multi-class"
  class(calDataObj) <- "SOptim.CalData"


  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "SVM",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  expect_equal(cellStats(predSegms,"min"), 1)
  expect_equal(cellStats(predSegms,"max"), 3)

})

# test_that("Test predictSegments for KNN classifier (multi-class)",{
# 
# 
#   rstSegm <- simRasterSegments2()
#   rstTrain <- simRasterTrain(classes = c(1:3,NA), probs = c(0.3,0.3,0.3,0.1))
#   rstFeat <- simRasterFeatures()
# 
#   calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
#                                minImgSegm = 10, verbose = FALSE)
# 
#   expect_is(calDataObj, "SOptim.CalData")
#   expect_equal(names(calDataObj), c("calData", "classifFeatData"))
#   expect_is(calDataObj$calData, "data.frame")
#   expect_is(calDataObj$classifFeatData, "data.frame")
# 
#   
#   cl <- calibrateClassifier(calData = calDataObj,
#                             classificationMethod = "KNN",
#                             balanceTrainData = FALSE,
#                             evalMethod = "5FCV",
#                             evalMetric = "Kappa",
#                             minTrainCases = 5,
#                             minCasesByClassTrain = 5,
#                             minCasesByClassTest = 5,
#                             runFullCalibration = TRUE,
#                             verbose = FALSE)
# 
#   expect_is(cl,"SOptim.Classifier")
# 
#   predSegms <- predictSegments(classifierObj  = cl,
#                                calData       = calDataObj,
#                                rstSegm       = rstSegm,
#                                predictFor    = "all",
#                                filename      = NULL,
#                                verbose       = FALSE,
#                                na.rm         = TRUE)
# 
#   expect_is(predSegms,"RasterLayer")
#   expect_equal(cellStats(predSegms,"min"), 1)
#   expect_equal(cellStats(predSegms,"max"), 3)
# 
# })

test_that("Test predictSegments for FDA classifier (multi-class)",{

  rstSegm <- simRasterSegments2()

  # Craft perfectly separable data
  #
  DF <- rbind(data.frame(SID   = 1:250,
                         train = 1,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              data.frame(SID   = 251:500,
                         train = 2,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5)),
              data.frame(SID   = 501:750,
                         train = 3,
                         v1 = rnorm(250,1000),
                         v2 = rnorm(250,500),
                         v3 = rnorm(250,1300))
  )

  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "multi-class"
  class(calDataObj) <- "SOptim.CalData"

  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "FDA",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  predSegms <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE)

  expect_is(predSegms,"RasterLayer")
  expect_equal(cellStats(predSegms,"min"), 1)
  expect_equal(cellStats(predSegms,"max"), 3)

})


## ---------------------------------------------------------------------- ##
## Test for memory-safe ops
## ---------------------------------------------------------------------- ##


test_that("Test predictSegments for RF classifier (single-class) with memory-safe ops",{

  rstSegm <- simRasterSegments2()
  # rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  # rstFeat <- simRasterFeatures()

  # calDataObj <- prepareCalData(rstSegm, rstTrain, rstFeat, funs = "mean",
  #                              minImgSegm = 10, verbose = FALSE)
  # 
  # expect_is(calDataObj, "SOptim.CalData")
  # expect_equal(names(calDataObj), c("calData", "classifFeatData"))
  # expect_is(calDataObj$calData, "data.frame")
  # expect_is(calDataObj$classifFeatData, "data.frame")

  DF <- rbind(data.frame(SID   = 1:250,
                         train = 0,
                         v1 = rnorm(250,10),
                         v2 = rnorm(250,100),
                         v3 = rnorm(250,20)),
              
              data.frame(SID   = 251:500,
                         train = 1,
                         v1 = rnorm(250,100),
                         v2 = rnorm(250,200),
                         v3 = rnorm(250,5))
  )
  
  calDataObj <- list(calData = DF, classifFeatData = DF)
  attr(calDataObj, "nClassType") <- "single-class"
  class(calDataObj) <- "SOptim.CalData"
  
  
  cl <- calibrateClassifier(calData = calDataObj,
                            classificationMethod = "RF",
                            balanceTrainData = FALSE,
                            evalMethod = "5FCV",
                            evalMetric = "Kappa",
                            minTrainCases = 5,
                            minCasesByClassTrain = 5,
                            minCasesByClassTest = 5,
                            runFullCalibration = TRUE,
                            verbose = FALSE)

  expect_is(cl,"SOptim.Classifier")

  outRstFile <- paste(tempfile(),".tif",sep="")

  # Use by row IO ops
  # Generate error if forceWriteByLine = TRUE) & filename = NULL
  #
  expect_error(predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE,
                               forceWriteByLine = TRUE))

  # Use by row IO ops
  predSegms1 <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = outRstFile,
                               verbose       = FALSE,
                               na.rm         = TRUE,
                               forceWriteByLine = TRUE)
  # Use all at once ops
  predSegms2 <- predictSegments(classifierObj  = cl,
                               calData       = calDataObj,
                               rstSegm       = rstSegm,
                               predictFor    = "all",
                               filename      = NULL,
                               verbose       = FALSE,
                               na.rm         = TRUE,
                               forceWriteByLine = FALSE)

  expect_is(predSegms1,"RasterLayer")
  expect_equal(cellStats(predSegms1,"min"), 0)
  expect_equal(cellStats(predSegms1,"max"), 1)

  expect_is(predSegms2,"RasterLayer")
  expect_equal(cellStats(predSegms2,"min"), 0)
  expect_equal(cellStats(predSegms2,"max"), 1)

  expect_true(compareRaster(predSegms1, predSegms2, values = TRUE))

})



