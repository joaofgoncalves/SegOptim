
context("Test user-defined performance/accuracy functions")

library(SegOptim)
library(raster)


test_that("User-defined accuracy function provides correct outputs",{
  
  # source("_CONFIG_.R")
  # 
  # skip_if(!dir.exists(TERRALIB_BIN_PATH), "Cannot find TerraLib path")
  # 
  # segmTLib_MRG <- segmentation_Terralib_MRGrow(inputRstPath=S1_SEGM_FEAT_PATH, 
  #                                              outputSegmRst = "segoptim_test.tif",
  #                              Threshold = 0.007, MinSize = 20, verbose = FALSE,
  #                              TerraLib.path = TERRALIB_BIN_PATH)
  # 
  # expect_true(file.exists("segoptim_test.tif"))
  # expect_is(segmTLib_MRG,"SOptim.SegmentationResult")
  # expect_equal(segmTLib_MRG$segm, "segoptim_test.tif")
  # 
  # segmRst <- raster(segmTLib_MRG$segm)
  # expect_is(segmRst, "RasterLayer")
  # 
  # trainDataRst <- raster(S1_TRAIN_AREAS_PATH)
  # expect_is(trainDataRst, "RasterLayer")
  # 
  # classificationFeatures <- stack(S1_CLASS_FEAT_PATH)
  # expect_is(classificationFeatures, "RasterStack")
  # 
  # calData <- prepareCalData(rstSegm = segmRst,
  #                           trainData = trainDataRst,
  #                           rstFeatures = classificationFeatures,
  #                           thresh = 0.5,
  #                           funs = "mean",
  #                           minImgSegm = 20,
  #                           verbose = FALSE)
  # 
  # head(calData[[1]])
  # head(calData[[2]])
  # 
  # expect_is(calData, "SOptim.CalData")
  # 
  # Modified inputs to use simulated data to test user-defined 
  # performance evaluation function
  
  calData <- list(calData = data.frame(
    train = sample(0:1,1000,replace = TRUE),v1=rnorm(1000),v2=rnorm(1000),v3=rnorm(1000)), 
    classifFeatData = data.frame(v1=rnorm(1000),v2=rnorm(1000),v3=rnorm(1000)))
  class(calData) <- "SOptim.CalData"
  
  
  accuracy <- function(obs, pred){

    threshs <- seq(0.025, 1, by = 0.025)
    accs <- vector(mode="numeric",length = length(threshs))

    i <- 0
    for(thresh in threshs){
      i<-i+1
      confMat <- generateConfusionMatrix(obs, as.integer(pred >= thresh))
      accs[i] <- sum(diag(confMat)) / sum(confMat)
    }
    #print(accs)
    out <- max(accs)
    attr(out,which = "thresh") <- threshs[which.max(accs)]
    return(out)
  }

  classifObj <- calibrateClassifier( calData = calData,
                                     classificationMethod = "RF",
                                     balanceTrainData = FALSE,
                                     balanceMethod = "ubOver",
                                     evalMethod = "5FCV",
                                     evalMetric = accuracy,
                                     minTrainCases = 20,
                                     minCasesByClassTrain = 10,
                                     minCasesByClassTest = 5,
                                     runFullCalibration = TRUE,
                                     verbose = FALSE)

  expect_is(classifObj, "SOptim.Classifier")

})

