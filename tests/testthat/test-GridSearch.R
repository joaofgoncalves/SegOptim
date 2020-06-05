
context("Tests for grid search optimization systematic and random")

library(SegOptim)
library(raster)

test_that("Test random search optimization", {
  
  
  source("_CONFIG_.R")
  skip_if(!dir.exists(OTB_BIN_PATH), "Cannot find OTB path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input segm feature data")
  skip_if(!file.exists(S1_CLASS_FEAT_PATH), "Cannot find input classification features data")
  skip_if(!file.exists(S1_TRAIN_AREAS_PATH), "Cannot find input train data")
  
  bestParams <- searchOptimSegmentationParams(
    S1_CLASS_FEAT_PATH,
    S1_TRAIN_AREAS_PATH,            
    
    "OTB_LSMS",         
    otbBinPath = OTB_BIN_PATH,
    inputRstPath = S1_SEGM_FEAT_PATH,
    
    segmParamList = list(
      SpectralRange = c(15, 20),
      SpatialRange  = c(15, 20),
      MinSize       = c(30,65)),
    
    optimMethod = "random",
    rand.numIter = 1, 
    rand.nneigh = 1, 
    rand.initNeighs = (2 * rand.nneigh), 
    rand.neighSizeProp = 0.025, 
    rand.iter = 1,
    trainThresh = 0.5,
    segmStatsFuns = "mean",
    classificationMethod = "RF",
    classificationMethodParams = NULL,
    balanceTrainData = FALSE,
    balanceMethod = "ubUnder",
    evalMethod = "5FCV",
    trainPerc = 0.8,
    evalMetric = "Kappa",
    minTrainCases = 10,
    minCasesByClassTrain = 5,
    minCasesByClassTest = 5,
    minImgSegm = 10,
    verbose = FALSE,
    parallel = FALSE,
    seed = NULL)
  
  expect_is(bestParams, "list")
  expect_equal(names(bestParams), c("bestFitValue", "bestParams"))
  
})


test_that("Test grid search optimization", {
  
  
  source("_CONFIG_.R")
  skip_if(!dir.exists(OTB_BIN_PATH), "Cannot find OTB path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input segm feature data")
  skip_if(!file.exists(S1_CLASS_FEAT_PATH), "Cannot find input classification features data")
  skip_if(!file.exists(S1_TRAIN_AREAS_PATH), "Cannot find input train data")
  
  bestParams <- searchOptimSegmentationParams(
    S1_CLASS_FEAT_PATH,
    S1_TRAIN_AREAS_PATH,            
    
    "OTB_LSMS",         
    otbBinPath = OTB_BIN_PATH,
    inputRstPath = S1_SEGM_FEAT_PATH,
    
    segmParamList = list(
      SpectralRange = c(15, 20),
      SpatialRange  = c(15, 20),
      MinSize       = c(30,65)),
    
    optimMethod = "grid",
    grid.searchSize = 1,
    trainThresh = 0.5,
    segmStatsFuns = "mean",
    classificationMethod = "RF",
    classificationMethodParams = NULL,
    balanceTrainData = FALSE,
    balanceMethod = "ubUnder",
    evalMethod = "5FCV",
    trainPerc = 0.8,
    evalMetric = "Kappa",
    minTrainCases = 10,
    minCasesByClassTrain = 5,
    minCasesByClassTest = 5,
    minImgSegm = 10,
    verbose = FALSE,
    parallel = FALSE,
    seed = NULL)
  
  expect_is(bestParams, "data.frame")
  #expect_equal(names(bestParams), c("bestFitValue", "bestParams"))
  
})




