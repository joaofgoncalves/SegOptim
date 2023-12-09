
context("Test data preparation outside a optimization context")

library(SegOptim)
library(terra)

test_that("Test if ESRI ArcGIS segmentation is performed correctly",{
  
  source("_CONFIG_.R")
 
  skip_if(!file.exists(S1_SEGM_OTB), "Cannot find OTB segmented raster")
  skip_if(!file.exists(S1_TRAIN_AREAS_PATH), "Cannot find input training data")
  skip_if(!file.exists(S1_CLASS_FEAT_PATH), "Cannot find classification features")

  trainAreasRst <- terra::rast(S1_TRAIN_AREAS_PATH)
  
  prepDF <- prepareCalData(rstSegm = S1_SEGM_OTB, 
                 #trainData = trainPoints, 
                 trainData = S1_TRAIN_AREAS_PATH,
                 rstFeatures = S1_CLASS_FEAT_PATH, 
                 thresh = 0.5, 
                 funs = "mean", 
                 minImgSegm = 20, 
                 bylayer = FALSE, 
                 tiles = NULL, 
                 verbose = TRUE, 
                 progressBar = FALSE)
    
    expect_is(prepDF,"SOptim.CalData")
  
  }
)
