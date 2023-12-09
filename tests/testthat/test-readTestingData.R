

context("Read test data")

library(SegOptim)
library(terra)


test_that("Read data for single-class tests",{
  
  skip_if(!dir.exists("./data"))
  
  r1 <- rast("./data/S1_sc/trainAreas.tif")
  expect_is(r1, "SpatRaster")
  
  r2 <- rast("./data/S1_sc/classificationFeatures.tif")
  expect_is(r2, "SpatRaster")
  
  r3 <- rast("./data/S1_sc/segmentationFeatures.tif")
  expect_is(r3, "SpatRaster")
  
})

test_that("Read data for multi-class tests",{
  
  skip_if(!dir.exists("./data"))
  
  r1 <- rast("./data/S2_mc/trainAreas.tif")
  expect_is(r1, "SpatRaster")
  
  r2 <- rast("./data/S2_mc/classificationFeatures.tif")
  expect_is(r2, "SpatRaster")
  
  r3 <- rast("./data/S2_mc/segmentationFeatures.tif")
  expect_is(r3, "SpatRaster")
  
})