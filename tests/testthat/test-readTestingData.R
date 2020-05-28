

context("Read test data")

library(SegOptim)
library(raster)


test_that("Read data for single-class tests",{
  
  skip_if(!dir.exists("./data"))
  
  r1 <- raster("./data/S1_sc/trainAreas.tif")
  expect_is(r1, "RasterLayer")
  
  r2 <- stack("./data/S1_sc/classificationFeatures.tif")
  expect_is(r2, "RasterStack")
  
  r3 <- stack("./data/S1_sc/segmentationFeatures.tif")
  expect_is(r3, "RasterStack")
  
})

test_that("Read data for multi-class tests",{
  
  skip_if(!dir.exists("./data"))
  
  r1 <- raster("./data/S2_mc/trainAreas.tif")
  expect_is(r1, "RasterLayer")
  
  r2 <- stack("./data/S2_mc/classificationFeatures.tif")
  expect_is(r2, "RasterStack")
  
  r3 <- stack("./data/S2_mc/segmentationFeatures.tif")
  expect_is(r3, "RasterStack")
  
})