

context("Test data generation/simulation functions")

library(SegOptim)
library(terra)
#library(igraph)


test_that("Simulate raster train data",{

  expect_is(simRasterTrain(),"SpatRaster")  
  
})


# test_that("Simulate raster segments data",{
#   
#   expect_is(simRasterSegments(),"RasterLayer")  
#   
# })


test_that("Simulate raster segments data",{
  
  expect_is(simRasterSegments2(),"SpatRaster")  
  
})


test_that("Simulate raster features data",{
  
  expect_is(simRasterFeatures(),"SpatRaster")  
  
})


test_that("Simulate raster features data",{
  
  expect_is(simRasterFeatures(distr = "runif", 
                              args = list(n=10000, 0, 1)),"SpatRaster")  
  
})

