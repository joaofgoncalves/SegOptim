
context("Test unsupervised clustering functions")

library(SegOptim)
library(terra)
library(cclust)

test_that("Test numSampPerStrata allocator",{
  
  st <- sample(1:5, 1000, replace=TRUE)
  tb <- table(st) / length(st)
  nsps <- numSampPerStrata(propStrata = tb, n=200, minSamp = TRUE, minSizeSet = 10)
  
  expect_is(nsps, "table")
  expect_equal(length(nsps),5)
})


test_that("Test stratified sampling (StRS)",{
  
  
  st <- sample(1:5, 1000, replace=TRUE)
  tb <- table(st) / length(st)
  nsps <- numSampPerStrata(propStrata = tb, n=200, minSamp = TRUE, minSizeSet = 10)
  
  expect_is(nsps, "table")
  expect_equal(length(nsps),5)
  
  strs <- StRS(x=1:1000, strata=st, nsps)
  
  expect_is(strs, "integer")  
  expect_equal(length(strs), sum(nsps))
})


test_that("Test clusteringRaster (kmeans)",{
  
  
  rstFeat <- simRasterFeatures(20,20)
  
  outRstPath <- paste(tempfile(),".tif",sep="")
  
  out <- 
    clusteringRaster(inRst            =  rstFeat, 
                     k                = 2:5, 
                     writeRasterData  = TRUE, 
                     outRst           = outRstPath, 
                     getSpatRaster      = TRUE, 
                     method           = "kmeans", 
                     calcIntCriteria  = TRUE,
                     crit             = c("Silhouette"), 
                     intCritSampSize  = 100, 
                     verbose          = FALSE)
  
  expect_is(out, "SpatRaster")
  expect_equal(global(out[[1]],"min")[1,1], 1)
  expect_equal(global(out[[1]],"max")[1,1], 2)
  expect_equal(global(out[[2]],"min")[1,1], 1)
  expect_equal(global(out[[2]],"max")[1,1], 3)
  expect_equal(global(out[[3]],"min")[1,1], 1)
  expect_equal(global(out[[3]],"max")[1,1], 4)
  expect_equal(global(out[[4]],"min")[1,1], 1)
  expect_equal(global(out[[4]],"max")[1,1], 5)
  
})

test_that("Test clusteringRaster (hardcl)",{
  
  
  rstFeat <- simRasterFeatures(20,20)
  
  outRstPath <- paste(tempfile(),".tif",sep="")
  
  out <- 
    clusteringRaster(inRst            =  rstFeat, 
                     k                = 2:5, 
                     writeRasterData  = TRUE, 
                     outRst           = outRstPath, 
                     getSpatRaster      = TRUE, 
                     method           = "hardcl", 
                     calcIntCriteria  = TRUE,
                     crit             = c("Silhouette"), 
                     intCritSampSize  = 100, 
                     verbose          = FALSE)
  
  expect_is(out, "SpatRaster")
  expect_equal(global(out[[1]],"min")[1,1], 1)
  expect_equal(global(out[[1]],"max")[1,1], 2)
  expect_equal(global(out[[2]],"min")[1,1], 1)
  expect_equal(global(out[[2]],"max")[1,1], 3)
  expect_equal(global(out[[3]],"min")[1,1], 1)
  expect_equal(global(out[[3]],"max")[1,1], 4)
  expect_equal(global(out[[4]],"min")[1,1], 1)
  expect_equal(global(out[[4]],"max")[1,1], 5)
  
})

test_that("Test clusteringRaster (neuralgas)",{
  
  
  rstFeat <- simRasterFeatures(20,20)
  
  outRstPath <- paste(tempfile(),".tif",sep="")
  
  out <- 
    clusteringRaster(inRst            =  rstFeat, 
                     k                = 2:5, 
                     writeRasterData  = TRUE, 
                     outRst           = outRstPath, 
                     getSpatRaster      = TRUE, 
                     method           = "neuralgas", 
                     calcIntCriteria  = TRUE,
                     crit             = c("Silhouette"), 
                     intCritSampSize  = 100, 
                     verbose          = FALSE)
  
  expect_is(out, "SpatRaster")
  expect_equal(global(out[[1]],"min")[1,1], 1)
  expect_equal(global(out[[1]],"max")[1,1], 2)
  expect_equal(global(out[[2]],"min")[1,1], 1)
  expect_equal(global(out[[2]],"max")[1,1], 3)
  expect_equal(global(out[[3]],"min")[1,1], 1)
  expect_equal(global(out[[3]],"max")[1,1], 4)
  expect_equal(global(out[[4]],"min")[1,1], 1)
  expect_equal(global(out[[4]],"max")[1,1], 5)
  
})

test_that("Test clusteringRaster (clara)",{
  
  
  rstFeat <- simRasterFeatures(20,20)
  
  outRstPath <- paste(tempfile(),".tif",sep="")
  
  out <- 
    clusteringRaster(inRst            =  rstFeat, 
                     k                = 2:5, 
                     writeRasterData  = TRUE, 
                     outRst           = outRstPath, 
                     getSpatRaster      = TRUE, 
                     method           = "clara", 
                     calcIntCriteria  = TRUE,
                     crit             = c("Silhouette"), 
                     intCritSampSize  = 100, 
                     verbose          = FALSE)
  
  expect_is(out, "SpatRaster")
  expect_equal(global(out[[1]],"min")[1,1], 1)
  expect_equal(global(out[[1]],"max")[1,1], 2)
  expect_equal(global(out[[2]],"min")[1,1], 1)
  expect_equal(global(out[[2]],"max")[1,1], 3)
  expect_equal(global(out[[3]],"min")[1,1], 1)
  expect_equal(global(out[[3]],"max")[1,1], 4)
  expect_equal(global(out[[4]],"min")[1,1], 1)
  expect_equal(global(out[[4]],"max")[1,1], 5)
  
})