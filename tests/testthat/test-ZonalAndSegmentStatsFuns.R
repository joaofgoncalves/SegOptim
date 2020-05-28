
context("Test zonal analysis and segment statistics functions")

library(SegOptim)
library(raster)


test_that("Zonal analysis for matrices (zonalDT)",{
  
  x <- matrix(1:1000,100,10)
  z <- rep(1:5, each=20)
  zs <- zonalDT(x, z, fun = "mean", na.rm = TRUE)
  
  expect_is(zs, c("data.table","data.frame"))
  expect_equal(sum(zs[,1]), 15)
  expect_equal(ncol(zs), 11)
  expect_equal(nrow(zs), 5)
  
})


test_that("Calculate segment statistics (calculateSegmentStats)", {
  
  # Make test raster features data
  r1 <- raster(matrix(rnorm(10000),100,100))
  r2 <- raster(matrix(rnorm(10000),100,100))
  r3 <- raster(matrix(rnorm(10000),100,100))
  rstFeatures <- stack(r1,r2,r3)
  names(rstFeatures) <- paste("R",1:3,sep="")
  
  # Make a test segmented raster
  rstSegm <- rstFeatures[[1]]
  values(rstSegm) <- rep(1:10, each=1000)
  
  # Calculate all
  res <- calculateSegmentStats(rstFeatures, rstSegm, funs = c("mean", "sd"), 
                               na.rm = TRUE, bylayer=FALSE, subset=NULL, progressBar = FALSE)
  
  # Set expectactions for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 7) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:10)
  
})


test_that("Calculate segment statistics (calculateSegmentStats) by layer", {
  
  # Make test raster features data
  r1 <- raster(matrix(rnorm(10000),100,100))
  r2 <- raster(matrix(rnorm(10000),100,100))
  r3 <- raster(matrix(rnorm(10000),100,100))
  rstFeatures <- stack(r1,r2,r3)
  names(rstFeatures) <- paste("R",1:3,sep="")
  
  # Make a test segmented raster
  rstSegm <- r1
  values(rstSegm) <- rep(1:10, each=1000)
  
  # Calculate all
  res <- calculateSegmentStats(rstFeatures, rstSegm, funs = c("median", "mad"), 
                               na.rm = TRUE, bylayer = TRUE, subset = NULL, progressBar = FALSE)
  
  # Set expectactions for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 7) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:10)
  
})


