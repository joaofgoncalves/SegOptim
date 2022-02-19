
context("Test zonal analysis and segment statistics functions")

library(SegOptim)
library(raster)
library(dplyr)
library(dtplyr)
library(igraph)


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
  
  # Set expectations for the outputs
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
  
  # Set expectations for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 7) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:10)
  
})

test_that("Calculate segment statistics (calculateSegmentStats) with progress bar", {
  
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
  res <- calculateSegmentStats(rstFeatures, rstSegm, funs = c("mean"), 
                               na.rm = TRUE, bylayer = FALSE, subset = NULL, progressBar = TRUE)
  
  # Set expectations for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 4) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:10)
  
})

test_that("Calculate segment statistics (calculateSegmentStats) with subset", {
  
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
  res <- calculateSegmentStats(rstFeatures, rstSegm, funs = c("mean"), 
                               na.rm = TRUE, bylayer = FALSE, subset = 1:5, progressBar = FALSE)
  
  # Set expectations for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 4) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:5)
  
})

test_that("Calculate segment statistics (calculateSegmentStats) with tiles", {
  
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
  
  #rstTiles <- createRasterTiles(rstSegm, 3)
  
  res <- calculateSegmentStats(rstFeatures, rstSegm, funs = c("mean"), tiles = 2,
                               na.rm = TRUE, bylayer = FALSE, subset = NULL, 
                               progressBar = FALSE)
  
  # Set expectations for the outputs
  expect_is(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_equal(ncol(res), 4) # Two columns per raster layer in the stack plus SID
  expect_equal(res$SID, 1:10)
  
})

test_that("Test calculate segments stats (calculateSegmentStats)",{
  
  #source("_CONFIG_.R")
  
  rstSegm <- raster::raster(nrow=100, ncol=100, crs=NA, res=1, 
                            xmn=0, xmx=100, ymn=0, ymx=100)
  values(rstSegm) <- sample(1:500, 10000, replace=TRUE)
  rstFeat  <- simRasterFeatures()
  
  segmStatsDF <- calculateSegmentStats(rstFeat, rstSegm, funs = c("mean", "sd"), na.rm = TRUE, 
                                       bylayer = FALSE, subset = NULL, progressBar = FALSE)
  
  expect_is(segmStatsDF, "data.frame")
  expect_equal(nrow(segmStatsDF), 500)
  expect_equal(ncol(segmStatsDF), 2*nlayers(rstFeat)+1)
  
})

test_that("Test calculate segments stats (calculateSegmentStats) - user defined function",{
  
  #source("_CONFIG_.R")
  
  rstSegm <- raster::raster(nrow=100, ncol=100, crs=NA, res=1, 
                            xmn=0, xmx=100, ymn=0, ymx=100)
  values(rstSegm) <- sample(1:500, 10000, replace=TRUE)
  rstFeat  <- simRasterFeatures()
  
  qt25perc <<- function(x,na.rm=TRUE,...) as.numeric(quantile(x, probs=0.25, na.rm=na.rm))
  #print(is.function(get("qt25")))
  
  segmStatsDF <- calculateSegmentStats(rstFeat, rstSegm, funs = "qt25perc", na.rm = TRUE, 
                                       bylayer = FALSE, subset = NULL, progressBar = FALSE)
  
  expect_is(segmStatsDF, "data.frame")
  expect_equal(nrow(segmStatsDF), 500)
  expect_equal(ncol(segmStatsDF), nlayers(rstFeat)+1)
})

test_that("Test calculateSegmentStats - generate error passing a data.frame!",{
  
  #source("_CONFIG_.R")
  
  rstSegm <- raster::raster(nrow=100, ncol=100, crs=NA, res=1, 
                            xmn=0, xmx=100, ymn=0, ymx=100)
  values(rstSegm) <- sample(1:500, 10000, replace=TRUE)
  rstFeat  <- simRasterFeatures()
  
  # Error in (function (classes, fdef, mtable)  : 
  #             unable to find an inherited method for function 'res' for signature '"numeric"'
  expect_error(
    
    calculateSegmentStats(values(rstFeat), rstSegm, funs = c("mean", "sd"), na.rm = TRUE, 
                          bylayer = FALSE, subset = NULL, progressBar = FALSE)
    
  )
  
})



