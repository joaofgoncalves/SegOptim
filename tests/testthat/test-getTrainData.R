
context("Functions for generating single and multi-class training data")

library(SegOptim)
library(dplyr)
library(dtplyr)
library(raster)


## ----------------------------------------------------------------------------------- ##
## Single-class tests ----
## ----------------------------------------------------------------------------------- ##


test_that("Expect correct outputs from getTrainData with single-class/binary inputs", {
  
  # Make binary train data
  x <- raster(matrix(c(rep(0,5000), 
                       sample(c(0,1), size = 5000, replace = TRUE, prob = c(0.4, 0.6))), 
                     nrow = 100, ncol = 100, byrow = TRUE))
  
  # Make a test segmented raster
  rstSegm <- x
  values(rstSegm) <- rep(1:50, each=200)
  
  res <- getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.3, na.rm=TRUE, 
                      dup.rm=TRUE, minImgSegm=30, ignore=FALSE) 
  
  expect_equal(colnames(res), c("SID","train"))
  expect_equal(dim(res), c(50, 2))
  
})


test_that("Train data generation handles NA's properly", {
  
  # Make binary train data with NA values
  x <- raster(matrix(c(rep(0,2500), rep(NA,2500), 
                       sample(c(0,1), size = 5000, replace = TRUE, prob = c(0.4, 0.6))), 
                     nrow = 100, ncol = 100, byrow = TRUE))
  
  # Make a test segmented raster
  rstSegm <- x
  values(rstSegm) <- rep(1:50, each=200)
  
  res <- getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.1, na.rm=TRUE, 
                      dup.rm=TRUE, minImgSegm=30, ignore=FALSE)
  
  expect_is(res, "data.frame")
})


test_that("Expect error in train data because one of the classes has no train cases", {
  
  # Make binary train data
  x <- raster(matrix(c(rep(0,5000), 
                       sample(c(0,1), size = 5000, replace = TRUE, prob = c(0.4, 0.6))), 
                     nrow = 100, ncol = 100, byrow = TRUE))
  
  # Make a test segmented raster
  rstSegm <- x
  values(rstSegm) <- sample(rep(1:50, each=200))
  
  # Expect warning when ignore = TRUE
  expect_warning(getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.3, na.rm=TRUE, 
                                             dup.rm=TRUE, minImgSegm=30, ignore=TRUE))
  
  expect_error(getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.3, na.rm=TRUE, 
                              dup.rm=TRUE, minImgSegm=30, ignore=FALSE))
  
  # Expect NA output when ignore = TRUE
  expect_equal(suppressWarnings(getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.3, na.rm=TRUE, 
                              dup.rm=TRUE, minImgSegm=30, ignore=TRUE)),NA)
})

test_that("Expect NA output from getTrainData due to low minImgSegm", {
  
  x <- raster(matrix(sample(c(0,1),10000,replace=TRUE, prob = c(0.25,0.75)), 
                     nrow = 100, ncol = 100))
  rstSegm <- x
  values(rstSegm) <- sample(rep(1:50,each=200),10000)
  
  expect_warning(getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, 
                              dup.rm=TRUE, minImgSegm=100, ignore=TRUE))
  
  expect_equal(suppressWarnings(getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, 
                              dup.rm=TRUE, minImgSegm=100, ignore=TRUE)), NA)
  
})

test_that("Test getTrainData function",{
  
  
  rstSegm <- simRasterSegments2()
  rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  
  trainDF <- getTrainData(rstTrain, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                          dup.rm = TRUE, minImgSegm = 5, ignore = FALSE)
  
  expect_is(trainDF, "data.frame")
  expect_equal(colnames(trainDF),c("SID","train"))
  expect_equal(is.integer(trainDF$SID),TRUE)
  expect_equal(length(unique(trainDF$train)),2)
  
})


## ----------------------------------------------------------------------------------- ##
## Multi-class tests ----
## ----------------------------------------------------------------------------------- ##

test_that("Expect correct outputs from getTrainData with multi-class inputs", {
  
  # Make binary train data
  x <- raster(matrix(sample(1:3,10000,replace=TRUE), nrow = 100, ncol = 100))
  
  # Make a test segmented raster
  rstSegm <- x
  values(rstSegm) <- rep(1:10, each=1000)
  
  res <- getTrainData(x, rstSegm, useThresh=TRUE, thresh=0.1, na.rm=TRUE, 
                      dup.rm=TRUE, minImgSegm=5, ignore=FALSE) 
  
  #print(res)
  expect_equal(colnames(res),c("SID", "train"))
  expect_equal(dim(res),c(10,2))
  
})

