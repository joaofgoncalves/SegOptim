
context("Test feature extraction functions")

library(SegOptim)
library(terra)


test_that("Test NDI combinations w/ simulated data",{
 
  rst <- c(rast(nrows=100, ncols=100, vals=runif(10000)),
               rast(nrows=100, ncols=100, vals=runif(10000)),
               rast(nrows=100, ncols=100, vals=runif(10000)))
  
  out <- calculateNDIcombinations(rst, bandNames=NULL, subsetBands=NULL, getSpatRaster = TRUE, scale10k=TRUE,
                           filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="SpatRaster")
  
}
)

test_that("Test NDI combos with S1 data",{
  
  source("_CONFIG_.R")
  
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  r <- rast(S1_SEGM_FEAT_PATH)
  
  out <- calculateNDIcombinations(r, bandNames=NULL, subsetBands=NULL, getSpatRaster = TRUE, scale10k=TRUE,
                                  filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="SpatRaster")
})
  

test_that("Test NDI combos with S2 data",{
  
  source("_CONFIG_.R")
  
  skip_if(!file.exists(S2_SEGM_FEAT_PATH), "Cannot find input data")
  
  r <- rast(S2_SEGM_FEAT_PATH)
  
  out <- calculateNDIcombinations(r, bandNames=NULL, subsetBands=NULL, getSpatRaster = TRUE, scale10k=TRUE,
                                  filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="SpatRaster")
})
