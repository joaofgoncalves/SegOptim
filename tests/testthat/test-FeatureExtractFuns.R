
context("Test feature extraction functions")

library(SegOptim)
library(raster)


test_that("Test NDI combinations w/ simulated data",{
 
  rst <- stack(raster(nrows=100, ncols=100, vals=runif(10000)),
               raster(nrows=100, ncols=100, vals=runif(10000)),
               raster(nrows=100, ncols=100, vals=runif(10000)))
  
  out <- calculateNDIcombinations(rst, bandNames=NULL, subsetBands=NULL, getRstStack = TRUE, scale10k=TRUE,
                           filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="RasterStack")
  
}
)

test_that("Test NDI combos with S1 data",{
  
  source("_CONFIG_.R")
  
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  r <- stack(S1_SEGM_FEAT_PATH)
  
  out <- calculateNDIcombinations(r, bandNames=NULL, subsetBands=NULL, getRstStack = TRUE, scale10k=TRUE,
                                  filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="RasterStack")
})
  

test_that("Test NDI combos with S2 data",{
  
  source("_CONFIG_.R")
  
  skip_if(!file.exists(S2_SEGM_FEAT_PATH), "Cannot find input data")
  
  r <- stack(S2_SEGM_FEAT_PATH)
  
  out <- calculateNDIcombinations(r, bandNames=NULL, subsetBands=NULL, getRstStack = TRUE, scale10k=TRUE,
                                  filename=NULL, writeSingleBandRaster=FALSE, verbose=FALSE)
  
  expect_true(class(out)=="RasterStack")
})
