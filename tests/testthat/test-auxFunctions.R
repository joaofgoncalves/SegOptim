
context("Test ancillary functions")

library(SegOptim)
library(raster)

test_that("Check conversion from factor to integer (f2int)",{
  
  #f2int<-function(x) (1:length(levels(x)))[as.integer(x)]
  f1 <- factor(letters[1:10])
  expect_equal(f2int(f1),1:10)
  
  f2 <- factor(c("Apples","Oranges","Tomatoes","Grapes"))
  expect_equal(f2int(f2),c(1,3,4,2))
  
  f3 <- factor(letters[1:(sample(2:26,1))])
  expect_equal(f2int(f3),1:length(f3))
})


test_that("Check removal of NA values (NRV.omit)", {
  
  x <- c(NA,1,NA,2,NA,3)
  expect_equal(NRV.omit(x),1:3)
  
  x <- matrix(c(rep(NA,3),1:3,4:6), nrow=3, ncol=3, byrow = TRUE)
  expect_equal(nrow(NRV.omit(x)), 2)
})


test_that("Python script not found (getPythonFile)", {
  
  expect_null(getPythonFile(pyScriptName = "A_RANDOM_FILE_NAME", pkgName = "SegOptim", 
                            altLocs = getwd()))
})


test_that("Get python scripts if the package is installed (getPythonFile)", {
  
  skip_if_not_installed("SegOptim")
  
  expect_is(getPythonFile(pyScriptName = "ArcGIS_MShift.py", 
                          pkgName = "SegOptim", altLocs = getwd()),"character")

  expect_is(getPythonFile(pyScriptName = "RSGISLib_Shep.py", 
                          pkgName = "SegOptim", altLocs = getwd()),"character")
})


test_that("Check segmentation function parameters with input errors (checkPrintSegmentationParams)",{
  
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "SAGA_SRG"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "GRASS_RG"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "ArcGIS_MShift"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "Terralib_Baatz"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "Terralib_MRGrow"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "RSGISLib_Shep"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "OTB_LSMS"))
  expect_error(checkPrintSegmentationParams(1, segmentMethod = "OTB_LSMS2"))
})


test_that("Check segmentation function parameters with correct number of inputs (checkPrintSegmentationParams)",{
  
  expect_message(checkPrintSegmentationParams(1:4, segmentMethod = "SAGA_SRG"))
  expect_message(checkPrintSegmentationParams(1:2, segmentMethod = "GRASS_RG"))
  expect_message(checkPrintSegmentationParams(1:3, segmentMethod = "ArcGIS_MShift"))
  expect_message(checkPrintSegmentationParams(1:4, segmentMethod = "Terralib_Baatz"))
  expect_message(checkPrintSegmentationParams(1:2, segmentMethod = "Terralib_MRGrow"))
  expect_message(checkPrintSegmentationParams(1:3, segmentMethod = "RSGISLib_Shep"))
  expect_message(checkPrintSegmentationParams(1:3, segmentMethod = "OTB_LSMS"))
  expect_message(checkPrintSegmentationParams(1:5, segmentMethod = "OTB_LSMS2"))
})


test_that("Generate a random string (randString)",{
  
  expect_is(randString(10),"character")
  expect_true(nchar(randString(100))==100)
})


test_that("Back slashes are replaced by forward slashes (repBSlash)",{
  
  expect_equal(repBSlash("a\\b\\c"), "a/b/c")
  expect_equal(repBSlash("a\\\\b\\c"), "a//b/c")
})


test_that("Test file and directory removal with doCleanUpActions",{
  
  # File removal
  x <- matrix(1:100,10,10)
  write.csv(x, "test-file-segoptim.csv")
  expect_true(file.exists("test-file-segoptim.csv"))
  doCleanUpActions(x = "test-file-segoptim.csv", recursive = TRUE, silent = TRUE)
  expect_true(!file.exists("test-file-segoptim.csv"))
  
  # Dir removal
  dir.create("test-dir-segoptim", showWarnings = FALSE)
  expect_true(dir.exists("test-dir-segoptim"))
  doCleanUpActions(x = "test-dir-segoptim", recursive = TRUE, silent = TRUE)
  expect_true(!dir.exists("test-dir-segoptim"))
  
})

test_that("Test calculate segments stats (calculateSegmentStats)",{
  
  #source("_CONFIG_.R")
  
  #rstSegm  <- simRasterSegments()
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
  
  #rstSegm  <- simRasterSegments()
  rstSegm <- raster::raster(nrow=100, ncol=100, crs=NA, res=1, 
                            xmn=0, xmx=100, ymn=0, ymx=100)
  values(rstSegm) <- sample(1:500, 10000, replace=TRUE)
  rstFeat  <- simRasterFeatures()
  
  qt25 <- function(x,na.rm=TRUE,...) as.numeric(quantile(x, probs=0.25, na.rm=na.rm))
  
  segmStatsDF <- calculateSegmentStats(rstFeat, rstSegm, funs = "qt25", na.rm = TRUE, 
                                       bylayer = FALSE, subset = NULL, progressBar = FALSE)
  
  expect_is(segmStatsDF, "data.frame")
  expect_equal(nrow(segmStatsDF), 500)
  expect_equal(ncol(segmStatsDF), nlayers(rstFeat)+1)
})

test_that("Test calculateSegmentStats - generate error passing a data.frame!",{
  
  #source("_CONFIG_.R")
  
  #rstSegm  <- simRasterSegments()
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

test_that("Test getTrainData function",{
  
  
  rstSegm <- simRasterSegments()
  rstTrain <- simRasterTrain(probs = c(0.4,0.4,0.2))
  
  # par(mfrow=c(1,2))
  # plot(rstSegm)
  # plot(rstTrain, col=rainbow(3))
  
  trainDF <- getTrainData(rstTrain, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                   dup.rm = TRUE, minImgSegm = 5, ignore = FALSE)
  
  expect_is(train_DF, "data.frame")
  expect_equal(colnames(trainDF),c("SID","train"))
  expect_equal(is.integer(trainDF$SID),TRUE)
  expect_equal(length(unique(trainDF$train)),2)
  
})




