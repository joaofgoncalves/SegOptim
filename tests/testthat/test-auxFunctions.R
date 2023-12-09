
context("Test ancillary functions")

library(SegOptim)
library(terra)
library(dplyr)

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

