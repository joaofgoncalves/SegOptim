
context("Test functions to check SegOptim parameters validity")

library(SegOptim)

test_that("Test classificationAlgorithms",{
  
  x <- SegOptim:::classificationAlgorithms()
  expect_is(x, "character")
  expect_equal(length(x), 5)
})

test_that("Test segmentationMethods",{
  
  x <- SegOptim:::segmentationMethods()
  expect_is(x, "character")
  expect_equal(length(x), 8)
})

test_that("Test evaluationMetrics_singleClass",{
  
  x <- SegOptim:::evaluationMetrics_singleClass()
  expect_is(x, "character")
  expect_equal(length(x), 4)
})

test_that("Test evaluationMetrics_multiClass",{
  
  x <- SegOptim:::evaluationMetrics_multiClass()
  expect_is(x, "character")
  expect_equal(length(x), 4)
})

test_that("Test dataBalancingMethods",{
  
  x <- SegOptim:::dataBalancingMethods()
  expect_is(x, "character")
  expect_equal(length(x), 2)
})

test_that("Test evaluationMethods",{
  
  x <- SegOptim:::evaluationMethods()
  expect_is(x, "character")
  expect_equal(length(x), 4)
})

test_that("Test classifierParameters (all)",{
  
  x <- SegOptim:::classifierParameters()
  expect_is(x, "character")
})

test_that("Test input parameter verification using doInputVerification()",{
  
  expect_equal(SegOptim:::doInputVerification("RF", what = "clAlgo"), TRUE)
  
  expect_equal(SegOptim:::doInputVerification("OTB_LSMS", what = "segmMethod"), TRUE)
  expect_equal(SegOptim:::doInputVerification("OTB_LSMS2", what = "segmMethod"), TRUE)

  expect_equal(SegOptim:::doInputVerification("AUC", what = "evalMetr_sc"), TRUE)
  expect_equal(SegOptim:::doInputVerification("Accuracy", what = "evalMetr_mc"), TRUE)

  expect_equal(SegOptim:::doInputVerification("ubOver", what = "dataBalancing"), TRUE)
  expect_equal(SegOptim:::doInputVerification("ubUnder", what = "dataBalancing"), TRUE)
  
  expect_equal(SegOptim:::doInputVerification("10FCV", what = "evalMethod"), TRUE)
  expect_equal(SegOptim:::doInputVerification("5FCV", what = "evalMethod"), TRUE)
  expect_equal(SegOptim:::doInputVerification("HOCV", what = "evalMethod"), TRUE)
  
  expect_equal(SegOptim:::doInputVerification("mtry", what = "classifierParams"), TRUE)
  expect_equal(SegOptim:::doInputVerification("ntree", what = "classifierParams"), TRUE)
  
  expect_equal(SegOptim:::doInputVerification("mean", what = "segmStatsFuns"), TRUE)
  expect_equal(SegOptim:::doInputVerification("sd", what = "segmStatsFuns"), TRUE)
  
  expect_equal(SegOptim:::doInputVerification(0.5, what = "trainThresh"), TRUE)
  expect_equal(SegOptim:::doInputVerification(0.5, what = "trainPerc"), TRUE)
  
})
