
context("Test data balancing functions")

library(SegOptim)
library(unbalanced)

test_that("Test function dataBalancing", {
  
  DF <- data.frame(SID   = 1:1000, 
                   train = sample(0:1, 1000, replace = TRUE, 
                                  prob = c(0.9, 0.1)),
                   v1 = rnorm(1000),
                   v2 = rnorm(1000),
                   v3 = rnorm(1000))
  
  DF_over <- dataBalancing(DF, method = "ubOver")
  expect_is(DF_over,"data.frame")
  expect_equal(nrow(DF_over) > nrow(DF), TRUE)
  
  DF_under <- dataBalancing(DF, method = "ubUnder")
  expect_is(DF_under,"data.frame")
  expect_equal(nrow(DF_under) < nrow(DF), TRUE)
  
})


test_that("Test balanceMulticlassData function",{
  
  DF <- data.frame(SID   = 1:100, 
                   train = as.factor(sample(1:3, 100, replace = TRUE)),
                   v1 = rnorm(100),
                   v2 = rnorm(100),
                   v3 = rnorm(100))
  
  DF_b <- SegOptim:::balanceMulticlassData(DF, class = "train", n = 20)
    
  expect_is(DF_b, "data.frame")
  expect_equal(nrow(DF_b), 60)
  expect_equal(ncol(DF_b), 5)
  
})