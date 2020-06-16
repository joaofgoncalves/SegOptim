
context("Test raster tiling system")

library(SegOptim)
library(raster)

test_that("Test raster tile generation (createRasterTiles)",{
  
  rst <- simRasterTrain()
  tiles <- createRasterTiles(rst, nd = 2)
  expect_is(tiles,"SOptim.Tiles")
  expect_is(tiles[[1]],"SOptim.Tile")
  expect_equal(length(tiles), 4)
})

test_that("Test data reading by tile",{
  
  r1 <- raster(ncol=10,nrow=10)
  values(r1) <- rnorm(100)
  r2 <- raster(ncol=10,nrow=10)
  values(r2) <- rnorm(100)
  r3 <- raster(ncol=10,nrow=10)
  values(r3) <- rnorm(100)
  r <- stack(r1, r2, r3)

  rtiles <- createRasterTiles(r, nd = 3)
  
  outDF <- readDataByTile(r, rtiles, nd = 3, tileIndex = 1, as.df = TRUE)
  out <- readDataByTile(r, rtiles, nd = 3, tileIndex = 1, as.df = FALSE)
  
  expect_is(outDF, "data.frame")
  expect_is(out, "matrix")
  expect_true(ncol(outDF) < ncell(r))
  expect_true(ncol(out) < ncell(r))
  
})