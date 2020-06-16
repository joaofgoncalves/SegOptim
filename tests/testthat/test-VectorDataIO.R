
context("Vector Data I/O testing")

library(SegOptim)
library(raster)
library(sf)


test_that("Test gdal_polygonizeR function for vectorization of raster data",{

  source("_CONFIG_.R")
    
  outFile <- tempdir()
  skip_if(file.access(outFile, mode=2) != 0)
  
  skip_if(!file.exists(pyGDALpolygonize))
  skip_if(!dir.exists(pythonPath))
  
  r <- raster::raster(nrow = 10, ncol = 10)
  values(r) <- sample(x = 1:5, size = 100, replace=TRUE)
  
  pols <- SegOptim:::gdal_polygonizeR(r, outFile, pyGDALpolygonize,
                   pythonPath,TRUE,verbose=FALSE)
  
  expect_is(pols, c("sf", "data.frame"))
  expect_equal(all(sf::st_geometry_type(pols)=="POLYGON"), TRUE)
})
