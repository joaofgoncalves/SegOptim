
context("Test segmentation functions")

library(SegOptim)
library(raster)


## ESRI ArcGIS segmentation test ----

test_that("Test if ESRI ArcGIS segmentation is performed correctly",{

  source("_CONFIG_.R")

  skip_if(!dir.exists(ARCGIS_PYTHON_PATH), "Cannot find ArcGIS python path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")

  segObj <- segmentation_ArcGIS_MShift(inputRstPath    = S1_SEGM_FEAT_PATH,
                                       outputSegmRst   = "test_arcgis.tif",
                                       SpectralDetail  = 10,
                                       SpatialDetail   = 8,
                                       MinSegmentSize  = 20,
                                       pythonPath      = ARCGIS_PYTHON_PATH,
                                       verbose         = TRUE)
  
  expect_is(raster("test_arcgis.tif"), "RasterLayer")
  file.remove(list.files(pattern="testRasterFeatures_SegmentMe_|testRasterFeatures_SegmentMe1|_interIndex|_Segment1"))
  doCleanUpActions(c(unlist(segObj),
                     paste(tools::file_path_sans_ext(segObj$Segmentation),"_0_0_FINAL.tif",sep="")))
}
)

test_that("Test if ESRI ArcGIS segmentation is performed correctly #2",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(ARCGIS_PYTHON_PATH), "Cannot find ArcGIS python path")
  #skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  rst <- simRasterFeatures(nr = 10, nc = 10, nlyrs=3)
  writeRaster(rst, filename = "testRasterFeatures.tif", overwrite=TRUE)
  
  segObj <- segmentation_ArcGIS_MShift(inputRstPath    = "testRasterFeatures.tif",
                                       outputSegmRst   = "test_arcgis.tif",
                                       SpectralDetail  = 10,
                                       SpatialDetail   = 8,
                                       MinSegmentSize  = 20,
                                       pythonPath      = ARCGIS_PYTHON_PATH,
                                       verbose         = TRUE)

  expect_is(raster("test_arcgis.tif"), "RasterLayer")
  file.remove(list.files(pattern="testRasterFeatures_SegmentMe_|testRasterFeatures_SegmentMe1|_interIndex|_Segment1"))
  file.remove("testRasterFeatures.tif")
  doCleanUpActions(c(unlist(segObj),
                     paste(tools::file_path_sans_ext(segObj$Segmentation),"_0_0_FINAL.tif",sep="")))
}
)


## Orfeo Toolbox Mean-shift segmentation test ----

test_that("Test if OTB segmentation is performed correctly",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(OTB_BIN_PATH), "Cannot find OTB path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  segObj <- segmentation_OTB_LSMS(inputRstPath  = S1_SEGM_FEAT_PATH, 
                                  outputSegmRst = "segm_test_otb1.tif", 
                                  SpectralRange = 17.3, 
                                  SpatialRange  = 16.64, 
                                  MinSize       = 64, 
                                  lsms_maxiter  = 5, 
                                  tilesizex     = 5000, 
                                  tilesizey     = 5000, 
                                  otbBinPath    = OTB_BIN_PATH, 
                                  RAM           = 4096, 
                                  verbose       = TRUE)
  
  expect_true(file.exists("segm_test_otb1.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segm_test_otb1.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  doCleanUpActions(c(unlist(segObj), 
                     paste(tools::file_path_sans_ext(segObj$Segmentation),"_0_0_FINAL.tif",sep="")))
  }
)


test_that("Test if OTB segmentation (two sets of parameters) is performed correctly",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(OTB_BIN_PATH), "Cannot find OTB path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  segObj <- segmentation_OTB_LSMS2(inputRstPath     = S1_SEGM_FEAT_PATH, 
                                  outputSegmRst     = "segm_test_otb2.tif", 
                                  MS_SpectralRange  = 17.3, 
                                  LSS_SpectralRange = 8.5, 
                                  MS_SpatialRange   = 16.64,
                                  LSS_SpatialRange  = 8.3, 
                                  MinSize           = 64, 
                                  lsms_maxiter = 5, 
                                  tilesizex    = 5000, 
                                  tilesizey    = 5000, 
                                  otbBinPath   = OTB_BIN_PATH, 
                                  RAM          = 4096, 
                                  verbose      = TRUE)
  
  expect_true(file.exists("segm_test_otb2.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segm_test_otb2.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  doCleanUpActions(c(unlist(segObj), 
                     paste(tools::file_path_sans_ext(segObj$Segmentation),"_0_0_FINAL.tif",sep="")))
}
)

test_that("Test if OTB segmentation (two sets of parameters) is performed correctly #2",{
  
  source("_CONFIG_.R")

  skip_if(!dir.exists(OTB_BIN_PATH), "Cannot find OTB path")
  #skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  rst <- simRasterFeatures(nr = 10, nc = 10, nlyrs=3)
  writeRaster(rst, filename = "testRasterFeatures.tif", overwrite=TRUE)
  
  segObj <- segmentation_OTB_LSMS2(inputRstPath = "testRasterFeatures.tif", 
                                   outputSegmRst = "segm_test_otb3.tif", 
                                   MS_SpectralRange = 17.3, 
                                   LSS_SpectralRange = 8.5, 
                                   MS_SpatialRange = 16.64,
                                   LSS_SpatialRange = 8.3, 
                                   MinSize = 64, 
                                   lsms_maxiter = 5, 
                                   tilesizex = 5000, 
                                   tilesizey = 5000, 
                                   otbBinPath = OTB_BIN_PATH, 
                                   RAM = 4096, 
                                   verbose = TRUE)
  
  expect_true(file.exists("segm_test_otb3.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segm_test_otb3.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  file.remove("testRasterFeatures.tif")
  doCleanUpActions(c(unlist(segObj), 
                     paste(tools::file_path_sans_ext(segObj$Segmentation),"_0_0_FINAL.tif",sep="")))
}
)


## SAGA GIS Simple Region Growing Segmentation test -----

test_that("Test if SAGA segmentation is performed correctly",{

  source("_CONFIG_.R")

  skip_if(!dir.exists(SAGA_BIN_PATH), "Cannot find SAGA path")
  skip_if(!file.exists(S1_SEGM_FEAT_SAGA_PATH), "Cannot find input data")

  segObj <- segmentation_SAGA_SRG(rstList             = S1_SEGM_FEAT_SAGA_PATH, 
                                  outputSegmRst       = "segm-test-saga.sgrd", 
                                  Bandwidth           = 25.75,
                                  GaussianWeightingBW = 6.32, 
                                  VarFeatSpace        = 0.763, 
                                  VarPosSpace         = 0.755, 
                                  seedType            = 0, 
                                  method1             = 0,
                                  DWeighting          = 3, 
                                  normalize           = 0, 
                                  neighbour           = 0, 
                                  method2             = 0, 
                                  thresh              = 0, 
                                  leafSize            = 1024,
                                  SAGApath            = SAGA_BIN_PATH, 
                                  verbose             = FALSE)

  expect_true(file.exists("segm-test-saga.sgrd"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segm-test-saga.sdat")
  expect_is(raster(segObj$segm), "RasterLayer")

  doCleanUpActions(unlist(segObj))
  }
)


## TerraLib Mean-region growing image segmentation test ----

test_that("Test if TerraLib/MRG segmentation is performed correctly",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(TERRALIB_BIN_PATH), "Cannot find Terralib path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  segObj <- segmentation_Terralib_MRGrow(inputRstPath  = S1_SEGM_FEAT_PATH, 
                                         outputSegmRst = "segoptim-test-terralibMRG.tif",
                                         Threshold     = 0.007, 
                                         MinSize       = 20, 
                                         verbose       = FALSE,
                                         TerraLib.path = TERRALIB_BIN_PATH)
  
  expect_true(file.exists("segoptim-test-terralibMRG.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-terralibMRG.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  doCleanUpActions(unlist(segObj))
  }
)


test_that("Test if TerraLib/MRG segmentation is performed correctly #2",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(TERRALIB_BIN_PATH), "Cannot find Terralib path")
  #skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  rst <- simRasterFeatures(nr = 10, nc = 10, nlyrs=3)
  writeRaster(rst, filename = "testRasterFeatures.tif", overwrite=TRUE)
  
  segObj <- segmentation_Terralib_MRGrow(inputRstPath  = "testRasterFeatures.tif", 
                                         outputSegmRst = "segoptim-test-terralibMRG.tif",
                                         Threshold     = 0.007, 
                                         MinSize       = 20, 
                                         verbose       = FALSE,
                                         TerraLib.path = TERRALIB_BIN_PATH)
  
  expect_true(file.exists("segoptim-test-terralibMRG.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-terralibMRG.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  file.remove("testRasterFeatures.tif")
  doCleanUpActions(unlist(segObj))
}
)

## TerraLib Baatz multi-resolution image segmentation test ----


test_that("Test if TerraLib/Baatz segmentation is performed correctly",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(TERRALIB_BIN_PATH), "Cannot find Terralib path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  segObj <- segmentation_Terralib_Baatz(inputRstPath      = S1_SEGM_FEAT_PATH, 
                                        outputSegmRst     = "segoptim-test-terralibBaatz.tif",
                                        CompactnessWeight = 0.593, 
                                        SpectralWeight    = 0.563, 
                                        Threshold         = 0.09,  
                                        MinSize           = 56, 
                                        verbose           = FALSE,
                                        TerraLib.path     = TERRALIB_BIN_PATH)
  
  expect_true(file.exists("segoptim-test-terralibBaatz.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-terralibBaatz.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  doCleanUpActions(unlist(segObj))
  }
)

test_that("Test if TerraLib/Baatz segmentation is performed correctly #2",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(TERRALIB_BIN_PATH), "Cannot find Terralib path")
  #skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  rst <- simRasterFeatures(nr = 10, nc = 10, nlyrs=3)
  writeRaster(rst, filename = "testRasterFeatures.tif", overwrite=TRUE)
  
  segObj <- segmentation_Terralib_Baatz(inputRstPath  = "testRasterFeatures.tif", 
                                        outputSegmRst = "segoptim-test-terralibBaatz.tif",
                                        CompactnessWeight = 0.593, 
                                        SpectralWeight    = 0.563, 
                                        Threshold         = 0.09,  
                                        MinSize           = 56, 
                                        verbose           = FALSE,
                                        TerraLib.path     = TERRALIB_BIN_PATH)
  
  expect_true(file.exists("segoptim-test-terralibBaatz.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-terralibBaatz.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  file.remove("testRasterFeatures.tif")
  doCleanUpActions(unlist(segObj))
}
)

## GRASS mean-region growing image segmentation test ----

test_that("Test if GRASS segmentation is performed correctly",{

  source("_CONFIG_.R")

  skip_if(!(file.exists(paste(GRASS_BIN_PATH,".bat",sep="")) ||
              file.exists(GRASS_BIN_PATH)), "Cannot find GRASS executable file")
  #skip_if(!file.exists(), "Cannot find input data")

  tmpOutFile <- paste(tempfile(),".tif",sep="")
  
  segObj <- segmentation_GRASS_RG(GRASS.path         = GRASS_BIN_PATH,
                                  GRASS.inputRstName = S1_SEGM_FEAT_GRASS_PATH,
                                  GRASS.GISDBASE     = GRASS_GISDBASE_PATH,
                                  GRASS.LOCATION_NAME = GRASS_LOCATION_NAME,
                                  GRASS.MAPSET        = "PERMANENT",
                                  outputSegmRst       = tmpOutFile, # "test-segm-GRASS.tif",
                                  Threshold           = 0.45, 
                                  MinSize             = 50, 
                                  memory              = 2046,
                                  iterations          = 30,
                                  verbose             = FALSE)

  #expect_true(file.exists("test-segm-GRASS.tif"))
  expect_true(file.exists(tmpOutFile))
  expect_is(segObj,"SOptim.SegmentationResult")
  #expect_equal(segObj$segm, "test-segm-GRASS.tif")
  expect_equal(segObj$segm, tmpOutFile)
  expect_is(raster(segObj$segm), "RasterLayer")

  doCleanUpActions(unlist(segObj))
  }
)


## RSGISLib Shepherd image segmentation test ----

test_that("Test if RSGISLib/Shepherd segmentation is performed correctly",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(RSGISLIB_PYTHON_PATH), "Cannot find RSGISLib path")
  skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  segObj <- segmentation_RSGISLib_Shep(inputRstPath   = S1_SEGM_FEAT_PATH, 
                                       outputSegmRst  = "segoptim-test-rsgis.tif", 
                                       NumClust       = 6, 
                                       MinSize        = 47, 
                                       SpectralThresh = 72, 
                                       pythonPath     = RSGISLIB_PYTHON_PATH, 
                                       verbose        = TRUE)
  
  expect_true(file.exists("segoptim-test-rsgis.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-rsgis.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  doCleanUpActions(unlist(segObj))
  }
)

test_that("Test if RSGISLib/Shepherd segmentation is performed correctly #2",{
  
  source("_CONFIG_.R")
  
  skip_if(!dir.exists(RSGISLIB_PYTHON_PATH), "Cannot find RSGISLib path")
  #skip_if(!file.exists(S1_SEGM_FEAT_PATH), "Cannot find input data")
  
  rst <- simRasterFeatures(nr = 10, nc = 10, nlyrs=3)
  writeRaster(rst, filename = "testRasterFeatures.tif", overwrite=TRUE)
  
  segObj <- segmentation_RSGISLib_Shep(inputRstPath   = "testRasterFeatures.tif", 
                                       outputSegmRst  = "segoptim-test-rsgis.tif", 
                                       NumClust       = 6, 
                                       MinSize        = 47, 
                                       SpectralThresh = 72, 
                                       pythonPath     = RSGISLIB_PYTHON_PATH, 
                                       verbose        = TRUE)
  
  expect_true(file.exists("segoptim-test-rsgis.tif"))
  expect_is(segObj,"SOptim.SegmentationResult")
  expect_equal(segObj$segm, "segoptim-test-rsgis.tif")
  expect_is(raster(segObj$segm), "RasterLayer")
  
  file.remove("testRasterFeatures.tif")
  doCleanUpActions(unlist(segObj))
}
)




