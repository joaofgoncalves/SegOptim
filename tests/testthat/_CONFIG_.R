
USER_NAME <- "JG"
OS <- "windows-10"
#OS <- "ubuntu-16.04"

if(USER_NAME=="JG" && OS=="windows-10"){
	
  ## ESRI ArcGIS
	ARCGIS_PYTHON_PATH <- "C:/Python27/ArcGIS10.5"
	
	# GRASS GIS
	#GRASS_BIN_PATH <- "C:/GRASS_72/grass72"
	GRASS_BIN_PATH <- "C:/Program Files/QGIS3121/bin/grass78"
	GRASS_GISDBASE_PATH <- "C:/Users/JG/Documents/grassdata"
	GRASS_LOCATION_NAME <- "TestSites_SOptim"

	# Orfeo ToolBox (OTB)
	#OTB_BIN_PATH <- "C:/OTB/bin"
	OTB_BIN_PATH <- "C:/OTB710/bin"
	
	# RSGISLib via Anaconda / Python 3.5
	RSGISLIB_PYTHON_PATH <- "C:/Anaconda3/envs/py35"
	
	# SAGA GIS
	#SAGA_BIN_PATH <- "C:/SAGA"
	SAGA_BIN_PATH <- "C:/SAGA763"
	
	# TerraLib / TISA mini-build
	TERRALIB_BIN_PATH = "C:/terralib-5.2.1-TISA/lib"
	
	## INPUT DATA
	##
	## Single-class test data
	##
	S1_SEGM_FEAT_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S1_sc/segmentationFeatures.tif"
	S1_SEGM_FEAT_SAGA_PATH <- list.files("C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S1_sc/SAGA",pattern=".sgrd$",full.names = TRUE)
	S1_SEGM_FEAT_GRASS_PATH <- "UAV_Mindelo_RGB_3cm_FID_1"
	S1_CLASS_FEAT_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S1_sc/classificationFeatures.tif"
	S1_TRAIN_AREAS_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S1_sc/trainAreas.tif"
	##
	## Multi-class test data
	S2_SEGM_FEAT_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S2_mc/segmentationFeatures.tif"
	S2_SEGM_FEAT_SAGA_PATH <- list.files("D:/R-dev/SegOptim/tests/testthat/data/S2_mc/SAGA",pattern=".sgrd$",full.names = TRUE)
	S2_SEGM_FEAT_GRASS_PATH <- "Arga01_6cm_WGS84UTM29N_GRD60m_FID0"
	S2_CLASS_FEAT_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S2_mc/classificationFeatures.tif"
	S2_TRAIN_AREAS_PATH <- "C:/MyFiles/R-dev/SegOptim/tests/testthat/data/S2_mc/trainAreas.tif"
}

if(USER_NAME=="JG" && OS=="ubuntu-16.04"){
  
  ## ESRI ArcGIS
  #ARCGIS_PYTHON_PATH <- "C:/Python27/ArcGIS10.3"
  
  # GRASS GIS
  GRASS_BIN_PATH <- "/usr/bin/grass72"
  #GRASS_BIN_PATH <- "C:/GRASS_74/grass74"
  GRASS_GISDBASE_PATH <- "/home/kamo/grassdata"
  GRASS_LOCATION_NAME <- "TestSites_SOptim"
  
  # Orfeo ToolBox (OTB)
  OTB_BIN_PATH <- "/home/kamo/Programs/otb/bin"
  
  # RSGISLib via Anaconda / Python 3.5
  #RSGISLIB_PYTHON_PATH <- "C:/Anaconda3/envs/py35"
  
  # SAGA GIS
  #SAGA_BIN_PATH <- "C:/SAGA"
  SAGA_BIN_PATH <- "/usr/bin"
  
  # TerraLib / TISA mini-build
  TERRALIB_BIN_PATH = "/home/kamo/terralib/build-5.2.1-dev/bin"
  
  ## INPUT DATA
  ##
  ## Single-class test data
  ##
  S1_SEGM_FEAT_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S1_sc/segmentationFeatures.tif"
  S1_SEGM_FEAT_SAGA_PATH <- list.files("/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S1_sc/SAGA",pattern=".sgrd$",full.names = TRUE)
  S1_SEGM_FEAT_GRASS_PATH <- "UAV_Mindelo_RGB_3cm_FID_1"
  S1_CLASS_FEAT_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S1_sc/classificationFeatures.tif"
  S1_TRAIN_AREAS_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S1_sc/trainAreas.tif"
  ##
  ## Multi-class test data
  S2_SEGM_FEAT_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S2_mc/segmentationFeatures.tif"
  S2_SEGM_FEAT_SAGA_PATH <- list.files("/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S2_mc/SAGA",pattern=".sgrd$",full.names = TRUE)
  S2_SEGM_FEAT_GRASS_PATH <- "Arga01_6cm_WGS84UTM29N_GRD60m_FID0"
  S2_CLASS_FEAT_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S2_mc/classificationFeatures.tif"
  S2_TRAIN_AREAS_PATH <- "/media/sf_MyDocs/R-dev/SegOptim/tests/testthat/data/S2_mc/trainAreas.tif"
}
