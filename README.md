# SegOptim

![SegOptim logo](./man/figures/logo.png)

<!-- badges: start -->

[![R build status](https://github.com/joaofgoncalves/SegOptim/workflows/R-CMD-check/badge.svg)](https://github.com/joaofgoncalves/SegOptim/actions) [![codecov](https://codecov.io/gh/joaofgoncalves/SegOptim/branch/master/graph/badge.svg)](https://codecov.io/gh/joaofgoncalves/SegOptim)

<!-- badges: end -->

------------------------------------------------------------------------

## Package description

*SegOptim* is an R package for object-based image analysis (OBIA). It allows to run, compare and optimize multiple image segmentation algorithms in the context of supervised classification. It also allows to perform unsupervised classification with several different methods and compare them through internal clustering metrics.

-   For more details about installation and how to use the package go to the **tutorial** [here](https://segoptim.bitbucket.io/docs/).

-   Check also the **paper** describing the package functionalities [here](https://www.sciencedirect.com/science/article/pii/S0303243418303556).

-   For sorting out technical issues, contact us through the dedicated Google group [here](https://groups.google.com/forum/#!forum/segoptim-user-group).

## Installing the package

``` r

# [!] Due to the removal from CRAN of package unbalanced
# this has to be compiled from source and installed

remotes::install_github("dalpozz/unbalanced")


# Now let's install SegOptim (with the latest updates)

remotes::install_github("joaofgoncalves/SegOptim")
```

## Compatibility issues

As of December 2023 (version 0.3.0), `SegOptim` moved to the `terra` package as a support for raster data due to the retirement of `sp`, `rgeos`, `rgdal`, `maptools` and, `raster` packages.

For this reason, older scripts (prior to v-0.3.0) will not be compatible with `SegOptim`'s current version and need to be updated. Sorry for any inconvenience this may cause.

## Running SegOptim

``` r

# Path to train data
train_data_path <- "TrainAreas.tif"

# Path to the raster used in segmentation and as features for training
features_path <- "WV2_SpectralBands.tif"

# Path to Orfeo Toolbox binaries (used to perform image segmentation)
otb_path <- "OTB/OTB_720/bin"


## STEP 1 ------------------------------------------------ ##
## Run the image segmentation in OTB
## ------------------------------------------------------- ##

out_segm_obj <- segmentation_OTB_LSMS(
  inputRstPath  = features_path, 
  SpectralRange = 3.1, 
  SpatialRange  = 4.5, 
  MinSize       = 21,
  lsms_maxiter  = 50,
  outputSegmRst = "segmRaster.tif",
  verbose       = TRUE,
  otbBinPath    = otbPath)


# Load the segmented raster and plot it
segm_rst <- rast(out_segm_obj$segm)
plot(segm_rst)

# Load train data
train_data_rst <- rast(train_data_path)

# Classification features
class_feat <- rast(features_path) 


## STEP 2 ------------------------------------------------ ##
## Prepare the calibration data
#
# The dataset includes the segment ID's, training samples/ 
# labels, and features obtained through segmentation statistics 
# (only the mean is calculated in this example)
## ------------------------------------------------------- ##

cal_data <- prepareCalData(rstSegm = segm_rst, 
                          trainData = train_data_rst, 
                          rstFeatures = class_feat, 
                          thresh = 0.5, 
                          funs = "mean", 
                          minImgSegm = 30, 
                          verbose = TRUE)


## STEP 3 ------------------------------------------------ ##
## Train/calibrate the Random Forest (RF) classifier
#
# 10-fold CV will be used for evaluation
# Over-sampling of the minority class applied to balance the 
# training dataset
# The runFullCalibration = TRUE adds a final training round 
# with all the data
## ------------------------------------------------------- ##

rf_trained <- calibrateClassifier( 
  calData = cal_data,
  classificationMethod       = "RF",
  balanceTrainData           = FALSE,
  balanceMethod              = "ubOver",
  evalMethod                 = "10FCV",
  evalMetric                 = "Kappa",
  minTrainCases              = 30,
  minCasesByClassTrain       = 10,
  minCasesByClassTest        = 5,
  runFullCalibration         = TRUE)

# Get more evaluation measures:
# AUC, Peirce Skill score (PSS), Gerrity Skill (PSS) score 
# and Cohen Kappa
evalMatrix <- evalPerformanceClassifier(rf_trained)

# Print the evaluation matrix (one line each CV round plus a 
# "FULL" round with all the data)
print(round(evalMatrix,2))


## STEP 4 ------------------------------------------------ ##
## Predict for the entire image
## ------------------------------------------------------- ##

pred_segm_rst <- predictSegments(classifierObj = rf_trained, 
                                 calData = cal_data, 
                                 rstSegm = segm_rst, 
                                 predictFor = "all", 
                                 filename = "rst_classified.tif")

# Check the classified output
plot(pred_segm_rst)

# Check feature importance ranking/scores from Random Forest
varImpPlot(rf_trained$ClassObj$FULL)
```

## Functionalities

Currently the package offers several functionalities, namely:

-   Run different image segmentation algorithms;

-   Populate image segments with aggregate statistics (using pre- and/or user-defined functions;

-   Perform object-based supervised classification with several different methods;

-   Evaluate classification performance for single- or multi-class problems;

-   Optimize image segmentation parameters using Genetic Algorithms (GA) and other methods;

-   Compare different algorithms based on optimized solutions;

-   Perform unsupervised classification with several methods and compare the results using internal clustering criteria.

## Available algorithms

### Image segmentation algorithms

*SegOptim* allows comparing multiple algorithms both for image segmentation, supervised and unsupervised classification.

Currently, the following methods are available for **image segmentation**:

-   **ArcGIS** Mean-shift ([link](http://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/segment-mean-shift.htm));

-   **GRASS GIS** Region Growing ([link](https://grass.osgeo.org/grass73/manuals/i.segment.html));

-   **Orfeo ToolBox (OTB)** Large-scale Mean-shift ([link](https://www.orfeo-toolbox.org/CookBook/Applications/app_MeanShiftSmoothing.html));

-   **RSGISLib** Shepherd's k-means ([link](http://www.rsgislib.org/rsgislib_segmentation.html));

-   **SAGA GIS** Seeded Region Growing ([link](http://www.saga-gis.org/saga_tool_doc/4.0.1/imagery_segmentation_3.html)).

-   **TerraLib 5** Baatz-Schape Multi-resolution segmentation and Mean Region Growing ([link](http://www.dpi.inpe.br/terralib5/wiki/doku.php?id=start))

### Supervised classification algorithms

As for **supervised classification**, the following methods are available through *R*:

-   Flexible Discriminant Analysis (**FDA**) ([link](https://CRAN.R-project.org/package=mda));

-   Generalized Boosted Model (**GBM**) ([link](https://CRAN.R-project.org/package=gbm));

-   K-nearest neighbor classifier (**KNN**) ([link](https://CRAN.R-project.org/package=class));

-   Random Forest (**RF**) ([link](https://CRAN.R-project.org/package=randomForest));

-   Support Vector Machines (**SVM**) ([link](https://CRAN.R-project.org/package=e1071)).

### Unsupervised classification algorithms

Regarding **unsupervised classification**, *SegOptim* supports the following algorithms:

-   CLARA (Clustering LARge Applications) ([link](https://CRAN.R-project.org/package=cluster))

-   Hard competitive learning algorithm ([link](https://CRAN.R-project.org/package=cclust))

-   K-means ([link](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html))

-   Neural gas algorithm ([link](https://CRAN.R-project.org/package=cclust))

The support for (internal) clustering criteria used for comparing each unsupervised solution is given by the **clusterCrit** package ([link](https://CRAN.R-project.org/package=clusterCrit)).

## Limitations

Currently, *SegOptim* has several limitations that derive from design decisions that were made during development stages and, to be completely honest, the fact that we are not software developers... (sorry for that ;-). Among other, these limitations are:

-   Memory restrictions (especially for large images);

-   Computational complexity and workload (especially for applying optimization);

-   No pre-processing for satellite data;

-   Only allows single-stage classification;

-   It does not allow to explore hierarchical or spatial relations among or between objects;

-   No post-classification processing;

-   Although tests have been successfully accomplished in several operating systems, Windows is for now the most supported one.

We are planning to add new features and to address some of these limitations in future releases however this is the *status quo*! ;-)

## News

#### version-0.3.0

-   SegOptim moved to the terra package as a support for raster data due to the retirement of `sp`, `rgeos`, `rgdal`, `maptools` and, `raster` packages.

#### version-0.2.4

-   SegOptim now supports the calculation of segment statistics by tiles greatly reducing memory usage

#### version-0.2.3

-   Corrects a bug in the calculation of Normalized Difference Indices

-   Implements the calculation of segment statistics by layer in `prepareCalData()` function

-   Greatly improves the speed of `predictSegments()` function using `data.table` objects

-   Other misc bug fixes and corrections

#### version-0.2.2

-   Corrects a couple of bugs

-   Adds the possibility to define available RAM in OTB segmentation
