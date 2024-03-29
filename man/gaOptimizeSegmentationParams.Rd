% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_OptimizationFunctions.R
\name{gaOptimizeSegmentationParams}
\alias{gaOptimizeSegmentationParams}
\title{Optimization of segmentation parameters using genetic algorithms}
\usage{
gaOptimizeSegmentationParams(
  rstFeatures,
  trainData,
  segmentMethod,
  trainThresh = 0.5,
  segmStatsFuns = c("mean", "sd"),
  bylayer = FALSE,
  tiles = NULL,
  classificationMethod = "RF",
  classificationMethodParams = NULL,
  balanceTrainData = TRUE,
  balanceMethod = "ubUnder",
  evalMethod = "5FCV",
  trainPerc = 0.8,
  nRounds = 10,
  evalMetric = "Kappa",
  minTrainCases = 30,
  minCasesByClassTrain = 10,
  minCasesByClassTest = 10,
  minImgSegm = 30,
  ndigits = 2,
  verbose = TRUE,
  ...,
  lower,
  upper,
  population = GA::gaControl("real-valued")$population,
  selection = GA::gaControl("real-valued")$selection,
  crossover = GA::gaControl("real-valued")$crossover,
  mutation = GA::gaControl("real-valued")$mutation,
  popSize = 20,
  pcrossover = 0.8,
  pmutation = 0.1,
  elitism = base::max(1, round(popSize * 0.05)),
  maxiter = 100,
  run = 20,
  maxFitness = 1,
  keepBest = TRUE,
  parallel = FALSE,
  seed = NULL
)
}
\arguments{
\item{rstFeatures}{Features used for supervised classification (typically a multi-layer SpatRaster with one feature 
per band). May be defined as a string with the path to a raster dataset or a \code{RasterStack} object.}

\item{trainData}{Input train data used for supervised classification. It must be a \code{SpatRaster} containing 
train areas (in raster format)}

\item{segmentMethod}{Character string used to define the segmentation method. 
Available options are:     
\itemize{
  \item \code{"SAGA_SRG"} - SAGA Simple Region Growing;
  \item \code{"GRASS_RG"} - GRASS Region Growing;
  \item \code{"ArcGIS_MShift"} - ArcGIS Mean Shift algorithm;
  \item \code{"Terralib_Baatz"} - TerraLib Baatz algorithm;
  \item \code{"Terralib_MRGrow"} - TerraLib Mean Region Growing;
  \item \code{"RSGISLib_Shep"} - RSGISLib Shepherd algorithm;
  \item \code{"OTB_LSMS"} - OTB Large Scale Mean Shift algorithm;
  \item \code{"OTB_LSMS2"} - OTB Large Scale Mean Shift algorithm 
  with two separate sets of parameters, one for mean-shift smoothing 
  and another for large-scale segmentation step;
}}

\item{trainThresh}{A threshold value defining the minimum proportion of the segment ]0, 1] that must be covered 
by a certain class to be considered as a training case. This threshold will only apply if \code{x} is a \code{RasterLayer} 
which means you are using train areas/pixels. 
If you are running a \emph{"single-class"} problem then this threshold only applies to the class of interest (coded as 1's). 
Considering this, if a given segment has a proportion cover of that class higher than \code{thresh} then it is 
considered a train case. In contrast, for the background class (coded as 0's), only segments/objects 
totaly covered by that class are considered as train cases.      
If you are running a \emph{"multi-class"} problem then \code{thresh} is applied differently. First, the train class is 
determined by a majority rule then if that class covers more than the value specified in \code{thresh} this case is 
kept in train data otherwise it will be filtered out. See also \code{useThresh}.}

\item{segmStatsFuns}{An aggregation function (e.g., \code{mean}) applied to the elements within each segment. 
Either a function object or a function name.}

\item{bylayer}{Calculate statistics layer by layer instead of all at once? (slightly 
increases computation time but spares memory load; default: FALSE).}

\item{tiles}{Number of times to slice the SpatRaster across row 
and column direction. The total number of tiles will be given by: 
\eqn{N_{tiles} = nd^{2}}.}

\item{classificationMethod}{An input string defining the classification algorithm to be used. Available options are: 
\code{"RF"} (random forests), \code{"GBM"} (generalized boosted models), \code{"SVM"} (support vector machines), \code{"KNN"} 
(k-nearest neighbour), and, \code{"FDA"} (flexible discriminant analysis).}

\item{classificationMethodParams}{A list object with a customized set of parameters to be used for the classification algorithms 
(default = NULL). See also \link{generateDefaultClassifierParams} to see which parameters can be changed and how to structure the 
list object.}

\item{balanceTrainData}{Defines if data balancing is to be used (only available for single-class problems; default: TRUE).}

\item{balanceMethod}{A character string used to set the data balancing method. Available methods are based on under-sampling 
\code{"ubUnder"} or over-sampling \code{"ubOver"} the target class.}

\item{evalMethod}{A character string defining the evaluation method. The available methods are \code{"10FCV"} (10-fold 
cross-validation; the default), \code{"5FCV"} (5-fold cross-validation), \code{"HOCV"} (holdout cross-validation with 
the training percentage defined by \code{trainPerc} and the number of rounds defined in \code{nRounds}), and, \code{"OOB"} 
(out-of-bag evaluation; only applicable to random forests).}

\item{trainPerc}{A decimal number defining the training proportion (default: 0.8; if \code{"HOCV"} is used).}

\item{nRounds}{Number of training rounds used for holdout cross-validation (default: 20; if \code{"HOCV"} is used).}

\item{evalMetric}{A character string setting the evaluation metric or a function that calculates the performance score 
based on two vectors one for observed and the other for predicted values (see below for more details). 
This option defines the outcome value of the genetic algorithm fitness function and the output of grid or random search 
optimization routines. Check \code{\link{evalPerformanceGeneric}} for available options. When \code{runFullCalibration=TRUE} 
this metric will be calculated however other evaluation metrics can be quantified using \link{evalPerformanceClassifier}.}

\item{minTrainCases}{The minimum number of training cases used for calibration (default: 20). If the number of rows 
in \code{x} is below this number then \code{calibrateClassifier} will not run.}

\item{minCasesByClassTrain}{Minimum number of cases by class for each train data split so that the classifier 
is able to run.}

\item{minCasesByClassTest}{Minimum number of cases by class for each test data split so that the classifier 
is able to run.}

\item{minImgSegm}{Minimum number of image segments/objects necessary to generate train data.}

\item{ndigits}{Number of decimal plates to consider for rounding the fitness function output. For example,
if \code{ndigits=2} then only improvements of 0.01 will be considered by the GA algorithm.}

\item{verbose}{Print output messages? (default: TRUE).}

\item{...}{Additional parameters passed to the segmentation functions that will not be optimized (see also: 
\code{\link{segmentationGeneric}}). It must also contain the input segmentation data (typically a multi-layer 
SpatRaster dataset with one input feature per band) depending one the algorithm selected.}

\item{lower}{a vector of length equal to the decision variables providing the lower bounds of the search space in case of real-valued or permutation encoded optimizations. Formerly this argument was named \code{min}; its usage is allowed but deprecated.}

\item{upper}{a vector of length equal to the decision variables providing the upper bounds of the search space in case of real-valued or permutation encoded optimizations. Formerly this argument was named \code{max}; its usage is allowed but deprecated.}

\item{population}{an R function for randomly generating an initial population. See \code{\link[GA]{ga_Population}} for available functions.}

\item{selection}{an R function performing selection, i.e. a function which generates a new population of individuals from the current population probabilistically according to individual fitness. See \code{\link[GA]{ga_Selection}} for available functions.}

\item{crossover}{an R function performing crossover, i.e. a function which forms offsprings by combining part of the genetic information from their parents. See \code{\link[GA]{ga_Crossover}} for available functions.}

\item{mutation}{an R function performing mutation, i.e. a function which randomly alters the values of some genes in a parent chromosome. See \code{\link[GA]{ga_Mutation}} for available functions.}

\item{popSize}{the population size.}

\item{pcrossover}{the probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8.}

\item{pmutation}{the probability of mutation in a parent chromosome. Usually mutation occurs with a small probability, and by default is set to 0.1.}

\item{elitism}{the number of best fitness individuals to survive at each generation. By default the top 5\% individuals will survive at each iteration.}

\item{maxiter}{the maximum number of iterations to run before the GA search is halted.}

\item{run}{the number of consecutive generations without any improvement in the best fitness value before the GA is stopped.}

\item{maxFitness}{the upper bound on the fitness function after that the GA search is interrupted.}

\item{keepBest}{a logical argument specifying if best solutions at each iteration should be saved in a slot called \code{bestSol}. See \code{\link[GA]{ga-class}}.}

\item{parallel}{
An optional argument which allows to specify if the Genetic Algorithm should be run sequentially or in parallel. 

For a single machine with multiple cores, possible values are: 
\itemize{
  \item a logical value specifying if parallel computing should be used (\code{TRUE}) or not (\code{FALSE}, default) for evaluating the fitness function;
  \item a numerical value which gives the number of cores to employ. By default, this is obtained from the function \code{\link[parallel]{detectCores}}; 
  \item a character string specifying the type of parallelisation to use. This depends on system OS: on Windows OS only \code{"snow"} type functionality is available, while on Unix/Linux/Mac OSX both \code{"snow"} and \code{"multicore"} (default) functionalities are available.
}
In all the cases described above, at the end of the search the cluster is automatically stopped by shutting down the workers.

If a cluster of multiple machines is available, evaluation of the fitness function can be executed in parallel using all, or a subset of, the cores available to the machines belonging to the cluster. However, this option requires more work from the user, who needs to set up and register a parallel back end. 
In this case the cluster must be explicitly stopped with \code{\link[parallel]{stopCluster}}.
}

\item{seed}{an integer value containing the random number generator state. This argument can be used to replicate the results of a GA search. Note that if parallel computing is required, the \pkg{doRNG} package must be installed.}
}
\value{
An object with \pkg{GA} optimization results. See \link[GA]{ga-class} for a description of available 
slots information.
}
\description{
This function makes some data checks and then performs the optimization of segmentation parameters 
using genetic algorithms.
}
\details{
-- INTRODUCTION --
        
Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological 
evolution and natural selection. GAs simulate the evolution of living organisms, where the fittest individuals 
dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, 
crossover and mutation. The GA package is a collection of general purpose functions that provide a flexible 
set of tools for applying a wide range of genetic algorithm methods (from package \pkg{GA}).
By default \pkg{SegOptim} uses genetic algorithm optimization for "real-valued" type, i.e., 
optimization problems where the decision variables (i.e., segmentation parameters) are floating-point 
representations of real numbers.     
        
        
-- INPUT DATA PREPARATION --
     
TODO: ...
       
-- COMPUTING TIME AND COMPLEXITY --
       
Depending on the size of the raster dataset and the amount of segmentation features/layers used, take into 
consideration that running this function may take quite some time!! Therefore it is crucial to use only a 
relevant subset (or subsets) of your data to run this procedure. Also, choosing an appropriate parameterization 
of the genetic algorithm is key to decrease computing time. For example, if \code{popSize} is set to 30 
and \code{maxiter} to 100, then a maximum number of 3000 image segmentation runs would be required 
to stop the optimization! (usually, running the segmentation is the most time-consuming task of the optimization 
procedure). 
However, if \code{run} is set to 20, this means that if that number of iterations records no 
improvement in fitness (i.e., the classification score) then the optimization stops and returns the best set 
of parameters. Bottom-line is that setting this parameters appropriately is fundamental to get good results 
in a admissible ammount of time. 
On another hand, classification algorithms are also working in the background of the fitness function. Using the 
previous example and admitting that we set \code{evalMethod} to "HOCV" and \code{nRounds} to 20 this means that 
classification would run a maximum of \eqn{3000 \times 20 = 60000} times!! So keep this in mind when setting \code{nRounds} 
value or select a more conservative cross-validation method such as 10- or 5-fold CV. The time required for training 
the classifier also depends on the input data thus a segmentation solution with larger number of objects will 
take longer. The number of classification features (or variables) also affects computation time, higher number of 
these will make classification algorithms running slower.
          
 -- SETTING PARAMETERS FOR OPTIMIZATION --
      
TODO: Controlling for 'biased' classification performance due to class inbalance

TODO: Setting appropriate ranges for segmentation algorithms 
 
TODO: Setting appropriate genetic algorithm parametrization
}
\note{
Do not use parallel option when performing image segmentation with OTB LSMS algorithm! Since the software uses a 
parallel implementation, this will probably freeze the system by consuming all CPU resources.
}
\references{
Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 
53(4), 1-37, \url{http://www.jstatsoft.org/v53/i04/}.       
          
Scrucca L. (2016). On some extensions to GA package: hybrid optimisation, parallelisation and 
islands evolution. Submitted to R Journal. Pre-print available at: \url{http://arxiv.org/abs/1605.01931}.
}
\seealso{
\itemize{
  \item \strong{Genetic algorithms}: \code{\link[GA]{ga}}     
  \item \strong{Control parameters in GA}: \code{\link[GA]{gaControl}}     
  \item \strong{Fitness function}: \code{\link{fitFuncGeneric}} (this function controls much of the processes 
  behind the optimization)
}
}
