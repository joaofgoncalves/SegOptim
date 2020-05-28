
#' Simulate raster train data
#' 
#' Uses random data to create a simulated \code{RasterLayer} useful 
#' for testing and debugging code.
#' 
#' @param nr Number of rows for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param classes A vector defining integer train classes (default for 
#' single-class: \code{c(0,1,NA)} but can be used for multi-class; 
#' \code{NA} should always be included to include background nodata).
#' 
#' @param probs A vector defining sampling probabilities for each class to use in 
#' [base::sample()] (default: \code{c(0.1,0.2,0.7)}). Values sum up to one if 
#' not are adjusted by sample function internally.
#' 
#' @return 
#' A \code{RasterLayer} object with random train data.
#' 
#' @importFrom raster raster
#' @importFrom raster values
#' 
#' @export
#' 

simRasterTrain <- function(nr = 100, nc = 100, classes = c(0,1,NA), 
                           probs = c(0.1,0.2,0.7)){
  
  if(length(classes) != length(probs)){
    stop("Different size between classes and probs vectors!")
  }
  
  rstTrain <- raster::raster(nrow = nr, ncol = nc, crs = NA, res = 1, 
                             xmn = 0, xmx = nc, ymn = 0, ymx = nr)
  
  raster::values(rstTrain) <- sample(classes, nr * nc, 
                                     replace = TRUE, prob = probs)
  
  return(rstTrain)
}


#' Simulate raster segments
#' 
#' Uses functionalities from package NLMR to generate raster segments 
#' very similar to an actual image segmentation algorithm outcome.
#' 
#' @param nr Number of rows for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param nClasses Number of classes to simulate. Must be equal to the length of \code{ai}
#' 
#' @return 
#' A simulated \code{RasterLayer} with segments.
#' 
#' @inheritParams NLMR::nlm_randomcluster
#' @inheritParams raster::clump
#' 
#' @importFrom NLMR nlm_randomcluster
#' @importFrom raster raster
#' @importFrom raster values
#' @importFrom raster maxValue
#' @importFrom raster clump
#' 
#' @export
#' 

simRasterSegments <- function(nr = 100, nc = 100, nClasses = 5, p = 0.3, 
                          ai = rep(1/nClasses, nClasses), neighbourhood = 4, 
                          directions = 8){
  
  if(nClasses != length(ai)){
    stop("nClasses value and ai vector length must be the same!")
  }
  
  rstSegm <- NLMR::nlm_randomcluster(ncol=nc, nrow=nr, resolution = 1, p = p, 
                               ai = ai, neighbourhood = neighbourhood, 
                               rescale = FALSE)
  
  rstClumps <- raster::raster(nrow = nr, ncol = nc, crs = NA, 
                      res = 1, xmn = 0, xmx = nc, ymn = 0, ymx = nr)
  raster::values(rstClumps) <- 0
  
  # Create raster clumps for each class to obtain segments
  # and not LULC patches (for which nlm_randomcluster is 
  # actually designed for...)
  
  for(i in 1:nClasses){
    
    if(i==1){
      rstTmp <- raster::clump(rstSegm == i, directions = directions)
      rstTmp[is.na(rstTmp)] <- 0
      rstClumps <- rstClumps + rstTmp
      rss <- raster::maxValue(rstTmp) # Offsets the value of each new segment
    }else{
      rstTmp <- raster::clump(rstSegm == i, directions = clumpDirs)
      rss <- rss + raster::maxValue(rstTmp)
      rstTmp <- rstTmp + rss
      rstTmp[is.na(rstTmp)] <- 0
      rstClumps <- rstClumps + rstTmp 
      
    }
  }
  return(rstClumps)
}


#' Simulate raster features
#' 
#' Simulate a \code{RasterStack} with multiple layers similar to features used in 
#' segmentation or supervised classification.
#' 
#' @param nr Number of rows for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{RasterLayer} (default: 100).
#' 
#' @param nlyrs Number of layers or bands to simulate (default: 5)
#' 
#' @param distr Name of the function to simulate data (default: random normal "rnorm")
#' 
#' @param args A list object with arguments to use in \code{do.call} passed to the 
#' function set in \code{distr} (default: \code{list(n = nr * nc, mean = 0, sd = 1)} 
#' for rnorm)
#' 
#' @return 
#' A \code{RasterStack} with simulated data
#' 
#' @importFrom raster raster
#' @importFrom raster stack
#' @importFrom raster values
#' 
#' @export
#' 

simRasterFeatures <- function(nr = 100, nc = 100, nlyrs = 5, 
                              distr = "rnorm", args = list(n = nr * nc, mean = 0, sd = 1)){

  for(i in 1:nlyrs){
    
    r <- raster::raster(nrow=nr, ncol=nc, crs=NA, res=1, xmn=0, xmx=nc, ymn=0, ymx=nr)
    raster::values(r) <- do.call(distr, args = args) 
    
    if(i==1){
      rstStack <- raster::stack(r)
    }else{
      rstStack <- raster::stack(rstStack, r)
    }
  }
  names(rstStack) <- paste("b",1:nlyrs,sep="")
  return(rstStack)
  
}



