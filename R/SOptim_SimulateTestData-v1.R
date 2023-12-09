
#' Simulate raster train data
#' 
#' Uses random data to create a simulated \code{SpatRaster} useful 
#' for testing and debugging code.
#' 
#' @param nr Number of rows for the simulated \code{SpatRaster} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{SpatRaster} (default: 100).
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
#' A \code{SpatRaster} object with random train data.
#' 
#' @importFrom terra rast
#' @importFrom terra values
#' 
#' @export
#' 

simRasterTrain <- function(nr = 100, nc = 100, classes = c(0, 1, NA), 
                           probs = c(0.1, 0.2, 0.7)){
  
  if(length(classes) != length(probs)){
    stop("Different size between classes and probs vectors!")
  }
  
  rstTrain <- terra::rast(nrow = nr, ncol = nc, crs = NA, resolution = 1, 
                             xmin = 0, xmax = nc, ymin = 0, ymax = nr)
  
  terra::values(rstTrain) <- sample(classes, nr * nc, 
                                     replace = TRUE, prob = probs)
  
  return(rstTrain)
}


# #' 
# #' 
# #' Simulate raster segments
# #' 
# #' Uses functionalities from package NLMR to generate raster segments
# #' very similar to an actual image segmentation algorithm outcome.
# #' 
# #' @param nr Number of rows for the simulated \code{SpatRaster} (default: 100).
# #' 
# #' @param nc Number of columns for the simulated \code{SpatRaster} (default: 100).
# #' 
# #' @param nClasses Number of classes to simulate. Must be equal to the length of \code{ai}
# #' 
# #' @return
# #' A simulated \code{SpatRaster} with segments.
# #' 
# #' @inheritParams NLMR::nlm_randomcluster
# #' @inheritParams terra::patches
# #' 
# #' @importFrom NLMR nlm_randomcluster
# #' @importFrom terra rast
# #' @importFrom terra values
# #' @importFrom terra maxValue
# #' @importFrom terra patches
# #' 
# #' @export
# #' 
# #' 
# 
# simRasterSegments <- function(nr = 100, nc = 100, nClasses = 5, p = 0.3, 
#                           ai = rep(1/nClasses, nClasses), neighbourhood = 4, 
#                           directions = 8){
#   
#   if(nClasses != length(ai)){
#     stop("nClasses value and ai vector length must be the same!")
#   }
#   
#   rstSegm <- NLMR::nlm_randomcluster(ncol=nc, nrow=nr, resolution = 1, p = p, 
#                                ai = ai, neighbourhood = neighbourhood, 
#                                rescale = FALSE)
#   
#   rstClumps <- terra::rast(nrow = nr, ncol = nc, crs = NA, 
#                       res = 1, xmin = 0, xmax = nc, ymin = 0, ymax = nr)
#   terra::values(rstClumps) <- 0
#   
#   # Create raster clumps for each class to obtain segments
#   # and not LULC patches (for which nlm_randomcluster is 
#   # actually designed for...)
#   
#   for(i in 1:nClasses){
#     
#     if(i==1){
#       rstTmp <- terra::patches(rstSegm == i, directions = directions)
#       rstTmp[is.na(rstTmp)] <- 0
#       rstClumps <- rstClumps + rstTmp
#       rss <- terra::minmax(rstTmp)[2,1] # Offsets the value of each new segment
#     }else{
#       rstTmp <- terra::patches(rstSegm == i, directions = directions)
#       rss <- rss + terra::minmax(rstTmp)[2,1]
#       rstTmp <- rstTmp + rss
#       rstTmp[is.na(rstTmp)] <- 0
#       rstClumps <- rstClumps + rstTmp 
#       
#     }
#   }
#   return(rstClumps)
# }


#' Simulate raster features
#' 
#' Simulate a \code{SpatRaster} with multiple layers similar to features used in 
#' segmentation or supervised classification.
#' 
#' @param nr Number of rows for the simulated \code{SpatRaster} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{SpatRaster} (default: 100).
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
#' A \code{SpatRaster} with simulated data
#' 
#' @importFrom terra rast
#' @importFrom terra values
#' 
#' @export
#' 

simRasterFeatures <- function(nr = 100, nc = 100, nlyrs = 5, 
                              distr = "rnorm", args = list(n = nr * nc, mean = 0, sd = 1)){

  for(i in 1:nlyrs){
    
    tmp <- terra::rast(nrow = nr, ncol = nc, crs = NA, resolution = 1, 
                     xmin = 0, xmax = nc, ymin = 0, ymax = nr)
    
    terra::values(tmp) <- do.call(distr, args = args) 
    
    if(i==1){
      rstStack <- tmp
    }else{
      rstStack <- c(rstStack, tmp)
    }
  }
  
  names(rstStack) <- paste("b",1:nlyrs,sep="")
  return(rstStack)
  
}

#' Modal value
#' 
#' Gets the mode
#' 
#' @param v Vector with numeric values
#' 
#' @export
#' 

modal_value <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Simulate raster segments (faster version)
#' 
#' Uses simulated data for n classes, performs modal smoothing and then 
#' checks for raster clumps similar to segments
#' 
#' @param nr Number of rows for the simulated \code{SpatRaster} (default: 100).
#' 
#' @param nc Number of columns for the simulated \code{SpatRaster} (default: 100).
#' 
#' @param nClasses Number of classes to simulate (default: 4). Larger numbers will 
#' tend to generate more segments.
#' 
#' @param winSize Window size for performing the focal operation using the modal function 
#' (higher values will generate less segments; default: 3)
#' 
#' @inheritParams terra::patches
#' 
#' @return 
#' A simulated \code{SpatRaster} with segments.
#' 
#' @importFrom terra rast
#' @importFrom terra values
#' @importFrom terra patches
#' @importFrom terra focal
#' @importFrom terra modal
#' 
#' @export
#' 

simRasterSegments2 <- function(nr = 100, nc = 100, nClasses = 10, 
                               winSize = 3, directions = 8){
  
  
  rst <- terra::rast(nrow = nr, ncol = nc, crs = NA, resolution = 1, 
                        xmin = 0, xmax = nc, ymin = 0, ymax = nr)
  
  terra::values(rst) <- sample(1:nClasses, ncell(rst), replace=TRUE)
  
  rstModal <- terra::focal(rst, w = matrix(1,winSize,winSize), 
                            fun = modal_value)   
  
  #return(rstModal)
  
  # Create raster clumps for each class to obtain "segments"
  rstClumps <- terra::rast(nrow = nr, ncol = nc, crs = NA,
                              resolution = 1, xmin = 0, xmax = nc, ymin = 0,
                              ymax = nr)

  terra::values(rstClumps) <- 0

  # Check clumps for each class after binarizing each class raster
  for(i in 1:nClasses){

    if(i==1){
      
      rstBin <- (rstModal == i)*1
      rstBin[rstBin==0] <- NA
      
      
      rstTmp <- terra::patches(rstBin, directions = directions)
      rstTmp[is.na(rstTmp)] <- 0
      rstClumps <- rstClumps + rstTmp
      rss <- terra::minmax(rstTmp)[2,1] # Offsets the value of each new segment
    }else{
      rstBin <- (rstModal == i)*1
      rstBin[rstBin==0] <- NA
      
      rstTmp <- terra::patches(rstBin, directions = directions)
      rss <- rss + terra::minmax(rstTmp)[2,1]
      rstTmp <- rstTmp + rss
      rstTmp[is.na(rstTmp)] <- 0
      rstClumps <- rstClumps + rstTmp

    }
  }

  return(rstClumps)
}




