
#' Generate supervised training data for classification
#' 
#' An ancillary function used to generate training data for classification
#' 
#' @param x Input train data used for classification. The input can be a \code{RasterLayer}, 
#' \code{SpatialPointsDataFrame} or a character string with a path to the raster layer.
#' 
#' @param rstSegm A path or a rasterLayer object containing the outputs of a segmentation algorithm 
#' with each object/segment identified by an integer index.
#' 
#' @param thresh A threshold value defining the minimum proportion of the segment ]0, 1] that 
#' must be covered by a certain class to be considered as a training case. This threshold will 
#' only apply if \code{x} is a \code{RasterLayer} which means you are using train areas/pixels. 
#' If you are running a \emph{"single-class"} problem then this threshold only applies to the 
#' class of interest (coded as 1's). Considering this, if a given segment has a proportion cover 
#' of that class higher than \code{thresh} then it is considered a train case. In contrast, for 
#' the background class (coded as 0's), only segments/objects totaly covered by that class are 
#' considered as train cases.      
#' If you are running a \emph{"multi-class"} problem then \code{thresh} is applied differently. 
#' First, the train class is determined by a majority rule then if that class covers more than 
#' the value specified in \code{thresh} this case is kept in train data otherwise it will be 
#' filtered out. See also \code{useThresh}.  
#' 
#' @param useThresh Use threshold to filter training data for multi-class? (default: TRUE; not 
#' used for points).
#' 
#' @param na.rm Remove NA's? (default: TRUE). Only used if \code{x} is a 
#' \code{SpatialPointsDataFrame} object.
#' 
#' @param dup.rm Remove duplicate values? (default: TRUE; see details). Only used if \code{x} is a 
#' \code{SpatialPointsDataFrame} object.
#' 
#' @param minImgSegm Minimum number of image segments/objects necessary to generate train data.
#' 
#' @param ignore If set to TRUE then train data may contain one single class. This is useful in cases 
#' where sample units contain only positive or negative train cases. Also applies if the threshold value 
#' is employed. In this case if no positive cases are generated then negatives will be returned 
#' (default: FALSE).
#' 
#' @param tiles An object of class \code{SOptim.Tiles} creted by \code{\link{createRasterTiles}} 
#' used to read data fractionally by tiles (default: NULL, i.e. not used for).
#' 
#' @details 
#' In some cases, dependending on the type of input training data or the output of the segmentation, 
#' duplicate segment IDs (SID) may occur for different class(es) meaning that a given segment may have 
#' more than one training class. In those cases dup.rm should be set to TRUE (default).       
#' 
#' If \code{x} is a \code{SpatialPointsDataFrame} object then it must contain a column named "train" 
#' with class labels (integer).     
#' 
#' Raster data (in \code{x} and \code{rstSegm}) will be coerced to integer values before performing 
#' cross-tabulations to evaluate the percent coverage.     
#' 
#' @return A two-column data frame with segment IDs (column "SID") and the corresponding train class 
#' (column "train").    
#' 
#' @importFrom stats runif
#' 
#' @examples 
#' 
#' library(raster)
#' 
#' rstSegm <- raster(nrows=100, ncols=100, xmn=0, xmx=100,ymn=0,ymx=100,res=1)
#' km <- kmeans(coordinates(rstSegm),100,iter.max = 100)
#' values(rstSegm) <- km$cluster
#' 
#' rstTrain <- raster(nrows=100, ncols=100, xmn=0, xmx=100,ymn=0,ymx=100,res=1)
#' km <- kmeans(coordinates(rstTrain),5,iter.max = 100)
#' values(rstTrain) <- km$cluster
#' 
#' getTrainData(rstTrain, rstSegm)
#' 
#' 
#' @note 
#' Train raster data must contain at least two categories, coded as integers: 
#' \itemize{
#'    \item - 0 and 1 for "single-class" (with 1's being the feature of interest); 
#'    \item - or 1,2,...,n for "multi-class".
#' }
#' To produce valid train data, the image segmentation produce must generate at least 30 unique 
#' segments (or objects)
#' 
#' @export

getTrainData <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                         dup.rm = TRUE, minImgSegm = 30, ignore = FALSE, tiles = NULL) 
  UseMethod("getTrainData", x)


#' @rdname getTrainData
#' @export
#' 

getTrainData.default <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                 dup.rm = TRUE, minImgSegm = 30, ignore = FALSE, tiles = NULL){
  stop("Object class in x can not be handled by getTrainData")
}


#' @rdname getTrainData
#' @importFrom stats na.omit
#' @export
#' 

getTrainData.SpatialPointsDataFrame <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, 
                                                na.rm = TRUE, dup.rm = TRUE, minImgSegm = 30, 
                                                ignore = FALSE, tiles = NULL){
  
  
  if(is.character(rstSegm)){ 
    
    if(file.exists(rstSegm)){
      rstSegm <- raster::raster(rstSegm)
    }
    else{
      stop("The raster layer in input parameter rstSegm does not exists!")
    }
  }
  else if(!inherits(rstSegm,"RasterLayer")){
    stop("Input rstSegm must be either a character string containing a file path or a RasterLayer object!")
  }
  
  
  ## Extract segment IDs from segmRst using point data ------------------------- ##
  ##
  SIDs <- try(as.integer(raster::extract(rstSegm, x)))
  
  if(inherits(SIDs,"try-error")){
    stop("An error occurred while extracting segment IDs from input point data!")  
  }
  
  # Number of classes in train data
  nUniqueSegments <- length(unique(SIDs))
  
  if(nUniqueSegments < minImgSegm){
    warning("Number of image segments is lower than minImgSegm! Not enough to generate train data")
    return(NA)
  }
  
  if(any(colnames(x@data) == "train")){
    trainData <- data.frame(SID=SIDs,train=as.integer(x@data[,"train"]))
  }
  else{
    trainData <- data.frame(SID=SIDs,train=as.integer(x@data[,1]))
  }
  
  ## Remove duplicates?
  ##
  if(dup.rm){
    trainData <- trainData[!duplicated(trainData),]
  }
  
  ## Remove NA's?
  ##
  if(na.rm){
    return(na.omit(trainData))
  }
  else{
    return(trainData)
  }
  
  # On failure return NULL
  return(NULL)
}


#' @rdname getTrainData
#' @export
#' 

getTrainData.RasterLayer <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                     dup.rm = TRUE, minImgSegm = 30, ignore = FALSE, tiles = NULL){

  getTrainData_(x         = x, 
                rstSegm   = rstSegm, 
                useThresh = useThresh, 
                thresh    = thresh, 
                na.rm     = na.rm, 
                dup.rm    = dup.rm, 
                minImgSegm = minImgSegm, 
                ignore     = ignore, 
                tiles      = tiles)
}


#' @rdname getTrainData 
#' @export
#' 

getTrainData.character <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                   dup.rm = TRUE, minImgSegm = 30,ignore = FALSE, tiles = NULL){
  
  getTrainData_(x         = x, 
                rstSegm   = rstSegm, 
                useThresh = useThresh, 
                thresh    = thresh, 
                na.rm     = na.rm, 
                dup.rm    = dup.rm, 
                minImgSegm = minImgSegm, 
                ignore     = ignore, 
                tiles      = tiles)
}


#' Ancillary function used in getTrainData_
#' 
#' Performs data aggregation ops necessary to calculate the percentage cover of each 
#' class (including: NoData or NA's) and subset it using the threshold rule.
#' 
#' @param x A data.frame or matrix with two columns named: "SID" (segment ID's) and 
#' "train" (with train class or NA)
#' 
#' @inheritParams getTrainData
#' 
#' @return A data.frame with training data.
#' 
#' @export
#' @importFrom rlang .data 
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#' @importFrom dplyr desc
#' @importFrom dplyr n
#' 

calcStats <- function(x, thresh = 0.5){
  
  # Create a "lazy" data.table for use with dplyr verbs and 
  # key by segment identifier (SID)
  x <- dtplyr::lazy_dt(x, key_by = "SID")
  
  # Count the number of pixels per class using a long format
  tb <- x %>% 
    dplyr::group_by(.data$SID, .data$train) %>% 
    dplyr::summarize(prop = dplyr::n()) %>% 
    dplyr::mutate(rs = sum(.data$prop)) %>% # Calculate the #of pixels per train class and segment
    dplyr::ungroup()%>% 
    dplyr::mutate(prop = .data$prop / .data$rs) %>% # Calculate the proportion/ relative frequency
    dplyr::select(-.data$rs) %>% 
    dplyr::filter(!is.na(.data$train)) %>% 
    dplyr::filter(.data$prop >= thresh) %>% # Remove segments below the threshold value
    dplyr::group_by(.data$SID) %>% 
    dplyr::arrange(.data$SID, dplyr::desc(prop)) %>% 
    dplyr::slice(1) %>% # If more than one class select the most prevalent one
    dplyr::ungroup() %>% 
    dplyr::select(-.data$prop) %>% 
    as.data.frame()
  
  return(tb)
}

#' Ancillary function used in getTrainData_ for initial processing (by tile)
#' 
#' Performs data aggregation ops necessary to calculate the number of pixels per 
#' class and segment ID.
#' 
#' @param x A data.frame or matrix with two columns named: "SID" (segment ID's) and 
#' "train" (with train class or NA)
#' 
#' @return A data.frame with preliminary training data.
#' 
#' @export
#' @importFrom rlang .data 
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' 

calcStatsStart <- function(x){
  
  # Create a "lazy" data.table for use with dplyr verbs and 
  # key by segment identifier (SID)
  x <- dtplyr::lazy_dt(x, key_by = "SID")
  
  # Count the number of pixels per class using a long format
  tb <- x %>% 
    dplyr::group_by(.data$SID, .data$train) %>% 
    dplyr::summarize(prop = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    as.data.frame()
  
}

#' Ancillary function used in getTrainData_ for final processing (by tile)
#' 
#' Performs data aggregation ops necessary to calculate the percentage cover per 
#' class and segment ID based on initial processing by \code{\link{calcStatsStart}}.
#' 
#' @param x A data.frame from \code{\link{calcStatsStart}}.
#' 
#' @inheritParams getTrainData
#' 
#' @return A data.frame with final training data.
#' 
#' @export
#' @importFrom rlang .data 
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#' @importFrom dplyr desc
#' 

calcStatsFinish <- function(x, thresh = 0.5){
  
  # Create a "lazy" data.table for use with dplyr verbs and 
  # key by segment identifier (SID)
  x <- dtplyr::lazy_dt(x, key_by = "SID")
  
  # Count the number of pixels per class using a long format
  tb <- x %>% 
    dplyr::group_by(.data$SID, .data$train) %>% 
    dplyr::summarize(prop = sum(.data$prop)) %>%    
    dplyr::mutate(rs = sum(.data$prop)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prop = .data$prop / .data$rs) %>% 
    dplyr::select(-.data$rs) %>% 
    dplyr::filter(!is.na(.data$train)) %>% 
    dplyr::filter(.data$prop >= thresh) %>% 
    dplyr::group_by(.data$SID) %>% 
    dplyr::arrange(.data$SID, dplyr::desc(prop)) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$prop) %>% 
    as.data.frame()
  
}


#' Generate supervised training data for classification
#' 
#' An ancillary function used to generate training data for classification. This function is the 
#' workhorse behind \code{\link{getTrainData}}.
#' 
#' @param x Input train data used for classification. The input can be a \code{RasterLayer} 
#' or a character string with a path to the raster layer.
#' 
#' @inheritParams getTrainData
#' 
#' @return A two-column data frame with segment IDs (column "SID") and the corresponding train 
#' class (column "train").  
#' 
#' @note 
#' Train raster data must contain at least two categories, coded as integers: 
#' \itemize{
#'    \item - 0 and 1 for "single-class" (with 1's being the class of interest and 
#'    typically the minority class); 
#'    \item - or 1, 2, ..., n for "multi-class".
#' }
#' Background or null pixels should be coded as NoData.
#' To produce valid train data, the image segmentation produce must generate more than 
#' \code{minImgSegm} unique segments (or objects).
#' 
#' 
#' @export
#' @importFrom raster values
#' @importFrom raster raster
#' @importFrom raster stack
#' @importFrom dplyr bind_rows
#' 

getTrainData_ <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                             dup.rm = TRUE, minImgSegm = 30,ignore = FALSE, tiles = NULL){
  
  
  # print(class(x))
  # print(class(rstSegm))
  
  # Read train raster metadata
  if(is.character(x)){
    x <- raster::raster(x)
  }
  
  # Read segmentation raster metadata
  if(is.character(rstSegm)){ 
    rstSegm <- raster::raster(rstSegm)
  }
  
  # print(class(x))
  # print(class(rstSegm))
  
  # Read raster data for train layer
  if(!inherits(x,"RasterLayer")){
    stop("Train data in x must be an object of class RasterLayer")
  }
  
  # Read raster data for segmentation layer
  if(!inherits(rstSegm,"RasterLayer")){
    stop("Segmented data in rstSegm must be an object of class RasterLayer")
  }
  
  if(!(compareRaster(x, rstSegm, stopiffalse = FALSE))){
    stop("Differences between train and segment rasters in getTrainData method!")
  }
  
  ### ------------------------------------------------------------- ###
  ### NO TILING / READ ALL DATA AT ONCE
  ### ------------------------------------------------------------- ###
  
  if(is.null(tiles)){
    
    rstSegmTrainDF <- data.frame(SID  = as.integer(raster::values(rstSegm)), # Force integer output
                                 train = as.integer(raster::values(x))) # Force integer output
    
    outTrainDF <- try(calcStats(x      = rstSegmTrainDF, 
                                thresh = thresh))
    
    if(inherits(outTrainDF,"try-error")){
      
      if(ignore){
        return(NA)
      }else{
        stop("An error occurred while calculating training data stats!")
      }
    }
  }## ------- End processing w/o tiles  ------- ##
  
  
  ### ------------------------------------------------------------- ###
  ### USE TILING SYSTEM / READ DATA IN CHUNKS
  ### ------------------------------------------------------------- ###
  
  else{
    
    # Stack input and rename
    rstStack <- raster::stack(rstSegm, x)
    names(rstStack) <- c("SID", "train")
    
    lenMax <- length(tiles) # Number of tiles to process
    for(i in 1:lenMax){
      
      # Read data for each tile
      tmpRstSegmTrainDF <- readDataByTile(rst       = rstStack, 
                                          tiles     = tiles, 
                                          tileIndex = i, 
                                          as.df     = FALSE)
      
      # Calculate initial stats (#of pixels per class)
      tmpOutTrainDF <- try(calcStatsStart(x = tmpRstSegmTrainDF))
      
      if(inherits(tmpOutTrainDF, "try-error")){
        
        if(ignore){
          warning("An error occurred while calculating training data stats (func: calcStatsStart)!")
          return(NA)
        }else{
          stop("An error occurred while calculating training data stats (func: calcStatsStart)!")
        }
      }else{ # Accumulate train data by tiles
        if(i==1){
          outTrainDF <- tmpOutTrainDF
        }else{
          outTrainDF <- dplyr::bind_rows(outTrainDF, tmpOutTrainDF)
        }
      }
    }
    
    # Aggregate across all segments and finalize stats
    outTrainDF <- try(calcStatsFinish(outTrainDF, thresh = thresh))
    
    if(inherits(outTrainDF, "try-error")){
      
      if(ignore){
        stop("An error occurred while calculating training data stats (func: calcStatsFinish)!")
        return(NA)
      }else{
        stop("An error occurred while calculating training data stats (func: calcStatsFinish)!")
      }
    }
  }## ------- End processing with tiles  ------- ##
  
  
  ## ----- Make final checks ----- ##
  
  # Get/check the number of unique segments in input segmentation raster
  nUniqueSegments <- length(unique(outTrainDF$SID)) 
  
  if(nUniqueSegments < minImgSegm){
    
    if(ignore){
      warning("Number of image segments is lower than minImgSegm! Not enough to generate train data")
      return(NA)
    }else{
      stop("Number of image segments is lower than minImgSegm! Not enough to generate train data")
    }
  }
  
  # Get/check the number of distinct classes in the final DF
  nClassesTrain <- length(unique(outTrainDF$train))
  
  if(nClassesTrain < 2){
    if(ignore){
      warning("Number of train classes is lower than two!")
      return(NA)
    }else{
      stop("Train data must have at least two different classes!")
    }
  }
  else{
    if(nClassesTrain == 2){
      attr(outTrainDF,"nClassType") <- "single-class"
      return(outTrainDF)
    }else{
      attr(outTrainDF,"nClassType") <- "multi-class"
      return(outTrainDF)
    }
  }
}


