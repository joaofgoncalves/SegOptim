
## ---------------------------------------------------------------------------------------------------------------- ##
## --- AUXILIARY FUNCTIONS -------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------------- ##


#' Factor to integer conversion
#' 
#' Converts a factor variable to an integer number
#' 
#' @param x A factor.
#' 
#' @return Returns an integer number.
#' 
#' @export

f2int <- function(x) (1:length(levels(x)))[as.integer(x)]
#f2int<-function(x) as.integer(as.character(x))


#' Remove NA values
#' 
#' An useful function to remove NA values for vector, matrices or data frames. By default removes all NA values 
#' in each column of a matrix or data frame.
#' 
#' @param x Either a vector, matrix or data frame.
#' 
#' @return Returns the original object without \code{NA} values. An attribute named \code{"na.action"} is returned 
#' as a list containing the original indices that were removed.
#' 
#' @details 
#' This function was adapted from IDPmisc package (URL: \url{https://github.com/cran/IDPmisc/blob/master/R/NaRV.omit.R}).
#' 
#' @importFrom stats na.omit
#' 
#' @export


NRV.omit <- function(x){

  if(is.vector(x)) {
    if (is.numeric(x)){
      x <- na.omit(x[is.finite(x)]) 
    } 
    else{
      x <- na.omit(x)
    }
  } 
  else if(is.factor(x)){
    x <- na.omit(x)
  } 
  else if(is.data.frame(x)){
    x.num <- as.matrix(x[,sapply(x, is.numeric)])
    ri <- (!apply(x.num, MARGIN = 1, function(x) sum(is.infinite(x))>0) & !apply(x, MARGIN = 1, function(x) sum(is.na(x))>0))
    x <- x[ri,,drop=FALSE]
  } 
  else if(is.matrix(x)){
    
    if(is.numeric(x)){
      ri <- !apply(x, MARGIN = 1, function(x) {sum(is.na(x)|is.infinite(x))>0})
      x <- x[ri,,drop=FALSE]
      attributes(x) <- c(attributes(x),list(na.action=which(!ri),class="omit"))
    } 
    else{
      x <- na.omit(x)
    }
  } 
  else{
    warning("'x' is neither a vector, nor a factor, nor a data.frame nor a matrix. \n'x' is returned unchanged\n")
    return(x)
  }
  return(x)
}


#' Find python scripts location
#' 
#' Finds the path to python scripts used to perform image segmentation in ArcGIS or RSGISLib based on 
#' the package location or other user-defined places.
#' 
#' @param pyScriptName The name of the python script.
#' @param pkgName The name of the package to look in.
#' @param altLocs Alternative locations to look for the script.
#' 
#' @return If the py script is found in any location it returns the complete path. 
#' 
#' @note The \code{altLocs} parameter is used to input development paths.
#' 
#' @export

getPythonFile <- function(pyScriptName, pkgName, altLocs){

  posibleLocations <- c(
    paste(.libPaths(),pkgName,"inst",pyScriptName,sep="/"),
    paste(.libPaths(),pkgName,"inst/PyScripts",pyScriptName,sep="/"),
    paste(.libPaths(),pkgName,"PyScripts",pyScriptName,sep="/"),
    paste(altLocs,pkgName,"inst",pyScriptName,sep="/"),
    paste(altLocs,pkgName,"inst/PyScripts",pyScriptName,sep="/"),
    paste(altLocs,pkgName,"PyScripts",pyScriptName,sep="/"),
    paste(altLocs,pyScriptName,sep="/")
  )

  for(f in posibleLocations){
    if(file.exists(f))
      return(f)
  }
  return(NULL)
}


#' Check and print segmentation parameters
#' 
#' An useful function to verify the name of segmentation parameters to be optimized and to print them during 
#' GA optimization.
#' 
#' @param x A character vector containing parameter names.
#' @param segmentMethod The name of the segmentation method. Check available options on \code{\link{segmentationGeneric}}.
#' @param rnd An integer defining the number of decimal plates.
#' @param print Boolean defining whether or not to print the parameters.
#' 
#' @return Prints a message with the segmentation parameters (returns NULL).
#' 
#' @examples 
#' checkPrintSegmentationParams(x = c(0.1, 5), segmentMethod = "GRASS_RG", rnd = 5, print = TRUE)
#'  
#' 
#' @export

checkPrintSegmentationParams <- function(x, segmentMethod, rnd = 5, print = TRUE){
  
  p <- segmentationParamNames()[[segmentMethod]]
  
  if(length(x) != length(p)){
    stop("The segmentation method requires ",length(p)," parameters to be optimized and ",length(x),
         " were defined! Please check input arguments")
  }
  
  if(print){
    for(i in 1:length(x)){
      message(p[i], "=", round(x[i], rnd))
    }
  }
}


#' Generate a random string
#' 
#' An ancillary function used to generate a random string containing integers and lettters with 
#' user-defined length.  
#' 
#' @param x The size of the random string to be generated (10 by default).
#' 
#' @return A character string.
#' 
#' @examples randString(12)
#' 
#' @export

randString <- function(x = 10){
  z <- sample(c(letters, sample(0:9, length(letters), replace = TRUE)), x, replace=TRUE)
  return(paste(z, collapse = "", sep = ""))
} 


#' Replace backslash
#' 
#' Replace back-slashes by forwardslashes which is probably useful for system paths
#' 
#' @param x A string to replace.
#' 
#' @return A string with replaced backslashes.
#' 
#' @examples 
#' repBSlash("\\aa\\bb\\cc")
#' 
#' @export

repBSlash <- function(x) gsub("\\\\","/",x) 


#' Calculate aggregated statistics by each image segment or region
#' 
#' Calculate statistics for each element (e.g., a segment, object or region) based on \code{data.table} objects 
#' (much faster processing!).
#' 
#' @param x An object of class \code{data.frame}, \code{matrix}, \code{RasterStack} or \code{RasterLayer} 
#' containing data (e.g., classification features).
#' 
#' @param z An object of class \code{RasterLayer}, \code{vector}, or a one column \code{data.frame}/\code{matrix} 
#' containing segment/object/zone indices (as integers). Usually the result of a image segmentation algorithm.
#' 
#' @param ... Further parameters to pass to \code{lapply}.
#' 
#' @param fun An aggregation function (e.g., \code{mean}) applied to the elements within each segment. 
#' Either a function object or a function name.
#' 
#' @param na.rm A boolean defining whether or not to remove NA values (default: \code{TRUE}). 
#' 
#' @param subset A vector defining which rows should be subset from z (zones or regions).
#' 
#' @return A data.table object containing statistics by segment.
#' 
#' @note Both object must have exactly the same number of elements. For example, if \code{x} and \code{z} are 
#' raster datasets these must have equal number of rows/columns, extent and coordinate reference system. If  
#' \code{x} is a \code{data.frame} or \code{matrix} then the number of cells in \code{z} must equal the number 
#' of rows in \code{x}.
#' 
#' 
#' @export
#' 
#' @import data.table 
#' @importFrom raster values
#' @importFrom raster raster
#' @importFrom raster compareRaster
#' 

zonalDT <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) UseMethod("zonalDT", x)

#' @rdname zonalDT
#' @export
#' 

zonalDT.default <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  stop("zonalDT allows only objects of type data.frame, RasterLayer or RasterStack")


#' @rdname zonalDT
#' @export
#' 

zonalDT.data.frame <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) {
  
  # Check and get function by name
  if(!is.function(fun) & is.character(fun)){
    fun <- match.fun(fun)
  }
  else if(!is.function(fun) & !is.character(fun)){
    stop("Invalid input in fun! Must be either a function object or a function name.")
  }
  
  if(inherits(z,"RasterLayer")){
    # Get data from raster object
    zones <- as.integer(raster::values(z))
    
  }else if(is.numeric(z)){
    zones <- as.integer(z)
    rm(z)
  }else if(is.data.frame(z)){
    zones <- as.integer(z[,1])
    rm(z)
  }else if(is.matrix(z)){
    zones <- as.integer(z[,1])
    rm(z)
  } else{
    stop("The object class in x is not allowed for zonalDT. 
         It must be either a RasterLayer, integer vector, or a one column data.frame/matrix")
  }

  # Compare rasters 
  if(length(zones) != nrow(x))
    stop("Different number of pixel values between x and z!")
  
  # Create data.table object
  rDT <- data.table::data.table(x, z=zones)
  rm(x)
  
  # Set data table key for the zones/segments
  data.table::setkey(rDT, z)
  
  # Subset data only to segments defined in subset?
  if(!is.null(subset)){
    rDT <- rDT[list(subset)]
  }
  
  # Perform the data aggregation
  return(rDT[, lapply(.SD, fun, na.rm=na.rm, ...), by=z])
} 


#' @rdname zonalDT
#' @export
#' 

zonalDT.RasterStack <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) {
  
  # Check and get function by name
  if(!is.function(fun) & is.character(fun)){
    fun <- match.fun(fun)
  }else if(!is.function(fun) & !is.character(fun)){
    stop("Invalid input in fun! Must be either a function object or a function name.")
  }
  
  # Compare rasters 
  if(inherits(z,"RasterLayer")){
    if(!raster::compareRaster(x, z, stopiffalse = FALSE, showwarning = TRUE))
      stop("Different rasters in x and z!")
  }
  
  if(inherits(z,"RasterLayer")){
    # Get data from raster object
    zones <- as.integer(raster::values(z))
  }else if(is.numeric(z)){
    zones <- as.integer(z)
    rm(z)
  }else if(is.data.frame(z)){
    zones <- as.integer(z[,1])
    rm(z)
  }else if(is.matrix(z)){
    zones <- as.integer(z[,1])
    rm(z)
  } else{
    stop("The object class in x is not allowed for zonalDT. 
         It must be either a RasterLayer, integer vector, or a one column data.frame/matrix")
  }
  
  
  # Create a data.table object by reference to avoid memory overloads
  #vals <- data.table::setDT(raster::values(x))
  vals <- raster::values(x)
  
  # Create data.table object and set key
  rDT <- data.table::data.table(vals, z=zones)
  rm(vals)
  
  # Set data table key
  data.table::setkey(rDT, z)
  
  # Subset data only to segments defined in subset?
  if(!is.null(subset)){
    rDT <- rDT[list(subset)]
  }
  
  return(rDT[, lapply(.SD, fun, na.rm=na.rm, ...), by=z])
  
} 


#' @rdname zonalDT
#' @export
#' 

zonalDT.RasterLayer <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  zonalDT.RasterStack(x, z, fun, na.rm = TRUE, subset, ...)

#' @rdname zonalDT
#' @export
#' 

zonalDT.RasterBrick <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  zonalDT.RasterStack(x, z, fun, na.rm = TRUE, subset, ...)


#' @rdname zonalDT
#' @export
#' 

zonalDT.matrix <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  zonalDT.data.frame(x, z, fun, na.rm = TRUE, subset, ...)



#' Calculate segment statistics for multiple raster features and aggregation functions
#' 
#' Uses \code{\link{zonalDT}} to calculate statistics for each segment to be used in the 
#' classification stage.
#' 
#' @param rstFeatures A string defining the path to the raster features or a \code{rasterStack} object.
#' 
#' @param rstSegm A string defining the path to the raster with segment IDs or a \code{rasterLayer} object.  
#' 
#' @param funs A character vector with the name(s) of the functions used to aggregate data.
#' 
#' @param na.rm Boolean defining if NA's should be removed.
#' 
#' @param subset A vector defining which segments should be subset.
#' 
#' @param bylayer Calculate statistics layer by layer instead of all at once? (slightly increases 
#' computation time but spares memory load)
#' 
#' @param progressBar Boolean. Show progress bar? (default: FALSE)
#'    
#' @return A data frame containing segment ids (first column, SID) and aggregated features. 
#' The name of the aggregation function will be appended to the name of each raster feature. 
#' 
#' @examples 
#'
#' library(raster)
#' 
#' rstSegm <- raster(nrows=100, ncols=100, xmn=0, xmx=100, 
#'                   ymn=0, ymx=100)
#' 
#' km <- kmeans(coordinates(rstSegm),100,iter.max = 100)
#' values(rstSegm) <- km$cluster
#'    
#' rstFeatures_1 <- raster(nrows=100, ncols=100, xmn=0, xmx=100, 
#' ymn=0, ymx=100, res=1, vals=runif(10000))
#' 
#' rstFeatures_2 <- raster(nrows=100, ncols=100, xmn=0, xmx=100, 
#' ymn=0, ymx=100, res=1, vals=runif(10000))
#'    
#' rstStackFeatures <- stack(rstFeatures_1, rstFeatures_2)
#' 
#' calculateSegmentStats(rstStackFeatures, rstSegm, 
#'                       funs = c("mean", "sd"), na.rm = TRUE)
#' 
#' 
#' @export
#' @importFrom raster stack
#' @importFrom raster raster
#' @importFrom raster values
#' @importFrom raster nlayers
#' @importFrom raster canProcessInMemory
#' @importFrom raster compareRaster
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom data.table data.table
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' 
#' 


# calculateSegmentStats <- function(rstFeatures, rstSegm, funs = c("mean", "sd"), 
#                                   na.rm = TRUE, subset=NULL){
#   
#   # Check if funs argument is a character vector
#   if(!is.character(funs))
#     stop("The funs argument (used to calculate segment statistics) must be a character vector!")
#   
#   # If rstFeatures or rstSegm are strings then first load raster metadata
#   #
#   if(is.character(rstFeatures)) rstFeatures <- raster::stack(rstFeatures)
#   
#   if(is.character(rstSegm)) rstSegm <- raster::raster(rstSegm)
#   
#   # Iterate by function
#   i<-0
#   
#   for(f in funs){
#     
#     i<-i+1
#     
#     # Do zonal stats based on data table object
#     tempDF <- as.data.frame(zonalDT(rstFeatures, rstSegm, f, na.rm = na.rm, subset = subset))
#     
#     # Set column names based on the aggregation function used
#     colnames(tempDF)[2:ncol(tempDF)] <- paste(colnames(tempDF)[2:ncol(tempDF)],f,sep="_")
#     
#     # Merge/Join data for each function calculated based on SID
#     # Z is the temp field name for segment ids
#     if(i==1){
#       outputDF <- tempDF
#     }else{
#       outputDF <- merge(outputDF, tempDF, by="z", all=FALSE)
#     }
#   }
#   
#   # Rename the first column to segment/object unique identifier (SID)
#   colnames(outputDF)[1] <- "SID"
#   return(outputDF)
#   
# }


calculateSegmentStats <- function(rstFeatures, rstSegm, funs = c("mean", "sd"), na.rm = TRUE, 
                                   bylayer = FALSE, subset = NULL, progressBar = FALSE){
  
  
  # Check if funs argument is a character vector
  if(!is.character(funs))
    stop("The funs argument (used to calculate segment statistics) must be a character vector!")
  
  # If rstFeatures or rstSegm are strings then first load raster metadata
  #
  if(is.character(rstFeatures)) rstFeatures <- raster::stack(rstFeatures)
  if(is.character(rstSegm)) rstSegm <- raster::raster(rstSegm)
  
  # Compare rasters 
  if(!raster::compareRaster(rstFeatures, rstSegm, stopiffalse = FALSE, showwarning = TRUE))
    stop("Different rasters defined in rstFeatures and rstSegm!")
  

  if(!bylayer){
    
    # Check memory requirements to handle the raster data   
    if(!raster::canProcessInMemory(raster::stack(rstSegm, rstFeatures), n = 2))
      stop("Not enough memory to process the input raster files! Modify option bylayer to TRUE")
    
    # Read data from raster files
    rDT <- data.table::data.table(raster::values(raster::stack(rstSegm, rstFeatures)))
    
    # Set data table key
    data.table::setkeyv(rDT, colnames(rDT)[1])
    
    # Subset data only to segments defined in subset?
    if(!is.null(subset)){
      rDT <- rDT[list(subset)]
    }
    
    if(progressBar) 
      pb <- utils::txtProgressBar(1,length(funs),style=3)
    
    for(i in 1:length(funs)){
      
      fun <- match.fun(funs[i])
      
      # Do zonal stats based on data table object
      tempDF <- rDT[, lapply(.SD, fun, na.rm=na.rm), by=eval(colnames(rDT)[1])]
      
      # Set column names based on the aggregation function used
      colnames(tempDF) <- c("SID", paste(colnames(tempDF)[2:ncol(tempDF)], funs[i], sep="_"))
      #colnames(tempDF)[1] <- "SID"
      data.table::setkey(tempDF, SID)
      
      # Merge/Join data for each function calculated based on SID
      if(i==1){
        outputDF <- tempDF
      }else{
        #outputDF <- merge(outputDF, tempDF, by="SID", all=FALSE)
        outputDF[tempDF, colnames(tempDF)[-1] := as.list(tempDF[,-1]) ]
      }
      
      if(progressBar) 
        utils::setTxtProgressBar(pb, i)
    }
  }else{
    
    # Check memory requirements to handle the raster data    
    if(!raster::canProcessInMemory(raster::stack(rstSegm, rstFeatures[[1]]), n = 2))
      stop("Not enough memory to process the input raster files when using option bylayer!")
    
    # IDs for raster segments
    rstSegmID <- raster::values(rstSegm)
    j<-0
    
    nl <- raster::nlayers(rstFeatures)
    
    if(progressBar){
      pb <- utils::txtProgressBar(1, nl * length(funs), style=3)
    }
      
    
    ## Iterate through each layer and generate statistics by segment
    ##
    for(lyr in 1:nl){
      
      # Read data from raster files
      lyrname <- names(rstFeatures)[lyr]
      rDT <- data.table::data.table(cbind(rstSegmID, raster::values(rstFeatures[[lyr]])))
      colnames(rDT) <- c("SID", lyrname)
      
      # Set data table key
      data.table::setkey(rDT, SID)
      
      # Subset data only to segments defined in subset?
      if(!is.null(subset)){
        rDT <- rDT[list(subset)]
      }
      
      # Iterate by user-defined fuction
      #
      for(i in 1:length(funs)){
        
        j<-j+1
        fun <- match.fun(funs[i])
        
        # Do zonal stats based on data table object
        # tempDF <- as.data.frame(rDT[, lapply(.SD, fun, na.rm=na.rm), by=SID])
        tempDF <- rDT[, lapply(.SD, fun, na.rm=na.rm), by=SID]
        
        # Set column names based on the aggregation function used
        colnames(tempDF) <- c("SID", "FEAT_VALUE") # paste(colnames(tempDF)[2:ncol(tempDF)], funs[i], sep="_"))
        data.table::setkey(tempDF, SID)
        
        # Merge/Join data for each function calculated based on SID
        if(j==1){
          outputDF <- tempDF
          colnames(outputDF)[2] <- paste(lyrname,funs[i],sep="_")
        }else{
          # outputDF <- merge(outputDF, tempDF, by="SID", all=FALSE)
          outputDF[tempDF, on = 'SID', `:=`(paste(lyrname,funs[i],sep="_"), FEAT_VALUE)]
        }
        
        if(progressBar) 
          utils::setTxtProgressBar(pb, j)
      }
    }
  }
  
  # Rename the first column to segment/object unique identifier (SID)
  #colnames(outputDF)[1] <- "SID"
  return(as.data.frame(outputDF))
  
}




#' Generate supervised training data for classification
#' 
#' An ancillary function used to generate training data for classification
#' 
#' @param x Input train data used for classification. The input can be a \code{RasterLayer}, \code{SpatialPointsDataFrame}, 
#' an integer vector (containing raster data after using \code{\link[raster]{values}} function) or a character string with a 
#' path to the raster layer. If x is an integer vector then it should have a length equal to the number of pixels in rstSegm.
#' 
#' @param rstSegm A path or a rasterLayer object containing the outputs of a segmentation algorithm with each object/segment 
#' identified by an integer index.
#' 
#' @param thresh A threshold value defining the minimum proportion of the segment ]0, 1] that must be covered 
#' by a certain class to be considered as a training case. This threshold will only apply if \code{x} is a \code{RasterLayer} 
#' which means you are using train areas/pixels. 
#' If you are running a \emph{"single-class"} problem then this threshold only applies to the class of interest (coded as 1's). 
#' Considering this, if a given segment has a proportion cover of that class higher than \code{thresh} then it is 
#' considered a train case. In contrast, for the background class (coded as 0's), only segments/objects 
#' totaly covered by that class are considered as train cases.      
#' If you are running a \emph{"multi-class"} problem then \code{thresh} is applied differently. First, the train class is 
#' determined by a majority rule then if that class covers more than the value specified in \code{thresh} this case is 
#' kept in train data otherwise it will be filtered out. See also \code{useThresh}.  
#' 
#' @param useThresh Use threshold to filter training data for multi-class? (default: TRUE; not used for points).
#' 
#' @param na.rm Remove NA's? (default: TRUE). Within the function, NA's in x are recoded as -1 so that percentage cover of each 
#' segment are calculated also considering background (null or NoData values).
#' 
#' @param dup.rm Remove duplicate values (default: TRUE; see details).
#' 
#' @param minImgSegm Minimum number of image segments/objects necessary to generate train data.
#' 
#' @param ignore If set to TRUE then train data may contain one single class. This is useful in cases where sample units 
#' contain only positive or negative train cases. Also applies if the threshold value is employed. 
#' In this case if no positive cases are generated then negatives will be returned (default: FALSE).
#' 
#' @details 
#' In some cases, dependending on the type of input training data or the output of the segmentation, 
#' duplicate segment IDs (SID) may occur for different class(es) meaning that a given segment may have more 
#' than one training class. In those cases dup.rm should be set to TRUE (default).       
#' 
#' If \code{x} is a \code{SpatialPointsDataFrame} object then it must contain a column named "train" with class labels
#' (integer).     
#' 
#' Raster data (in \code{x} and \code{rstSegm}) will be coerced to integer values before performing cross-tabulations to 
#' evaluate the percent coverage.     
#' 
#' @return A two-column data frame with segment IDs (column "SID") and the corresponding train class (column "train").    
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
#' To produce valid train data, the image segmentation produce must generate at least 30 unique segments (or objects)
#' 
#' @export

getTrainData <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                         dup.rm = TRUE, minImgSegm = 30, ignore = FALSE) 
  UseMethod("getTrainData", x)


#' @rdname getTrainData
#' @export
#' 

getTrainData.default <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                 dup.rm = TRUE, minImgSegm = 30, ignore = FALSE){
  stop("Object class in x can not be handled by getTrainData")
}


#' @rdname getTrainData
#' @importFrom stats na.omit
#' @export
#' 

getTrainData.SpatialPointsDataFrame <- function(x, rstSegm, useThresh = TRUE, thresh = 0.5, na.rm = TRUE, 
                                                dup.rm = TRUE, minImgSegm = 30, ignore = FALSE){
  
  
  if(is.character(rstSegm)){ 
    
    if(file.exists(rstSegm)){
      rstSegm<-raster::raster(rstSegm)
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

getTrainData.RasterLayer <- function(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, dup.rm=TRUE, minImgSegm=30,ignore=FALSE){
  getTrainData_(x, rstSegm, useThresh, thresh, na.rm, dup.rm, minImgSegm, ignore)
}


#' @rdname getTrainData 
#' @export
#' 

getTrainData.character <- function(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, dup.rm=TRUE, minImgSegm=30,ignore=FALSE){
  getTrainData_(x, rstSegm, useThresh, thresh, na.rm, dup.rm, minImgSegm, ignore)
}


#' @rdname getTrainData
#'
#' 

getTrainData.integer <- function(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, dup.rm=TRUE, minImgSegm=30,ignore=FALSE){
  getTrainData_(x, rstSegm, useThresh, thresh, na.rm, dup.rm, minImgSegm, ignore)
}



#' Generate supervised training data for classification
#' 
#' An ancillary function used to generate training data for classification. This function is the 
#' workhorse behind \code{\link{getTrainData}}.
#' 
#' @param x Input train data used for classification. The input can be a \code{RasterLayer}, 
#' an integer vector (containing raster data after using \code{\link[raster]{values}} function) 
#' or a character string with a path to the raster layer. If x is an integer vector then it should 
#' have a length equal to the number of pixels in \code{rstSegm}.
#' 
#' @inheritParams getTrainData
#' 
#' @return A two-column data frame with segment IDs (column "SID") and the corresponding train 
#' class (column "train").  
#' 
#' @note 
#' Train raster data must contain at least two categories, coded as integers: 
#' \itemize{
#'    \item - 0 and 1 for "single-class" (with 1's being the class of interest and tipically the minority class); 
#'    \item - or 1, 2, ..., n for "multi-class".
#' }
#' Background or null pixels should be coded as NoData.
#' To produce valid train data, the image segmentation produce must generate more than \code{minImgSegm} 
#' unique segments (or objects).     
#' In case of \code{NA}'s the function internally assigns them a value of -1 to allow their use for calculating 
#' the relative coverage of each class by segment in train data. 
#' 
#' 
#' @export
#' @importFrom raster values
#' @importFrom raster raster
#' 

getTrainData_ <- function(x, rstSegm, useThresh=TRUE, thresh=0.5, na.rm=TRUE, dup.rm=TRUE, minImgSegm=30,ignore=FALSE){
  
  # Read train raster metadata
  if(is.character(x)){
    x <- raster::raster(x)
  }
    
  # Read segmentation raster metadata
  if(is.character(rstSegm)){ 
    rstSegm <- raster::raster(rstSegm)
  }
  
  # Read raster data for train layer
  if(inherits(x,"RasterLayer")){
    x <- as.integer(raster::values(x)) # Force integer output
  }
  
  # Read raster data for segmentation layer
  if(inherits(rstSegm,"RasterLayer")){
    rstSegm <- as.integer(raster::values(rstSegm)) # Force integer output
  }
  
  # 1st col -> segment IDs
  # 2nd col -> train data
  if(length(x) == length(rstSegm)){
      
    # Generate matrix with segment ID's [col1] and train data [col2]
    rstSegmTrainDF <- cbind(rstSegm, x, deparse.level = 0)
  }else{
    stop("Different size between train and segment rasters for getTrain method!")
  }

  # Number of unique segments in input segmentation raster
  nUniqueSegments <- length(unique(rstSegmTrainDF[,1])) 
  
  if(nUniqueSegments < minImgSegm){
    warning("Number of image segments is lower than minImgSegm! Not enough to generate train data")
    return(NA)
  }
  
  # Check if the data has got any NA values
  hasNAs <- sum(is.na(rstSegmTrainDF[,2])) > 0
  
  if(hasNAs){
    
    # Replace NA's by -1 before performing the cross-tab
    rstSegmTrainDF[is.na(rstSegmTrainDF[,2]), 2] <- -1
    
    # Check the number of different classes to determine the correct output
    
    # Number of classes in train data (after removing the background class coded as -1):
    nClassesTrain <- length(unique(rstSegmTrainDF[,2])) - 1 
  }else{
    # If the data has no NA's then do not subtract 1
    nClassesTrain <- length(unique(rstSegmTrainDF[,2]))
  }

  
  if(nClassesTrain < 2){
    if(!ignore){
      stop("Train data must have at least two different classes!")
    }else{
      trainData <- data.frame(SID = unique(rstSegmTrainDF[,1]), train = rstSegmTrainDF[1,2])
      attr(trainData,"nClassType") <- "single-class"
      return(trainData)
    }
  }
  else if(nClassesTrain == 2){ ## Processing single-class input data --------------------------------------------------- ##
    
    # Cross-tabulate classes (one column per class) by segment id (one row per segment) 
    # First column of xt is used as the frequency of NA values (coded as -1's)
    xt <- table(rstSegmTrainDF[,1], rstSegmTrainDF[,2])
    xt <- xt / (apply(xt, 1, sum)) # calculate relative frequency table

    # Generate data
    # NA's coded as -1 values appear in position [1]
    # Negative cases (majority class [2]) must have 100% of coverage so that train = 0
    # For positive cases [3] coverage is defined by thresh

    if(hasNAs){
      # If data has got NAs then positions 1, 2 and 3 in the xt object exist
      neg <- try(data.frame(SID = as.integer(rownames(xt)[xt[,2] == 1]), train = 0), silent = TRUE)
      pos <- try(data.frame(SID = as.integer(rownames(xt)[xt[,3] >= thresh]), train = 1), silent = TRUE)
    }else{
      # Otherwise, there is no background so only positions 1 and 2
      neg <- try(data.frame(SID = as.integer(rownames(xt)[xt[,1] == 1]), train = 0), silent = TRUE)
      pos <- try(data.frame(SID = as.integer(rownames(xt)[xt[,2] >= thresh]), train = 1), silent = TRUE)
    }
    
    if(!inherits(pos,"try-error") && !inherits(neg,"try-error")){
      # Assemble positive and negatives cases data if no error occured
      trainData <- rbind(pos, neg)
      
      attr(trainData,"nClassType") <- "single-class"
      return(trainData)
    }
    else{
      if(!ignore){
        
        warning("An error occurred while assembling training data in method getTrainData! 
                One of the classes has no train cases")
        return(NA)
      
        }else{
        
          if(!inherits(neg,"try-error")){
            trainData <- neg
            if(na.rm){
              trainData <- trainData[(trainData$train != -1),]
            } 
            attr(trainData,"nClassType") <- "single-class"
            
            warning("An error occurred while assembling train data in method getTrainData! 
                    One of the classes has no train cases (ignored...)")
            return(trainData)
          }
          
          if(!inherits(pos,"try-error")){
            trainData <- pos
            if(na.rm){
              trainData <- trainData[(trainData$train != -1),]
            } 
            attr(trainData,"nClassType") <- "single-class"
            
            warning("An error occurred while assembling train data in method getTrainData! 
                    One of the classes has no train cases (ignored...)")
            return(trainData)
          }
      }
    }
  }
  else{ ## Processing multi-class input data --------------------------------------------------- ##
    
    # Make cross-tabulation
    # First column of xt is used as the frequency of NA values by segment (coded as -1's)
    xt <- table(rstSegmTrainDF[,1], rstSegmTrainDF[,2])
    
    # Apply majority rule
    # NOTE: If ties appear, the first column in the sequence is selected!!!
    majorityClass <- apply(xt, 1, which.max)
    trainClasses <- as.integer(colnames(xt))
    trainData <- data.frame(SID   = as.integer(rownames(xt)), 
                            train = trainClasses[majorityClass])
    
    if(na.rm){
      idx <- trainData$train != -1
      trainData <- trainData[idx,]
    } 
    
    # Filter by the fractional cover threshold of the majority class 
    # (i.e., class occupying the greatest proportion of the segment)
    if(useThresh){
      # Calculate relative frequency or fractional cover
      if(na.rm){
        xt <- xt[idx, ]
        majorityClass <- majorityClass[idx]
      } 
      
      # Calculate relative frequency (including the coverage of NA values)
      relFreq <- xt / apply(xt, 1, sum)
      
      # Select/subset segments equal or higher than the coverage threshold
      # The index within relFreq is a matrix selecting all rows and the 
      # majority class relative frequency
      trainData <- trainData[relFreq[cbind(1:length(majorityClass),majorityClass)] >= thresh,]
    }
    
    attr(trainData,"nClassType") <- "multi-class"
    return(trainData)
  }
}


#' Perform clean-up actions
#' 
#' Remove temporary files or directories. Useful for cleaning files that result from image segmentation procedures 
#' during optimization.
#' 
#' @param x A character vector containing paths to files or directories to be removed.
#' @param recursive Boolean. Should directories be deleted recursively?
#' @param silent Boolean. Should errors should be printed?
#' 
#' @details By default the \code{force} parameter in \code{\link[base]{unlink}} function 
#' (used to remove folders) is set to TRUE
#' 
#' @export

doCleanUpActions <- function(x, recursive = TRUE, silent = TRUE){
  
  for(fname in x){
    
    # Remove files
    if(!dir.exists(fname) && file.exists(fname)){
      try(file.remove(fname), silent = silent)
    }
    
    # Remove directories
    if(dir.exists(fname)){
      try(unlink(fname, recursive=recursive, force = TRUE), silent = silent)
    }
  }
}

