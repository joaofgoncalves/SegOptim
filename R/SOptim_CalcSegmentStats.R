
# TODO: Replace raster::canProcessInMemory!

#' Calculate aggregated statistics by each image segment or region
#' 
#' Calculate statistics for each element (e.g., a segment, object or region) based on \code{data.table} objects 
#' (much faster processing!).
#' 
#' @param x An object of class \code{data.frame}, \code{matrix} or \code{SpatRaster} 
#' containing data (e.g., classification features).
#' 
#' @param z An object of class \code{SpatRaster}, \code{vector}, or a one column \code{data.frame}/\code{matrix} 
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
#' @importFrom terra values
#' @importFrom terra rast
#' @importFrom terra compareGeom
#' 

zonalDT <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) UseMethod("zonalDT", x)

#' @rdname zonalDT
#' @export
#' 

zonalDT.default <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  stop("zonalDT allows only objects of type data.frame or SpatRaster")


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
  
  if(inherits(z,"SpatRaster")){
    # Get data from raster object
    zones <- as.integer(terra::values(z))
    
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
         It must be either a SpatRaster, integer vector, or a one column data.frame/matrix")
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

zonalDT.SpatRaster <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) {
  
  # Check and get function by name
  if(!is.function(fun) & is.character(fun)){
    fun <- match.fun(fun)
  }else if(!is.function(fun) & !is.character(fun)){
    stop("Invalid input in fun! Must be either a function object or a function name.")
  }
  
  # Compare rasters 
  if(inherits(z,"SpatRaster")){
    if(!terra::compareGeom(x, z, stopOnError = FALSE, messages = TRUE))
      stop("Different rasters in x and z!")
  }
  
  if(inherits(z,"SpatRaster")){
    # Get data from raster object
    zones <- as.integer(terra::values(z))
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
         It must be either a SpatRaster, integer vector, or a one column data.frame/matrix")
  }
  
  
  # Create a data.table object by reference to avoid memory overloads
  #vals <- data.table::setDT(raster::values(x))
  vals <- terra::values(x)
  
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

zonalDT.matrix <- function(x, z, fun, na.rm = TRUE, subset = NULL, ...) 
  zonalDT.data.frame(x, z, fun, na.rm = TRUE, subset, ...)


#' Aggregate data by segment ID
#'
#' An ancillary function used to aggregate data by segment ID (field: SID) for multiple 
#' functions
#' 
#' @param x A data.frame or matrix object.
#' 
#' @param funs Function names (default: \code{c("mean","sd")}).
#' 
#' @param na.rm Remove NA's? This will be passed to aggregation functions defined 
#' (default: TRUE).
#' 
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_all
#' @importFrom dtplyr lazy_dt
#' @importFrom rlang .data 
#' 

aggregateMultiStats <- function(x, funs = c("mean","sd"), na.rm = TRUE){
  
  # Get all functions in a list object
  funList <- sapply(X = funs, match.fun)
  
  # Create a "lazy" data.table for use with dplyr verbs
  x <- dtplyr::lazy_dt(x)
  
  # Do aggregation statistics by segment ID
  out <- x %>% 
    dplyr::group_by(.data$SID) %>% 
    dplyr::summarise_all(.funs = funList, na.rm = na.rm) %>%
    as.data.frame()
  
  return(out)
}





#' Calculate segment statistics for multiple raster features and aggregation functions
#' 
#' This function allows calculating aggregation statistics by segment ID to be used in the 
#' classification stage. This function employs \code{dtplyr} (based on \code{data.table}) 
#' and \code{dplyr} verbs for processing.
#' 
#' @param rstFeatures A string defining the path to the raster features or a 
#' \code{SpatRaster} object.
#' 
#' @param rstSegm A string defining the path to the raster with segment IDs or a 
#' \code{SpatRaster} object.  
#' 
#' @param funs A character vector with the name(s) of the functions used to aggregate 
#' data (default: \code{c("mean", "sd")}).
#' 
#' @param na.rm Boolean defining if NA's should be removed (default: TRUE).
#' 
#' @param subset A vector defining which segments should be selected.
#' 
#' @param bylayer Calculate statistics layer by layer instead of all at once? (slightly 
#' increases computation time but spares memory load; default: FALSE).
#' 
#' @param tiles Number of times to slice the SpatRaster across row 
#' and column direction. The total number of tiles will be given by: 
#' \eqn{N_{tiles} = nd^{2}}.
#' 
#' @param progressBar Boolean. Show progress bar? (default: FALSE).
#'    
#' @return A data frame containing segment ids (first column, SID) and aggregated features. 
#' The name of the aggregation function will be appended to the name of each raster feature. 
#' 
#' 
#' @export
#' @importFrom terra rast
#' @importFrom terra values
#' @importFrom terra nlyr
#' @importFrom terra compareGeom
#' @importFrom terra readStart
#' @importFrom terra readStop
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom rlang .data 
#' 

# MODIFIED:
# @param tiles An object of class \code{SOptim.Tiles} created by \code{\link{createRasterTiles}} 
# used to read data fractionally by tiles (default: NULL, i.e. not used for).



calculateSegmentStats <- function(rstFeatures, rstSegm, funs = c("mean", "sd"), 
                                  na.rm = TRUE, bylayer = FALSE, tiles = NULL, 
                                  subset = NULL, progressBar = FALSE){
  
  
  # Check if both bylayer and tiles options are "active" which should not happen
  if(bylayer && !is.null(tiles)){
    stop("Select only one processing option, either bylayer or tiles!")
  }
  
  # Check if funs argument is a character vector
  if(!is.character(funs))
    stop("The funs argument (used to calculate segment statistics) must be a character vector!")
  
  # If rstFeatures or rstSegm are strings then first load raster metadata
  if(is.character(rstFeatures)) rstFeatures <- terra::rast(rstFeatures)
  if(is.character(rstSegm)) rstSegm <- terra::rast(rstSegm)
  
  # Compare rasters 
  if(!terra::compareGeom(rstFeatures, rstSegm, stopOnError = FALSE, messages = TRUE))
    stop("Different rasters defined in rstFeatures and rstSegm!")
  
  # Process data by tiles
  if(!is.null(tiles)){
    
    # Force the number of tiles as an integer number
    tiles <- as.integer(tiles)
    
    if(!inherits(tiles,"SOptim.Tiles")){
      #stop("Object in tiles must be of class SOptim.Tiles!")
      
      if(!is.integer(tiles))
         stop("The number of tiles across x and y must be an integer number")
      
      if(tiles <= 0)
        stop("tiles input parameter must be greater than 0")
      
      # Create a SOptim.Tiles object 
      nd <- tiles
      tiles <- createRasterTiles(rstFeatures, nd = nd)
      
    }
    
    if(progressBar){
      pb <- utils::txtProgressBar(min = 1, max = length(tiles), style = 3)
    }
    
    # Stack segment IDs and raster features
    rstStack <- c(rstSegm, rstFeatures)
    
    # Open read connection
    #rstStack <- terra::readStart(rstStack)
    suppressMessages(suppressWarnings(
      terra::readStart(rstStack)))
    
    # Iterate by tile
    for(tile_i in 1:length(tiles)){
      
      # Read raster data by tile
      tileRstFeatDF <- readDataByTile(rst       = rstStack, 
                                      tiles     = tiles, 
                                      tileIndex = tile_i, 
                                      as.df     = TRUE)
      # Set column names
      colnames(tileRstFeatDF) <- c("SID", names(rstFeatures))
      
      # Subset for target segment IDs
      if(!is.null(subset)){
        tileRstFeatDF <- dtplyr::lazy_dt(tileRstFeatDF, key_by = "SID")
        tileRstFeatDF <- tileRstFeatDF %>% dplyr::filter(.data$SID %in% subset)
      }
      
      # Calculate aggregate statistics for all functions for a given tile i
      tmpAggStats <- aggregateMultiStats(tileRstFeatDF, funs = funs, na.rm = na.rm)
      
      # Bind aggregated data by tile
      if(tile_i == 1){
        outputDF <- tmpAggStats
      }else{
        outputDF <- dplyr::bind_rows(outputDF, tmpAggStats)
      }
      
      if(progressBar){
        utils::setTxtProgressBar(pb, tile_i)
      }
    }
    
    # Close read connection
    #rstStack <- terra::readStop(rstStack)
    terra::readStop(rstStack)
    
    # Do a final aggregation calculating the mean value for 
    # This will merge segments that get split across different tiles
    outputDF <- aggregateMultiStats(outputDF, funs = "mean")
    
  }else{ 
    
    # Run all the data!
    
    if(!bylayer){
      
      # Check memory requirements to handle the raster data   
      # if(!raster::canProcessInMemory(raster::stack(rstSegm, rstFeatures), n = 2))
      #   stop("Not enough memory to process the input raster files! Set bylayer = TRUE or use the tiles option.")
      
      # Read data from raster files
      rDT <- terra::values(c(rstSegm, rstFeatures))
      colnames(rDT) <- c("SID", names(rstFeatures))
      
      # Subset data only to segments defined in subset?
      if(!is.null(subset)){
        rDT <- dtplyr::lazy_dt(rDT, key_by = "SID")
        rDT <- rDT %>% dplyr::filter(.data$SID %in% subset)
      }
      
      outputDF <- aggregateMultiStats(rDT, funs = funs, na.rm = na.rm)
      
    }else{
      
      # Run by layer reading
      
      # Check memory requirements to handle the raster data    
      # if(!raster::canProcessInMemory(raster::rast(rstSegm, rstFeatures[[1]]), n = 2))
      #   stop("Not enough memory to process the input raster files when using option bylayer!")
      
      nl <- terra::nlyr(rstFeatures)
      
      if(progressBar){
        pb <- utils::txtProgressBar(1, nl, style=3)
      }
      
      
      ## Iterate through each layer and generate statistics by segment
      ##
      for(lyr in 1:nl){
        
        # Read data from raster files for a given layer
        lyrname <- names(rstFeatures)[lyr]
        rDT <- terra::values(c(rstSegm, rstFeatures[[lyr]]))
        colnames(rDT) <- c("SID", lyrname)
        
        # Subset data only to segments defined in subset?
        if(!is.null(subset)){
          rDT <- dtplyr::lazy_dt(rDT,key_by = "SID")
          rDT <- rDT %>% dplyr::filter(.data$SID %in% subset)
        }
        
        tempDF <- aggregateMultiStats(rDT, funs = funs, na.rm = na.rm)
        colnames(tempDF) <- c("SID", paste(lyrname, funs, sep="_"))
        
        # Merge/Join data for each layer calculated based on segment IDs (col: SID)
        if(lyr == 1){
          outputDF <- tempDF
        }else{
          outputDF <- dtplyr::lazy_dt(outputDF, key_by = "SID")
          outputDF <- outputDF %>% 
            dplyr::left_join(y = tempDF, by = "SID") %>% 
            as.data.frame()
        }
        
        if(progressBar)
          utils::setTxtProgressBar(pb, lyr)
        
      } # End by layer loop
    }
    
  }
  
  return(outputDF)
  
}


