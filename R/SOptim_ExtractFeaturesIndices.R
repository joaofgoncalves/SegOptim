
#' Calculate Normalized Difference Index (NDI) combinations
#' 
#' Calculates (all or some) NDI combinations based on an input SpatRaster stack with n>1 layers. Combinations are 
#' obtained using the \code{\link[utils]{combn}} function (for combinations of n layers, taken 2 
#' at a time). 
#' 
#' @param rst An input \code{SpatRaster} object with n > 1 layers.
#' 
#' @param bandNames A character vector defining band names (e.g., c("Bl","Gr","Rd","NIR")).
#' 
#' @param subsetBands An integer vector defining which bands to be used.
#' 
#' @param getSpatRaster Should the function return a final \code{SpatRaster} object 
#' with all NDI combinations? (default: TRUE) If many layers exist in rst then it is advisable 
#' that this option is set to FALSE otherwise lack of memory problems may happen.
#' 
#' @param scale10k Should data be scaled/multiplied by a factor of 10000 (default: FALSE). If 
#' TRUE then the raster data type is set to "INT4S" (otherwise "FLT4S" is used). Used to prevent 
#' out-of-memory problems.
#' 
#' @param filename A base filename used to export NDI combinations as a multiband (if 
#' writeSingleBandRaster=FALSE) or single-band (if otherwise).
#' 
#' @param writeSingleBandRaster If an output filename is defined should each 
#' NDI raster be written separetely? (default: FALSE) This is used to save up memory 
#' by not having to stack up each NDI raster. If writeSingleBandRaster is set to TRUE 
#' then a filename must be defined. The final filename will have the band indices or names 
#' as suffix.
#'   
#' @param verbose Should the function print out progress messages? (default: TRUE)
#' 
#' @param ... Additional arguments passed to 
#' 
#' @return A \code{SpatRaster} object is returned if \code{getSpatRaster=TRUE}. 
#' 
#' @importFrom utils combn 
#' @importFrom utils txtProgressBar
#' @importFrom utils write.table
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @importFrom terra writeRaster
#' @importFrom terra rast
#' @importFrom terra datatype
#' @importFrom terra nlyr
#' 
#' @export

calculateNDIcombinations <- function(rst, bandNames = NULL, subsetBands = NULL, getSpatRaster = TRUE, scale10k = FALSE,
                                   filename = NULL, writeSingleBandRaster = FALSE, verbose = TRUE, ...){
  
  if(!inherits(rst, "SpatRaster"))
    stop("rst must be a SpatRaster object!")
    
  if(writeSingleBandRaster && is.null(filename))
    stop("filename must be defined if writeSingleBandRaster is set to TRUE!")
  
  n <- terra::nlyr(rst)
  
  if(n==1)
    stop("rst must have at least 2 bands!")
  
  if(!is.null(bandNames))
    if(length(bandNames) != n)
      stop("The number of band names is different from the number of actual bands! Please verify")
  
  # Get band combinations (order does not matter)
  ndi_combns <- utils::combn(1:n, 2)
  
  if(!is.null(subsetBands)){
    # Changed to & AND condition after veifying the mistake...
    ndi_combns <- ndi_combns[,((ndi_combns[1,] %in% subsetBands) & (ndi_combns[2,] %in% subsetBands))]
  }

  nc <- ncol(ndi_combns)
  rstNames <- vector(length = nc, mode = "character")
  if(verbose) pbar <- utils::txtProgressBar(min = 1, max = nc, style = 3)
  
  if(verbose) cat("|| Calculating the following",nc,"band combinations ||\n\n")
  if(verbose) cat(paste("(",apply(t(ndi_combns),1,paste,collapse=","),")",sep=""),"\n\n", sep=" | ")
  if(verbose) cat("|| Calculating NDI combinations ||\n\n")
  
  for(i in 1:nc){
    
    b1 <- ndi_combns[1, i]
    b2 <- ndi_combns[2, i]
    
    # Make NDI calculations
    NDI_TMP <- (rst[[b1]] - rst[[b2]]) / (rst[[b1]] + rst[[b2]])
    
    if(scale10k){
      NDI_TMP <- NDI_TMP * 10000
      #terra::datatype(NDI_TMP) <- "INT4S"
    }
      
    if(getSpatRaster){
      if(i==1){
        NDI_STACK <- NDI_TMP
      }else{
        NDI_STACK <- c(NDI_STACK, NDI_TMP)
      }
    }

    if(is.null(bandNames)) rstNames[i] <- paste("NDI_b",b1,"__b",b2,sep="")
    else rstNames[i] <- paste("NDI_",bandNames[b1],"__",bandNames[b2],sep="")
    
    if(writeSingleBandRaster){
      terra::writeRaster(NDI_TMP, 
                  filename = paste(tools::file_path_sans_ext(filename),rstNames[i],
                                   tools::file_ext(filename),sep="."), 
                  datatype = ifelse(scale10k,"INT4S","FLT4S"), ...)
    }
    
    if(verbose) utils::setTxtProgressBar(pbar, i)
  }
  
  if(!is.null(filename) && !writeSingleBandRaster){
    if(verbose) cat("\n\n|| Writing multi-band raster data... ||\n")
    terra::writeRaster(NDI_STACK, filename, datatype = ifelse(scale10k,"INT4S","FLT4S"), ...)
    utils::write.table(rstNames, file = paste(tools::file_path_sans_ext(filename),"bna",sep="."), 
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    if(verbose) cat("done.\n\n")
  }
  
  if(getSpatRaster){
    # Set raster names: NDI_bi_bj
    names(NDI_STACK) <- rstNames
    return(NDI_STACK)
  }else{
    return(TRUE)
  } 
}

