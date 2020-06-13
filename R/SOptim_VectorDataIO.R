

#' Convert a raster dataset into polygon 
#' 
#' This utility function uses gdal_polygonize.py to create vector polygons for all 
#' connected regions of pixels in the raster sharing a common pixel value. Each 
#' polygon is created with an attribute indicating the pixel value of that polygon. 
#' A raster mask may also be provided to determine which pixels are eligible for processing. 
#' This function will create the output vector datasource if it does not already exist. 
#' The python interface uses the GDALPolygonize function which details can be foud here:  
#' \url{https://gdal.org/programs/gdal_polygonize.html}. The output format is ESRI Shapefile.
#' 
#' @param x An input path to the raster dataset that will be converted to polygon coverage 
#' or a \code{Raster.*} object.
#' 
#' @param outshape Output shapefile path. Note that the output is by deafult an ESRI shapefile
#' 
#' @param pyGDALpolygonize File path to the gdal_polygonize.py script file
#' 
#' @param pythonPath Folder path to the python executable (e.g., \code{"C:/Anaconda2"})
#'  
#' @param readpoly Read the polygon output data into R? (default: TRUE) 
#' 
#' @param readAs Read as? If \code{readpoly = TRUE} then data may be read as simple features 
#' (option: "sf" the deafult) or as sp objects (option: "sp")
#' 
#' @param verbose Print messages? (default: TRUE)
#' 
#' @param stringsAsFactors Read text data as factors? (default: FALSE)
#' 
#' @return An \code{sf} or \code{sp} object, or \code{NULL} if \code{readpoly = FALSE}
#' 
#' @importFrom raster writeRaster
#' @importFrom rgdal readOGR
#' @importFrom sf st_read
#' @importFrom methods is
#' 


gdal_polygonizeR <- function(x, outshape = NULL, pyGDALpolygonize = NULL, pythonPath = NULL, 
                             readpoly = TRUE, readAs = "sf", verbose = TRUE, stringsAsFactors = FALSE) {
  
  ## Internally assumes that ESRI Shapefile format is going to be used
  gdalformat <- 'ESRI Shapefile'
  
  # Use either rgdal or sf (simple featue package)
  #if (readpoly && readAs == "sp") require(rgdal)
  #else if (readpoly && readAs == "sf") require(sf)
  
  if (is.null(pyGDALpolygonize)) pyGDALpolygonize <- Sys.which('gdal_polygonize.py')
  if (!file.exists(pyGDALpolygonize)) stop("Can't find gdal_polygonize.py on your system.")
  
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pyGDALpolygonize))
  
  if (!is.null(outshape)) {
    
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else{
    outshape <- tempfile()
  } 
  
  
  if (is(x, 'Raster')) {
    # If this is a raster object then temporarily write to disk
    #require(raster)
    
    f <- tempfile(fileext='.tif')
    raster::writeRaster(x, f, datatype = "INT4U")
    rastpath <- normalizePath(f)
    
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } 
  else{
    stop('x must be a file path (character string), or a Raster object.')
  } 
  
  # Execute command line to rasterize the input
  system2(ifelse(is.null(pythonPath),'python',paste(pythonPath,"python",sep="/")), 
          args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
          pyGDALpolygonize, rastpath, gdalformat, outshape)))
  
  # Read output data as an sp object
  if(readpoly && readAs == "sp") {
    
    if(verbose) cat("\nReading data from ESRI Shapefile....")
    shp <- rgdal::readOGR(dirname(outshape), layer = basename(outshape), verbose=verbose)
    if(verbose) cat("done.\n\n")
    
    return(shp)
    
  # Read output data as an sf object
  }else if(readpoly && readAs == "sf"){
    
    if(verbose) cat("\nReading data from ESRI Shapefile....")
    shp <- sf::st_read(paste(outshape, ".shp", sep=""), quiet = !verbose, 
                   stringsAsFactors = stringsAsFactors)
    if(verbose) cat("done.\n\n")
    return(shp)
    
  }else{
    return(NULL)
  }
}
