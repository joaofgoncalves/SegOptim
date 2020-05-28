


#' Import files to GRASS GIS
#' 
#' An auxiliary function used to import raster files into GRASS GIS using \code{r.import} command line interface. Inside R 
#' the \code{\link[base]{shell}} function is used to run the temporary a GRASS batch run file which will invoke a single batch 
#' file with \code{r.import} command.
#'
#' @param fileList A list of complete raster file paths to import to GRASS.
#' 
#' @param addParams Additional parameters or flags passed to GRASS GIS \code{r.import()} (default: "" - empty string).
#' 
#' @param verbose Print messages? (default: TRUE)
#' 
#' @param ... Further arguments passed to \code{\link[base]{shell}}.
#' 
#' @inheritParams segmentation_GRASS_RG
#' 
#' @return NULL 
#' 
#' @details 
#' By default the name of raster files imported to GRASS will have the same file basename as in \code{fileList} 
#' without the extension (check \code{\link[base]{basename}} and \code{\link[tools]{file_path_sans_ext}}).        
#'          
#' The working directory will be used to place some batch temporary files responsible to run GRASS GIS raster 
#' importer through \code{r.import()} (for more info check: \url{https://grass.osgeo.org/grass72/manuals/r.import.html}).
#'  
#' By default the function will overwrite (flag: \code{--overwrite}) any existing file with the same output name and use 
#' 1024MB of memory (parameter: \code{memory=1024}).
#' 
#' @importFrom tools file_path_sans_ext
#' @export

importToGRASS <- function(fileList,
                        GRASS.path="grass70",
                        GRASS.GISDBASE,
                        GRASS.LOCATION_NAME,
                        GRASS.MAPSET="PERMANENT", 
                        addParams="", verbose=TRUE, ...){
  
  GRASS.GISDBASE <- gsub("\\\\","/",GRASS.GISDBASE)
  GRASSinitPath <- paste(GRASS.GISDBASE,GRASS.LOCATION_NAME,GRASS.MAPSET,sep="/")
  
  for(i in 1:length(fileList)){ 
    
    fin <- fileList[i]
    fout <- tools::file_path_sans_ext(basename(fin))
    
    if(verbose) cat("-> [",i,"/",length(fileList),"] Importing file [",fin,"] to GRASS....", sep="")
    
    # Generate temp file names/paths
    tmpFileBatchJob<-paste(getwd(),"/GRASS_tmpJob_",randString(),".bat",sep="")
    tmpFileBatchRun<-paste(getwd(),"/GRASS_tmpRun_",randString(),".bat",sep="")
    
    # Create the batch file to run segmentation in GRASS GIS
    cat("r.import --overwrite input=",fin," memory=1024 output=",fout,ifelse(addParams=="","",paste(" ",trimws(addParams,"both")," ",sep="")),
        sep="", file=tmpFileBatchJob)
    
    # Creates a batch file defining the batch job and starts GRASS
    cat("set GRASS_BATCH_JOB=",tmpFileBatchJob,"\n\n",
        GRASS.path," ",GRASSinitPath," -text",
        sep="",file=tmpFileBatchRun)
    
    # Run the batch file
    shell(tmpFileBatchRun, ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE, ...)
   
    # Clean GRASS temp files for batch job and run
    doCleanUpActions(c(tmpFileBatchJob,tmpFileBatchRun), recursive=TRUE, silent=TRUE)
    
    if(verbose) cat("done.\n\n")
    
  }
}



#' Convert files to SAGA raster format
#' 
#' An auxiliar function used to convert a \code{RasterLayer}, \code{RasterStack} or any raster file (given by a file path) 
#' into SAGA .sdat/.sgrd raster format.
#' 
#' @param rst An input \code{RasterLayer}, \code{RasterStack} object or alternatively a character indicating a raster file 
#' path used for conversion.
#' 
#' @param outRasterPath The output SAGA raster .sdat file.
#'  
#' @param verbose Print messages? (default: TRUE)
#'  
#' @param ... Additional parameters passed to \code{\link[raster]{writeRaster}}.
#' 
#' @details 
#' For \code{RasterStack} objects each individual layer (or band) will be convert to a single .sdat file (this format has 
#' no support for multiband raster files). Each output file will be given a suffix _b1, _b2, ..., _bn according to the band 
#' position.
#' 
#' @importFrom raster stack
#' @importFrom raster writeRaster
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#' @export

convertToSAGA <- function(rst, outRasterPath, verbose=TRUE, ...){
  
  if(is.character(rst)){
    if(file.exists(rst)){
      rst <- raster::stack(rst)
    }else{
      stop("File in rst cannot be found! Please review parameters.")
    }
  }

  if(!inherits(rst,c("RasterLayer","RasterStack")))
    stop("rst must be an object of class RasterLayer, RasterStack or character (holding a complete file path to the input raster)")
  
  ext <- tools::file_ext(outRasterPath)
  
  if(ext != "sdat")
    stop("The extension of the output file must be .sdat! Please review input parameters.")
  
  if(inherits(rst,c("RasterLayer"))){ ## --------------------------------------------- ##
    
    if(verbose) cat("-> Converting RasterLayer object.... ")
    raster::writeRaster(rst, outRasterPath, format="SAGA", ...)
    if(verbose) cat("done.\n\n")
  
    }else if(inherits(rst,c("RasterStack"))){ ## -------------------------------------- ##
      
      if(verbose) cat("||| Converting RasterStack object |||\n\n")
      
      for(i in 1:nlayers(rst)){
        
        if(verbose) cat("-> Converting band/layer b",i," .....",sep="") 
        fn <- paste(tools::file_path_sans_ext(outRasterPath),"_b",i,".",ext,sep="")
        raster::writeRaster(rst[[i]], fn, format="SAGA", ...)
        cat("done.\n\n")
    }
  }
}






