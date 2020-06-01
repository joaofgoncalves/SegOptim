
## ---------------------------------------------------------------------------------------------------------------- ##
## --- SEGMENTATION FUNCTIONS ----------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------------- ##


#' Names of segmentation parameters that will be optimized
#' 
#' Outputs a list containing the names of segmentation parameters that will be optimized by 
#' the package for each available segmentation algorithm: 
#' \itemize{
#'    \item SAGA_SRG - SAGA Seeded Region Growing, 
#'    \item GRASS_RG - GRASS Region Growing, 
#'    \item ArcGIS_MShift - Mean Shift algorithm from ArcGIS, 
#'    \item TerraLib_Baatz and TerraLib_MRGrow - Baatz algorithm and Mean Region Growing from TerraLib,  
#'    \item RSGISLib_Shep - Shepherd's algorithm from RSGISLib,and,
#'    \item Orfeo Toolbox (OTB) - Large-scale Mean-shift (single and two parameter set)
#'    
#' }
#' 
#' @return Returns the parameter names as \code{list} object.
#'  
#' @seealso 
#' \itemize{
#'    \item \code{\link{segmentation_SAGA_SRG}}, 
#'    \item \code{\link{segmentation_GRASS_RG}}, 
#'    \item \code{\link{segmentation_ArcGIS_MShift}}, 
#'    \item \code{\link{segmentation_Terralib_Baatz}}, 
#'    \item \code{\link{segmentation_Terralib_MRGrow}}, 
#'    \item \code{\link{segmentation_RSGISLib_Shep}},
#'    \item \code{\link{segmentation_OTB_LSMS}},
#'    \item \code{\link{segmentation_OTB_LSMS2}}
#' }
#' 
#' @export

segmentationParamNames <- function(){
  
  segmOptimParamList<-
    list(
      SAGA_SRG        = c("Bandwidth",
                          "GaussianWeightingBW",
                          "VarFeatSpace",
                          "VarPosSpace"),
      
      GRASS_RG        = c("Threshold",
                          "MinSize"),
      
      ArcGIS_MShift   = c("SpectralDetail",
                          "SpatialDetail",
                          "MinSegmentSize"),
      
      Terralib_Baatz  = c("CompactnessWeight",
                          "SpectralWeight",
                          "Threshold",
                          "MinSize"),
      
      Terralib_MRGrow = c("Threshold",
                          "MinSize") ,
      
      RSGISLib_Shep   = c("NumClust",
                          "MinSize",
                          "SpectralThresh"),
      
      OTB_LSMS   =      c("SpectralRange",
                          "SpatialRange",
                          "MinSize"),
      
      OTB_LSMS2   =      c("MS_SpectralRange",
                          "MS_SpatialRange",
                          "LSS_SpectralRange",
                          "LSS_SpatialRange",
                          "MinSize")
      )
  
  return(segmOptimParamList)
}


#' SAGA Seeded Region Growing segmentation algorithm
#' 
#' A function to access SAGA's Seeded Region Growing segmentation algorithm 
#' through CLI and optimize some of its parameters using genetic algorithms. The segmentation algorithm has two 
#' stages: 1) seed generation and, 2) seeded region growing.
#' 
#' @param x A vector containing the parameters to be optimized by package GA: 
#' \itemize{
#'  \item [1] BAND_WIDTH - bandwidth (cells; default: 10), 
#'  \item [2] DW_BANDWIDTH - Gaussian and Exponential Weighting Bandwidth (default: 5) (both for seed generation in stage 1) and, 
#'  \item [3] SIG_1 - variance in feature space (default: 1), 
#'  \item [4] SIG_2 - variance in position space (default: 1) (both for region growing - stage 2).#' 
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{Bandwidth}, \code{GaussianWeightingBW}, \code{VarFeatSpace} and 
#' \code{VarPosSpace}. 
#' 
#' @param rstList File paths to the SAGA header files (format: .sgrd) used to perform image segmentation.
#' File names should be provided either by a single string with each file separated by a ';'
#' (e.g. \code{red_band.sgrd;green_band.sgrd;nir_band.sgrd}), or a vector of strings with one file name
#' for each element (e.g. resulting from a \code{list.files} call). 
#' 
#' @param outputSegmRst A path to the output segmented image in SAGA .sdat/.srgd format (default: \code{NULL}; in this 
#' case the segmenter will use the working directory).
#' 
#' @param Bandwidth Segmentation bandwidth.
#' 
#' @param GaussianWeightingBW Gaussian and Exponential Weighting Bandwidth. 
#' 
#' @param VarFeatSpace Variance in feature space.
#' 
#' @param VarPosSpace Variance in position space.
#' 
#' @param seedType Seed type options: [0] minima of variance [1] maxima of variance (default: 0).
#' 
#' @param method1 Method 1: [0] band width smoothing [1] band width search (default: 0).
#' 
#' @param DWeighting Weighting function: [0] no distance weighting [1] inverse distance to a power [2] exponential 
#' [3] gaussian weighting (default: 3) - note: only exponential or Gaussian will work! Otherwise it will fail to 
#' optimize DW_BANDWIDTH parameter.
#' 
#' @param normalize Normalize inputs? [0] No [1] Yes (default: 0).
#' 
#' @param neighbour Select the neighbourhood type: [0] 4 connected pixels (von Neumann), [1] 8 connected pixels 
#' (Moore) (default is 0).
#' 
#' @param method2 Method 2: [0] feature space and position [1] feature space (default: 0).
#' 
#' @param thresh Similarity threshold (default: 0).
#' 
#' @param leafSize Leaf size (for speed optimisation) (default: 1024).
#' 
#' @param SAGApath Path to SAGA command-line interface (default: NULL; meaning that PATH env variable is used).
#' 
#' @param verbose Output messages? (default: TRUE)
#' 
#' @details 
#' Only accepts SAGA raster file format (problems encontered during segmentation when using other file formats).       
#' Only parameters in \code{x} will be optimized while the remaining will maintain their default values.     
#'  
#' This function uses the following SAGA commands: \emph{saga_cmd imagery_segmentation 2} and 
#' \emph{saga_cmd imagery_segmentation 3}. See more info at:         
#' \url{http://www.saga-gis.org/saga_tool_doc/3.0.0/imagery_segmentation_2.html}      
#' \url{http://www.saga-gis.org/saga_tool_doc/3.0.0/imagery_segmentation_3.html}      
#' \url{http://www.saga-gis.org/saga_tool_doc/}
#' 
#' @references 
#' Adams, R. & Bischof, L. (1994): Seeded Region Growing. IEEE Transactions on Pattern Analysis 
#' and Machine Intelligence, Vol.16, No.6, p.641-647.     
#'       
#' Bechtel, B., Ringeler, A. & Boehner, J. (2008): Segmentation for Object Extraction of Trees 
#' using MATLAB and SAGA. In: Boehner, J., Blaschke, T., Montanarella, L. [Eds.]: SAGA - Seconds 
#' Out. Hamburger Beitraege zur Physischen Geographie und Landschaftsoekologie, 19:59-70.      
#' \href{http://downloads.sourceforge.net/saga-gis/hbpl19_01.pdf}{download}
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' (these files will be cleaned after each GA iteration). 
#' 
#' @importFrom tools file_ext
#' 
#' @export


segmentation_SAGA_SRG <- function(x, rstList, outputSegmRst=NULL, 
                                  Bandwidth=NULL, GaussianWeightingBW=NULL, VarFeatSpace=NULL, VarPosSpace=NULL, 
                                  seedType=0, method1=0, DWeighting=3, normalize=0, neighbour=0, 
                                  method2=0,thresh=0, leafSize=1024, SAGApath=NULL, verbose=TRUE){

  if(missing(x)){
    if(is.null(Bandwidth) || is.null(GaussianWeightingBW) || is.null(VarFeatSpace) || is.null(VarPosSpace)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: Bandwidth, GaussianWeightingBW, VarFeatSpace and VarPosSpace! Check documentation for 
           more details.")
    }else{
      x<-c(Bandwidth, GaussianWeightingBW, VarFeatSpace, VarPosSpace)
    }
  }
  
  # Generate file names (user-defined or temporary)
  if(!is.null(outputSegmRst)){
    
    extOutFile <- tools::file_ext(outputSegmRst)
    if(!(extOutFile %in% c("sgrd","sdat")))
      stop("The output file extension in outputSegmRst must be SAGA binary format: .sdat or .sgrd")
    
    fname <- gsub(".sgrd$","",basename(outputSegmRst))
    dname <- dirname(outputSegmRst)
    tmpFile.seeds <- paste(dname,"/",fname,"_seeds.sgrd",sep="")
    tmpFile.var   <- paste(dname,"/",fname,"_var.sgrd",sep="")
    tmpFile.segm  <- outputSegmRst
  }else{
    tmpFile.seeds<-repBSlash(tempfile("saga_seeds_",getwd(),".sgrd"))
    tmpFile.var<-repBSlash(tempfile("saga_var_",getwd(),".sgrd"))
    tmpFile.segm <- repBSlash(tempfile("saga_segm_",getwd(),".sgrd"))
  }
  
  
  # If binary SAGA path is defined then use to make the command line
  if(is.null(SAGApath)){
    prefix<-""
  }else{
    prefix<-paste(SAGApath,"/",sep="")
  }
  
  # If rstList is a vector with length > 1, then collapse into a single string
  if(length(rstList) > 1){
    rstList <- paste(rstList, collapse=';')
  }
  
  # Create SAGA comand line strings
  # Stage 1 - seed generation
  segmSAGA_Stage1<-paste(prefix,"saga_cmd imagery_segmentation 2 ",
                         "-FEATURES \"",rstList,"\" ",
                         "-VARIANCE ",tmpFile.var," ",
                         "-SEED_GRID ",tmpFile.seeds, " ",
                         "-SEED_TYPE ",seedType," ",
                         "-METHOD ",method1," ", 
                         "-BAND_WIDTH ",x[1]," ",
                         "-NORMALIZE ",normalize," ",
                         "-DW_WEIGHTING ",DWeighting," ",
                         "-DW_BANDWIDTH ",x[2],
                         sep="") 
  
  # Stage 2 - image segmentation
  segmSAGA_Stage2<-paste(prefix,"saga_cmd imagery_segmentation 3 ",
                         "-FEATURES \"",rstList,"\" ", 
                         "-SEEDS ",tmpFile.seeds," ",
                         "-SEGMENTS ",tmpFile.segm," ",
                         "-NORMALIZE ",normalize," ",
                         "-NEIGHBOUR ",neighbour," ",
                         "-METHOD ",method2," ",
                         "-SIG_1 ",x[3], " ",
                         "-SIG_2 ",x[4]," ",
                         "-THRESHOLD ",thresh," ",
                         "-LEAFSIZE ",leafSize,
                         sep="")
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="SAGA_SRG")
  
  # Run SAGA commands
  #
  if(.Platform$OS.type=="windows"){
    system(segmSAGA_Stage1,ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE)
    system(segmSAGA_Stage2,ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE)
  }else{
    system(segmSAGA_Stage1,ignore.stdout = TRUE, ignore.stderr = TRUE)
    system(segmSAGA_Stage2,ignore.stdout = TRUE, ignore.stderr = TRUE)
  }
  
  out<-list(

    segm=gsub(".sgrd",".sdat",tmpFile.segm),
    segm.sgrd=tmpFile.segm,
    segm.meta1=gsub(".sgrd",".prj",tmpFile.segm),
    segm.meta1=gsub(".sgrd",".sdat.aux.xml",tmpFile.segm),
    segm.meta1=gsub(".sgrd",".mgrd",tmpFile.segm),
    
    seeds=gsub(".sgrd",".sdat",tmpFile.seeds),
    seeds.sgrd=tmpFile.seeds,
    seeds.meta1=gsub(".sgrd",".prj",tmpFile.seeds),
    seeds.meta2=gsub(".sgrd",".sdat.aux.xml",tmpFile.seeds),
    seeds.meta3=gsub(".sgrd",".mgrd",tmpFile.seeds),
    
    var=gsub(".sgrd",".sdat",tmpFile.var),
    var.sgrd=tmpFile.var,
    var.meta1=gsub(".sgrd",".prj",tmpFile.var),
    var.meta2=gsub(".sgrd",".sdat.aux.xml",tmpFile.var),
    var.meta3=gsub(".sgrd",".mgrd",tmpFile.var)
  )
  
  class(out) <- "SOptim.SegmentationResult"
  
  return(out)
  
}



#' GRASS Region Growing image segmentation
#' 
#' A function providing CLI access to GRASS Region Growing image segmentation algorithm 
#' and optimize parameters using genetic algorithms. To use this function, image features used in 
#' segmentation must be imported into a GRASS database (\code{\link{importToGRASS}}).
#' 
#' @param x A vector with size two defining the values of segmentation parameters optimized by GA: 
#' \itemize{
#'   \item [1] threshold (float) - difference threshold between 0 and 1. Threshold: 0 merges only 
#' identical segments; threshold: 1 merges all. 
#'   \item [2] minimum number of cells in a segment (integer) - the final step will merge small 
#'  segments with their best neighbor; options: 1-100000 (default: 1).
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{Threshold} and \code{MinSize}. 
#' 
#' @param GRASS.path Defines the initialization path to GRASS .bat file for windows (for example: `C:/GRASS/grass70`) 
#' or the .sh or .py file for Linux (usually this should be set to `/usr/bin/grass`). 
#' The default is "grass70" which assumes that GRASS is in the path environmental variables and version 
#' 7.0.x (which may not be appropriate in most cases and therefore this parameter should be set explicitly).
#' 
#' @param GRASS.inputRstName Name of the raster dataset that will be segmented (typically a 
#' multi-layered raster dataset with segmentation features in each band).
#' 
#' @param GRASS.GISDBASE Name of GRASS GIS database.
#' 
#' @param GRASS.LOCATION_NAME Name of GRASS location name (default: "demolocation").
#' 
#' @param GRASS.MAPSET Name of GRASS mapset (default: "PERMANENT").
#' 
#' @param outputSegmRst A path to the output segmented image exported by GRASS \code{r.out.gdal}. The output file can 
#' be in any of GDAL formats  (default: \code{NULL}; in this case the segmenter will internally determine a temporary 
#' output folder and file path inside the working directory). 
#' 
#' @param Threshold Difference threshold between 0 and 1. Threshold: 0 merges only identical segments; 
#' threshold: 1 merges all
#' @param MinSize Minimum number of cells in a segment (integer) - the final step will merge small 
#' segments with their best neighbor
#' 
#' @param memory Memory used to run i.segment (default: 1024Mb).
#' 
#' @param iterations Maximum number of iterations (default: 20).
#' 
#' @param verbose Output messages? (default: TRUE)
#' 
#' @details 
#' This function uses a batch file defining the batch job (GRASS_BATCH_JOB) and starts GRASS with the 
#' appropriate parametrization. The \code{i.segment} command is used to run the segmentation.    
#' The similarity calculation method uses the default value ("euclidean").     
#' See more info at GRASS manual pages online:
#' \code{i.segment} at \url{https://grass.osgeo.org/grass72/manuals/i.segment.html}   
#' Access to GRASS GIS manuals \url{https://grass.osgeo.org/documentation/manuals/}
#' 
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration. 
#' 
#' @export

segmentation_GRASS_RG <- function(x, GRASS.path="grass", GRASS.inputRstName, GRASS.GISDBASE, GRASS.LOCATION_NAME="demolocation",
                                GRASS.MAPSET="PERMANENT", outputSegmRst=NULL, Threshold=NULL, MinSize=NULL, memory=1024, 
                                iterations=20,verbose=TRUE){
  
  if(missing(x)){
    if(is.null(Threshold) || is.null(MinSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: Threshold and MinSize! Check documentation for more details.")
    }else{
      x<-c(Threshold, MinSize)
    }
  }
  
  
  GRASS.GISDBASE<-gsub("\\\\","/",GRASS.GISDBASE)
  GRASSinitPath<-paste(GRASS.GISDBASE,GRASS.LOCATION_NAME,GRASS.MAPSET,sep="/")
  
  # Generate user-defined or temporary file names/paths
  tmpFile.GRASSsegm <- gsub("\\\\|/","",tempfile("GRASS_RG_Segm_",""))
  extType <- ifelse(.Platform$OS.type == "windows",".bat",".sh")
  
  if(is.null(outputSegmRst)){
    tmpFile.batchJob <- repBSlash(tempfile("GRASS_Segm_BatchCmd_",getwd(),extType))
    tmpFile.batchRun <- repBSlash(tempfile("GRASS_Segm_BatchCmdRun_",getwd(),".bat"))
    tmpFile.outRasterSegm <- repBSlash(tempfile("GRASS_Segm_",getwd(),".tif"))
  }else{
    #tmpFile.batchJob <- repBSlash(tempfile("GRASS_Segm_BatchCmd_",dirname(outputSegmRst),extType))
    #tmpFile.batchRun <- repBSlash(tempfile("GRASS_Segm_BatchCmdRun_",dirname(outputSegmRst),".bat"))
    #  
    tmpFile.batchJob <- repBSlash(tempfile("GRASS_Segm_BatchCmd_",getwd(),extType))
    tmpFile.batchRun <- repBSlash(tempfile("GRASS_Segm_BatchCmdRun_",getwd(),".bat"))
    tmpFile.outRasterSegm <- outputSegmRst
  }

  # Create the batch file to run segmentation in GRASS GIS
  cat("g.region raster=",GRASS.inputRstName,".1@",GRASS.MAPSET,"\n\n
      i.segment --verbose --overwrite group=",GRASS.inputRstName,"@",GRASS.MAPSET," output=",tmpFile.GRASSsegm," threshold=",x[1]," minsize=",as.integer(x[2])," memory=",memory," iterations=",iterations,"\n\n
      r.out.gdal -c --overwrite input=",tmpFile.GRASSsegm,"@",GRASS.MAPSET," output=",tmpFile.outRasterSegm," format=GTiff type=UInt32\n\n
      g.remove -f -b type=raster name=",tmpFile.GRASSsegm,"@",GRASS.MAPSET,"\n\n",
      sep="", file=tmpFile.batchJob)
  
  # Creates a batch file defining the batch job and starts GRASS
  # Change the batch file head if on Linux (chmod to allow .sh file to be executed...?!)
  prefix <- ifelse(.Platform$OS.type == "windows","",paste("#!/bin/sh\nchmod +x ",tmpFile.batchJob,"\n",sep=""))
  
  # Change the command according to the OS used to set the GRASS_BATCH_JOB variable
  setWord <- ifelse(.Platform$OS.type == "windows", "set", "export")
  
  cat(prefix,
      setWord," GRASS_BATCH_JOB=",tmpFile.batchJob,"\n\n",
      GRASS.path," ",GRASSinitPath," -text",
        sep="",file=tmpFile.batchRun)
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="GRASS_RG")

  # Run the batch files
  if(.Platform$OS.type == "windows"){
    # In windows shell function apparently gives less problems...
    out <- try(shell(tmpFile.batchRun, 
                      ignore.stdout = TRUE, ignore.stderr = TRUE))
  }else{
    out <- try(system(paste("sh",tmpFile.batchRun), 
                      ignore.stdout = TRUE, ignore.stderr = TRUE))
  }
  
  # On-error clean up files
  if(inherits(out, "try-error") || out != 0){
    on.exit(doCleanUpActions(c(segm=tmpFile.outRasterSegm,
                               segmMeta=gsub(".tif",".tif.aux.xml",tmpFile.outRasterSegm),
                               batFile1=tmpFile.batchJob,
                               batFile2=tmpFile.batchRun)))

    stop("An error occurred while performing GRASS image segmentation! Check input parameters. 
         (On Linux/Mac check user permissions and perhaps try starting R (or RStudio) without root user...)")
  }else{
    out <- list(segm=tmpFile.outRasterSegm,
                segmMeta=gsub(".tif",".tif.aux.xml",tmpFile.outRasterSegm),
                batFile1=tmpFile.batchJob,
                batFile2=tmpFile.batchRun)
    
    class(out) <- "SOptim.SegmentationResult"
    
    return(out)
  }
}




#' ArcGIS Mean Shift segmentation algorithm
#' 
#' A function providing CLI access to ArcGIS Mean Shift segmentation and optimize its parameters 
#' using genetic algorithms.
#' 
#' @param x A vector with size three defining the parameters that will be optimized by GA:
#' \itemize{
#'   \item [1] spectral detail - level of importance given to the spectral differences of features in your imagery, 
#'   \item [2] spatial detail - level of importance given to the proximity between features in your imagery, and, 
#'   \item [3] minimum segment size - in pixels.
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{SpectralDetail}, \code{SpatialDetail} and \code{MinSegmentSize}. 
#' 
#' @param inputRstPath Path to the input raster file (typically a multi-layered raster dataset with 
#' segmentation features in each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this case the segmenter will 
#' internally determine a temporary output folder and file path inside the working directory)
#' 
#' @param SpectralDetail Spectral detail - level of importance given to the spectral differences of features in your imagery
#' @param SpatialDetail Spatial detail - level of importance given to the proximity between features in your imagery
#' @param MinSegmentSize Minimum segment size - in pixels
#' 
#' @param pythonPath Path to the python interpreter (default: NULL; meaning that it uses the one in the 
#' PATH variable).
#' 
#' @param verbose Print messages while running? (default: TRUE)
#' 
#' @details 
#' Some more info regarding the parameters that will be optimized:     
#' 
#' \describe{
#'     \item{\emph{Spectral detail}}{valid values range from 1.0 to 20.0 (double). A higher value is appropriate when 
#' you have features you want to classify separately but have somewhat similar spectral characteristics. 
#' Smaller values create spectrally smoother outputs. For example, with higher spectral detail in a forested 
#' scene, you will be able to have greater discrimination between the different tree species};
#' 
#'     \item{\emph{Spatial detail}}{valid values range from 1 to 20 (integer). A higher value is appropriate for a scene 
#' where your features of interest are small and clustered together. Smaller values create spatially 
#' smoother outputs. For example, in an urban scene, you could classify an impervious surface using a 
#' smaller spatial detail, or you could classify buildings and roads as separate classes using a higher 
#' spatial detail};
#' 
#'     \item{\emph{Minimum segment size}}{define the minimum size (in pixels; integer) of a given segment. This threshold is 
#' used to merge segments smaller than the defined size with their best fitting neighbor segment}.
#' }
#' 
#' @note 
#' Uses a predefined python script for accessing ArcGIS API and run the segmentation. Check 
#' \code{SegOptim/inst} folder.        
#' Each ArcGIS/Python instance works with a separate scratch directory to avoid  
#' disk I/O conflicts. However, some ArcGIS/Python undetermined problems may still occur 
#' during segmentation. 
#' 
#' @seealso ArcGIS manual on Mean Shift segmentation: 
#' \url{http://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/segment-mean-shift.htm}
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration. 
#' 
#' @export

segmentation_ArcGIS_MShift <- function(x, inputRstPath, outputSegmRst=NULL, 
                                       SpectralDetail=NULL, SpatialDetail=NULL, MinSegmentSize=NULL, 
                                       pythonPath=NULL, verbose=TRUE){
  
  
  if(missing(x)){
    if(is.null(SpectralDetail) || is.null(SpatialDetail) || is.null(MinSegmentSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: SpectralDetail, SpatialDetail and MinSegmentSize! Check documentation for more details.")
    }else{
      x <- c(SpectralDetail, SpatialDetail, MinSegmentSize)
    }
  }
  
  
  if(is.null(pythonPath)){
    prefix=""
  }else{
    prefix=paste(pythonPath,"/",sep="")
  }

  # Check if an output file was defined otherwise use a temporary
  if(!is.null(outputSegmRst)){
    tmpDirScratch <- dirname(outputSegmRst)
    tmpFileSegmOutRst <- basename(outputSegmRst)
  }else{
    tmpDirScratch <- repBSlash(tempfile("",getwd()))
    tmpFileSegmOutRst <- gsub("\\\\","",tempfile("ArcGIS_SegmIdx_","",".tif"))
  }

  # Get the python script used to perform ArcGIS Mean-shift segmentation algorithm
  PyScriptPath <- getPythonFile("ArcGIS_MShift.py",
                              "SegOptim",
                              c("D:/MyDocs/R-dev",getwd()))

  if(is.null(PyScriptPath)){
    stop("Unable to find the path to the ArcGIS python segmentation script (ArcGIS_MShift.py) in dev or pkg paths!")
  }
    
  # Assemble command string to run
  cmd <- paste(prefix,
             "python \"",PyScriptPath,"\" ",
             tmpDirScratch," ",
             "\"",inputRstPath,"\" ",
             x[1]," ",
             as.integer(x[2])," ",
             as.integer(x[3])," ",
             tmpFileSegmOutRst,
             sep="")
  
  #if(verbose) cat(cmd,"\n\n")
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="ArcGIS_MShift")
  
  ## Run python script for ArcGIS Mean-shift image segmentation from the command line
  sysOut<-try(system(cmd,ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE))
  
  if(sysOut!=0){
    on.exit(doCleanUpActions(tmpDirScratch))
    warning("Unable to perform segmentaton with function segmentation_ArcGIS_MShift!")
    return(NA)
  }
  
  
  # Append the temporary scratch directory to the the file names
  tmpFileSegmOutRst<-paste(tmpDirScratch,tmpFileSegmOutRst,sep="/")
  
  
  out <- list(
    segm=tmpFileSegmOutRst,
    segmMeta1=gsub(".tif",".tif.aux.xml",tmpFileSegmOutRst),
    segmMeta2=gsub(".tif",".tfw",tmpFileSegmOutRst),
    segmMeta3=gsub(".tif",".tif.ovr",tmpFileSegmOutRst),
    segmMeta4=gsub(".tif",".tif.vat.cpg",tmpFileSegmOutRst),
    segmMeta5=gsub(".tif",".tif.vat.dbf",tmpFileSegmOutRst),
    segmMeta6=gsub(".tif",".tif.xml",tmpFileSegmOutRst),
    tempDir = tmpDirScratch)
  
  class(out) <- "SOptim.SegmentationResult"
  
  return(out)
  
}


#' TerraLib Baatz-Shcape segmentation algorithm 
#' 
#' A function providing CLI access to TerraLib 5 Baatz-Shcape segmentation and optimize its parameters 
#' using genetic algorithms.
#' 
#' @param x A vector with size four defining the segmentation parameters that will be optimized using 
#' genetic algorithms from GA package: 
#' \itemize{
#'   \item [1] Compactness weight (double), 
#'   \item [2] Spectral / color weight (double), 
#'   \item [3] Threshold parameter (double), 
#'   \item [4] Minimum size of the segments (integer).
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{CompactnessWeight}, \code{SpectralWeight}, \code{Threshold} and 
#' \code{MinSize}.
#' 
#' @param inputRstPath Path to the input raster file (typically a multi-layered raster dataset with 
#' segmentation features in each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this case the segmenter will 
#' internally determine a temporary output folder and file path inside the working directory)
#' 
#' @param CompactnessWeight Compactness weight (double)
#' @param SpectralWeight Spectral / color weight (double)
#' @param Threshold Threshold parameter (double)
#' @param MinSize Minimum size of the segments (integer)
#' 
#' @param verbose Print output messages? (default: TRUE)
#' 
#' @param TerraLib.path Path to the TerraLib binaries (locating \code{terralib_cli_baatz} executable). 
#' If not set it uses the PATH env var to find it.
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration.  
#' 
#' @details 
#' By default all bands have the same weight (bw), i.e., bw = 1 / number of bands. Also, by default, 
#' the executable file for running this segmentation algorithm (named \code{terralib_cli_baatz}) should 
#' be accessible by the PATH env variable. 
#' 
#' @note 
#' Windows binaries for CLI access to TerraLib v5.1.1 Baatz-Schape segmenter are available at: 
#' \url{https://bitbucket.org/joao_goncalves/terralib5_cli_segmentation_mod/downloads} 
#' (unfortunetaley not yet tested in Linux environment...). Source could be used to build TerraLib for this 
#' effect, check source location \code{/examples/segmentationBaatz} and TerraLib instructions 
#' (\href{http://www.dpi.inpe.br/terralib5/wiki/doku.php?id=wiki:terralib50_build}{link}).    
#' The executable file for running this segmentation algorithm is named: \code{terralib_cli_baatz}    
#' 
#' @references 
#' Baatz, M.; Schape, A. Multiresolution segmentation: an optimization approach for high quality 
#' multi-scale image segmentation. In: XII Angewandte Geographische Informationsverarbeitung, 
#' Wichmann Verlag, Heidelberg, 2000.
#' 
#' @importFrom raster raster
#' @importFrom raster nbands
#' @export
#' 

segmentation_Terralib_Baatz <- function(x, inputRstPath, outputSegmRst=NULL, CompactnessWeight=NULL, SpectralWeight=NULL, Threshold=NULL, MinSize=NULL, 
                                      verbose=TRUE, TerraLib.path=NULL){
  
  if(missing(x)){
    if(is.null(CompactnessWeight) || is.null(SpectralWeight) || is.null(Threshold) || is.null(MinSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: CompactnessWeight, SpectralWeight, Threshold and MinSize! Check documentation for more details.")
    }else{
      x <- c(CompactnessWeight, SpectralWeight, Threshold, MinSize)
    }
  }
  
  # Output segmentation raster file
  if(is.null(outputSegmRst)){
    tmpFile.outRasterSegm <- repBSlash(tempfile("Terralib_Baatz_Segm_",getwd(),".tif")) 
  }else{
    tmpFile.outRasterSegm <- outputSegmRst
  }
  
  # Read inputRstPath metadata to extract the number of bands (required as segmentation input)
  inputRstMeta <- raster::raster(inputRstPath)
  nb <- (raster::nbands(inputRstMeta)) - 1 # Changed this because terralib starts band counting at 0
  
  if(is.null(TerraLib.path))
    TerraLib.path<-""
  else
    TerraLib.path<-paste(TerraLib.path,"/",sep="")
  
  cmd<-paste(
    TerraLib.path, "terralib_cli_baatz ", # Command line exec file
    inputRstPath," ",         # Input raster path
    0, " ",    # Band interval to use - first band index nr
    nb, " ",   # Band interval to use - last band index nr
    x[1], " ", # Compactness Weight
    x[2], " ", # Spectral / color Weight
    x[3], " ", # Threshold parameter
    x[4], " ", # Min size of the segments
    tmpFile.outRasterSegm, # Output file
    sep=""
  )
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="Terralib_Baatz")
  
  ## Run command line
  sysOut<-try(system(cmd,ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE))

  
  if(!inherits(sysOut,"try-error") && sysOut == 0){
    
    out <- list(segm=tmpFile.outRasterSegm)
    class(out) <- "SOptim.SegmentationResult"
    return(out)
    
  }else{
    return(NA)
  }
  
}


#' TerraLib Mean Region Growing segmentation algorithm
#' 
#' A function providing CLI access to TerraLib 5 Mean Region Growing segmentation and optimize 
#' its parameters using genetic algorithms.
#' 
#' @param x A vector with size two defining the segmentation parameters that will be optimized using 
#' genetic algorithms from GA package: 
#' \itemize{
#'   \item [1] Threshold (double), 
#'   \item [2] Minimum size of the segments (integer).
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{Threshold} and \code{MinSize}.
#' 
#' @param inputRstPath Path to the input raster file (typically a multi-layered raster dataset with 
#' segmentation features in each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this case the segmenter will 
#' internally determine a temporary output folder and file path inside the working directory)
#' 
#' @param Threshold Threshold (double)
#' @param MinSize Minimum size of the segments (integer)
#' 
#' @param verbose Print output messages? (default: TRUE).
#' 
#' @param TerraLib.path Path to the TerraLib binaries (locating \code{terralib_cli_rg} executable). 
#' If not set it uses the PATH env var to find it.
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration. 
#'  
#' @details 
#' By default all bands have the same weight (bw), i.e., bw = 1 / number of bands. Also, by default, 
#' the executable file for running this segmentation algorithm (named \code{terralib_cli_rg}) should 
#' be accessible by the PATH env variable.   
#' 
#' @note  
#' Windows binaries for CLI access to TerraLib v5.1.1 Mean Region Growing segmenter are available at: 
#' \url{https://bitbucket.org/joao_goncalves/terralib5_cli_segmentation_mod/downloads} 
#' (unfortunetaley not yet tested in Linux environment...). Source could be used to build TerraLib for this 
#' effect, check source location \code{/examples/segmentationRegionGrowing} and TerraLib instructions 
#' (\href{http://www.dpi.inpe.br/terralib5/wiki/doku.php?id=wiki:terralib50_build}{link}).
#' 
#' @importFrom raster raster
#' @importFrom raster nbands
#' @export

segmentation_Terralib_MRGrow <- function(x, inputRstPath, outputSegmRst=NULL, Threshold=NULL, MinSize=NULL, verbose=TRUE, TerraLib.path=NULL){
  
  if(missing(x)){
    if(is.null(Threshold) || is.null(MinSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: Threshold and MinSize! Check documentation for more details.")
    }else{
      x <- c(Threshold, MinSize)
    }
  }
  
  if(is.null(outputSegmRst)){
    # Output segmentation raster file
    tmpFile.outRasterSegm <- repBSlash(tempfile("Terralib_MRGrow_Segm_",getwd(),".tif"))
  }else{
    tmpFile.outRasterSegm <- outputSegmRst
  }

  
  # Read inputRstPath metadata to extract the number of bands (required as segmentation input)
  inputRstMeta <- raster::raster(inputRstPath)
  nb <- (raster::nbands(inputRstMeta)) - 1 # Changed this because terralib starts band counting at 0
  
  if(is.null(TerraLib.path))
    TerraLib.path<-""
  else
    TerraLib.path<-paste(TerraLib.path,"/",sep="")
  
  cmd<-paste(
    TerraLib.path, "terralib_cli_rg ", # Command line exec file
    inputRstPath, " ",      # Input raster
    0, " ",    # Band interval to use - first band index nr
    nb, " ",   # Band interval to use - last band index nr
    x[1], " ", # Threshold parameter
    x[2], " ", # Min size of the segments
    tmpFile.outRasterSegm, # Output file
    sep=""
  )
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="Terralib_MRGrow")
  
  ## Run command line
  sysOut<-try(system(cmd,ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE))

  
  if(!inherits(sysOut,"try-error") && sysOut == 0){
    out <- list(segm=tmpFile.outRasterSegm)
    class(out) <- "SOptim.SegmentationResult"
    return(out)
    
  }else{
    return(NA)
  }
  
}

#' Shepherd segmentation algorithm from RSGISLib
#' 
#' A function providing CLI access to RSGISLib Shepherd segmentation and optimize its parameters 
#' using genetic algorithms.
#' 
#' @param x A vector of size three containing the parameters that will be optimized by the genetic algorithms 
#' from package GA: 
#' \itemize{
#'   \item [1] Number of clusters for running the k-means algorithm (integer)
#'   \item [2] Minimum segment size, if smaller it will be merged to neighbouring segments (in pixels; integer)
#'   \item [3] Spectral threshold used to merge objects (float), is a value providing the maximum (Euclidean 
#' distance) spectral separation for which to merge clumps. Set to a large value to ignore spectral 
#' separation and always merge.
#' }
#' If you are using this function outside an optimization context it is better to directly define the 
#' segmentation parameters in \code{NumClust}, \code{MinSize} and \code{SpectralThresh}.
#' 
#' @param inputRstPath Path to the input raster file (typically a multi-layered raster dataset with 
#' segmentation features in each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this case the segmenter will 
#' internally determine a temporary output folder and file path inside the working directory)
#' 
#' @param NumClust Number of clusters for running the k-means algorithm (integer)
#' @param MinSize Minimum segment size (in pixels; integer)
#' @param SpectralThresh Spectral threshold used to merge objects (float)
#' 
#' @param pythonPath Path to the Python interpreter (by default uses the one in the PATH env var). In windows it 
#' should be used for defining the path to the conda distribution of RSGISLib. 
#' 
#' @param verbose Print output messages (default: TRUE).
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration.
#' 
#' @details 
#' By default the k-means algorithm running inside the segmenter will use \code{"INITCLUSTER_DIAGONAL_FULL_ATTACH"} 
#' for the initialization stage, 250 iterations and 0.0025 for the degree of change stopping condition.
#' 
#' @note 
#' Unfortunetelly these functionalities where not yet tested in Linux or Mac environments.
#' 
#' @seealso  
#' Also check out the RSGISLib manual pages at: \url{http://www.rsgislib.org/rsgislib_segmentation.html}.
#' 
#' @references 
#' Clewley, D., Bunting, P., Shepherd, J., Gillingham, S., Flood, N., Dymond, J., Lucas, R., 
#' Armston, J., Moghaddam, M., 2014. A Python-Based Open Source System for Geographic 
#' Object-Based Image Analysis (GEOBIA) Utilizing Raster Attribute Tables. Remote 
#' Sensing 6, 6111.        
#' 
#' @export

segmentation_RSGISLib_Shep <- function(x, inputRstPath, outputSegmRst=NULL, NumClust=NULL, MinSize=NULL, SpectralThresh=NULL, 
                                     pythonPath=NULL, verbose=TRUE){
  
  if(missing(x)){
    if(is.null(NumClust) || is.null(MinSize) || is.null(SpectralThresh)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: NumClust, MinSize and SpectralThresh! Check documentation for more details.")
    }else{
      x <- c(NumClust, MinSize, SpectralThresh)
    }
  }
  
  if(is.null(pythonPath) || is.na(pythonPath)){
    stop("pythonPath is not defined! Unable to find RSGISLib...")
  }else{
    prefix=paste(pythonPath,"/",sep="")
  }
  
  # Find python script with CLI access to RSGISLib segmentation
  PyScriptPath<-getPythonFile("RSGISLib_Shep.py",
                              "SegOptim",
                              c("/media/kamo/HDD1/MyDocs/R-dev", # only for testing!!
                                "D:/MyDocs/R-dev/SegOptim/inst/PyScripts",
                                "E:/JG_Dropbox/Dropbox/SharedWorkFolder/SegOptim/inst/PyScripts",
                                "/media/sf_MyDocs/R-dev/SegOptim/inst/PyScripts",
                                getwd()))
  
  # If the the script can not be found stop execution
  if(is.null(PyScriptPath)){
    stop("Unable to find the path to the RSGISLib python script (RSGISLib_Shep.py) in dev or pkg paths!")
  }
  
  # Output segmentation raster file

  
  if(is.null(outputSegmRst)){
    tmpDir <- getwd()
    tmpFileSegmOutRst <- repBSlash(paste(tmpDir,"/RSGISLib_Shep_Segm_",randString(8),".tif",sep=""))
    tmpBatchFile <- repBSlash(paste(tmpDir,"/RSGISLib_Shep_Segm_",randString(8),".bat",sep=""))
  }else{
    tmpDir <- getwd()
    tmpFileSegmOutRst <- outputSegmRst
    tmpBatchFile <- repBSlash(paste(tmpDir,"/RSGISLib_Shep_Segm_",randString(8),".bat",sep=""))
  }
  
  # Assemble command string to run
  cmd<-paste(prefix,
             "python \"",PyScriptPath,"\" ",
             "\"",inputRstPath,"\" ",
             tmpDir," ",
             x[1]," ", # this will be converted to integer by python
             x[2]," ", # this one too
             x[3]," ",
             tmpFileSegmOutRst, sep="")
    
  if(.Platform$OS.type=="windows"){
    
    # If OS is windows-like then set the envir paths to the python path defined
    # Typically this would be Anaconda or Miniconda system
    cmd_paths<-paste("@echo off\n",
                     "set PYTHONPATH=",pythonPath,"/Lib\n",
                     "set PYTHONHOME=",pythonPath,"\n",
                     "set PATH=",pythonPath,";%PATH%", sep="")
  }else{
    cmd_paths<-"#!/bin/sh"
  }

  cat(cmd_paths,"\n",cmd, sep="", file = tmpBatchFile)
    
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="RSGISLib_Shep")
  
  ## Run command line (don't print RSGISLib output to console)
  ## Changed this to run via system using cmd (win) or sh (unix)
  
  if(.Platform$OS.type == "windows"){
    #sysOut<-try(shell(tmpBatchFile, ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE))
    sysOut<-try(shell(tmpBatchFile, 
                       ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE))
  }else{
    sysOut<-try(system(paste("sh",tmpBatchFile), 
                       ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE))
  }
  
  if(!inherits(sysOut,"try-error") && sysOut == 0){
    out <-list( # The remaining temp files generated by RSGISLib were already removed by python
                segm=tmpFileSegmOutRst,
                batchFile=tmpBatchFile)
    class(out) <- "SOptim.SegmentationResult"
    return(out)
  }else{
    return(NA)
  }
}

#' Large Scale Mean Shift segmentation algorithm from Orfeo Toolbox (OTB)
#' 
#' A function providing CLI access to OTB's LSMS segmentation and optimize its parameters 
#' using genetic algorithms. This function uses the same values for the spectral and spatial 
#' range parameters in the mean-shift smoothing and large-scale segmentation step. If you wish 
#' to use different values in each step use function \code{segmentation_OTB_LSMS2}.
#' 
#' @param x A vector of size three containing the parameters that will be optimized by the 
#' genetic algorithms from package GA:    
#' \itemize{
#'     \item [1] Range radius defining the radius (expressed in radiometry units) in the 
#'     multi-spectral space (float);
#'     \item [2] Spatial radius of the neighborhood (float);
#'     \item [3] Minimum segment/region size. If, after the segmentation, a region is of 
#'     size lower than this criterion, 
#'     the region is merged with the "nearest" region (radiometrically) (integer).
#' }
#' If you are using this function outside an optimization context it is better to directly 
#' define the segmentation parameters in \code{SpectralRange}, \code{SpatialRange} and 
#' \code{MinSize}.
#' 
#' @param inputRstPath The input raster dataset used to perform the image segmentation using 
#' OTB LSMS algorithm (typically a multi-layered raster dataset with segmentation features 
#' in each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this 
#' case the segmenter will internally determine a temporary output folder and file path 
#' inside the working directory).
#' 
#' @param SpectralRange Range radius defining the radius (expressed in radiometry units) in 
#' the multi-spectral space (float).
#' 
#' @param SpatialRange Spatial radius of the neighborhood (float).
#' 
#' @param MinSize Minimum segment/region size.
#' 
#' @param lsms_maxiter Algorithm iterative scheme will stop if convergence hasn't been 
#' reached after the maximum number of iterations (integer).
#' 
#' @param tilesizex Size of tiles along the X-axis (integer).
#' 
#' @param tilesizey Size of tiles along the Y-axis (integer).
#' 
#' @param otbBinPath The system path to OTB binaries (e.g., where otbcli_MeanShiftSmoothing 
#' is located). By default this is equal to \code{NULL} which means that the system path is 
#' used.
#' 
#' @param RAM Available memory for processing the image data (in MB).
#' 
#' @param verbose Print output messages (default: TRUE).
#' 
#' @note 
#' Changed system() option to \code{show.output.on.console = TRUE} to avoid for errors 
#' happening on windows-10...
#' 
#' Check [OTB documentation](https://www.orfeo-toolbox.org/CookBook/Applications/app_LargeScaleMeanShift.html) 
#' for more info.
#'  
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration or if an error occurs. Also, notice 
#' that some auxiliary files will be created: otb_filt_range_, otb_filt_spatial_, 
#' otb_segm_init_.
#' 
#' @references 
#' J. Michel, D. Youssefi and M. Grizonnet, 2015. Stable Mean-Shift Algorithm and Its 
#' Application to the Segmentation of Arbitrarily Large Remote Sensing Images. IEEE 
#' Transactions on Geoscience and Remote Sensing, 53: 2, 952-964.
#' 
#' @export
#' 

segmentation_OTB_LSMS <- function(x, inputRstPath, outputSegmRst=NULL, 
                                  SpectralRange=NULL, SpatialRange=NULL, MinSize=NULL, 
                                  lsms_maxiter=10, tilesizex = 2500, tilesizey = 2500, 
                                  otbBinPath=NULL, RAM=3072, verbose=TRUE){
  
  if(missing(x)){
    if(is.null(SpectralRange) || is.null(SpatialRange) || is.null(MinSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: SpectralRange, SpatialRange and MinSize! Check documentation for more details.")
    }else{
      # Set x from detailed parameter list
      x <- c(SpectralRange, SpatialRange, MinSize)
    }
  }
  
  if(is.null(outputSegmRst)){
    # Output segmentation raster file
    tmpDir<-getwd()
    
    # Make temp files path
    tmp_FilteredRange<-repBSlash(paste(tmpDir,"/otb_filt_range_",randString(6),".tif",sep=""))
    tmp_FilteredSpatial<-repBSlash(paste(tmpDir,"/otb_filt_spatial_",randString(6),".tif",sep=""))
    tmp_Segmentation<-repBSlash(paste(tmpDir,"/otb_segm_init_",randString(6),".tif",sep=""))
    tmp_SegmentationMerged<-repBSlash(paste(tmpDir,"/otb_segm_merge_",randString(6),".tif",sep=""))
    
  }else{
    # Output segmentation raster file
    tmpDir<-dirname(outputSegmRst)
    
    # Make temp files path
    tmp_FilteredRange <- repBSlash(paste(tmpDir,"/otb_filt_range_",randString(6),".tif",sep=""))
    tmp_FilteredSpatial <- repBSlash(paste(tmpDir,"/otb_filt_spatial_",randString(6),".tif",sep=""))
    tmp_Segmentation <- repBSlash(paste(tmpDir,"/otb_segm_init_",randString(6),".tif",sep=""))
    tmp_SegmentationMerged <- outputSegmRst
    
  }
  
  # Use OTB binary path?
  if(is.null(otbBinPath)){
    prefix=""
  }else{
    prefix=paste(otbBinPath,"/",sep="")
  }
  
  cmd_1<-paste(prefix,"otbcli_MeanShiftSmoothing",
               " -in ",inputRstPath,
               " -fout ",tmp_FilteredRange,
               " -foutpos ",tmp_FilteredSpatial,
               " -ranger ",x[1],
               " -spatialr ",x[2],
               " -maxiter ",lsms_maxiter,
               " -ram ",RAM,
               " -modesearch 0", sep="")
  
  cmd_2<-paste(prefix,"otbcli_LSMSSegmentation",
              " -in ",tmp_FilteredRange,
              " -inpos ",tmp_FilteredSpatial,
              " -out  ",tmp_Segmentation," uint32",
              " -ranger ",x[1],
              " -spatialr ",x[2],
              " -minsize 0",
              " -tilesizex ",tilesizex,
              " -tilesizey ",tilesizey, sep="")
  
  cmd_3<-paste(prefix,"otbcli_LSMSSmallRegionsMerging",
              " -in ",tmp_FilteredRange,
              " -inseg ",tmp_Segmentation,
              " -out ",tmp_SegmentationMerged," uint32",
              " -minsize ",x[3],
              " -tilesizex ",tilesizex,
              " -tilesizey ",tilesizey, 
              " -ram ",RAM, sep="")
  
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="OTB_LSMS")
  
  ## Run command lines #1 - Mean shift ... #2 LSMS Segmentation ... #3 Region merging
  if(.Platform$OS.type=="windows"){
    ## Changed system() option to \code{show.output.on.console = TRUE} to avoid 
    ## errors happening on windows-10...
    sysOut_1 <- try(system(cmd_1, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                         show.output.on.console = TRUE))
    sysOut_2 <- try(system(cmd_2, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                         show.output.on.console = TRUE))
    sysOut_3 <- try(system(cmd_3, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                         show.output.on.console = TRUE))
  }else{
    sysOut_1 <- try(system(cmd_1, ignore.stdout = TRUE, ignore.stderr = TRUE))
    sysOut_2 <- try(system(cmd_2, ignore.stdout = TRUE, ignore.stderr = TRUE))
    sysOut_3 <- try(system(cmd_3, ignore.stdout = TRUE, ignore.stderr = TRUE))
  } 

  if(!inherits(sysOut_1,"try-error") && 
     !inherits(sysOut_2,"try-error") && 
     !inherits(sysOut_2,"try-error")){
    out <- list(
        FilteredRange = tmp_FilteredRange,
        FilteredSpatial = tmp_FilteredSpatial,
        Segmentation = tmp_Segmentation,
        segm = tmp_SegmentationMerged)
    
    class(out) <- "SOptim.SegmentationResult"
    return(out)
  }
  else{
    on.exit(doCleanUpActions(c(
      FilteredRange = tmp_FilteredRange,
      FilteredSpatial = tmp_FilteredSpatial,
      Segmentation = tmp_Segmentation,
      segm = tmp_SegmentationMerged
    )))
    warning("Unable to perform segmentaton with method OTB_LSMS!")
    return(NA)
  }
}



#' Large Scale Mean Shift segmentation algorithm from Orfeo Toolbox (OTB) with two sets 
#' of parameters
#' 
#' A function providing CLI access to OTB's LSMS segmentation and optimize its parameters 
#' using genetic algorithms. This function differs from \code{segmentation_OTB_LSMS} by 
#' implementing two sets of parameters for the spectral and spatial range, the first for 
#' running mean-shift smoothing (MS_ prefix) and, the second, for the large-scale 
#' segmentation step (LSS_ prefix).
#' 
#' @param x A vector of size five containing the parameters that will be optimized by the 
#' genetic algorithms from package GA:    
#' \itemize{
#'     \item [1] Range radius defining the radius (expressed in radiometry units) in the 
#'     multi-spectral space for the mean-shift smoothing step (float);
#'     \item [2] Spatial radius of the neighborhood for mean-shift smoothing (float);
#'     \item [3] Range radius defining the radius (expressed in radiometry units) in the 
#'     multi-spectral space for the large-scale segmentation step (float);
#'     \item [4] Spatial radius of the neighborhood for large-scale segmentation (float);
#'     \item [5] Minimum segment/region size. If, after the segmentation, a region is of 
#'     size lower than this criterion, the region is merged with the "nearest" region 
#'     (radiometrically) (integer).
#' }
#' If you are using this function outside an optimization context it is better to directly 
#' define the segmentation parameters in \code{MS_SpectralRange}, \code{MS_SpatialRange} 
#' \code{LSS_SpectralRange}, \code{LSS_SpatialRange} and \code{MinSize}.
#' 
#' @param inputRstPath The input raster dataset used to perform the image segmentation using 
#' OTB LSMS algorithm (typically a multi-layer raster dataset with segmentation features in 
#' each band).
#' 
#' @param outputSegmRst A path to the output segmented image (default: \code{NULL}; in this 
#' case the segmenter will internally determine a temporary output folder and file path 
#' inside the working directory).
#' 
#' @param MS_SpectralRange Threshold on spectral signature euclidean distance (expressed in 
#' radiometry unit) to consider neighborhood pixel for averaging. Higher values will be less 
#' edge-preserving (more similar to simple average in neighborhood), whereas lower values 
#' will result in less noise smoothing. Note that this parameter has no effect on processing 
#' time (float).
#' 
#' @param MS_SpatialRange Radius of the spatial neighborhood for averaging. Higher values 
#' will result in more smoothing and higher processing time (float).
#' 
#' @param LSS_SpectralRange Threshold on spectral signature euclidean distance (expressed 
#' in radiometry unit) to consider pixels in the same segment. A good value is half the 
#' range radius used in the MeanShiftSmoothing application (ranger parameter; float).
#' 
#' @param LSS_SpatialRange Threshold on Spatial distance to consider pixels in the same 
#' segment. A good value is half the spatial radius used in the MeanShiftSmoothing 
#' application (spatialr parameter; float).
#' 
#' @param MinSize Minimum segment/region size.
#' 
#' @param lsms_maxiter Algorithm iterative scheme will stop if convergence hasn't been 
#' reached after the maximum number of iterations (integer).
#' 
#' @param tilesizex Size of tiles along the X-axis (integer).
#' 
#' @param tilesizey Size of tiles along the Y-axis (integer).
#' 
#' @param otbBinPath The system path to OTB binaries (e.g., where otbcli_MeanShiftSmoothing 
#' is located). By default this is equal to \code{NULL} which means that the system path is 
#' used.
#' 
#' @param RAM Available memory for processing the image data (in MB).
#' 
#' @param verbose Print output messages (default: TRUE).
#' 
#' @note 
#' Check [OTB documentation](https://www.orfeo-toolbox.org/CookBook/Applications/app_LargeScaleMeanShift.html) 
#' for more info.
#' 
#' @return A list object containing output file paths resulting from the segmentation run. 
#' These files will be cleaned after each GA iteration or if an error occurs. Also, notice 
#' that some auxiliary files will be created: otb_filt_range_, otb_filt_spatial_, 
#' otb_segm_init_.
#' 
#' @references 
#' J. Michel, D. Youssefi and M. Grizonnet, 2015. Stable Mean-Shift Algorithm and Its 
#' Application to the Segmentation of Arbitrarily Large Remote Sensing Images. IEEE 
#' Transactions on Geoscience and Remote Sensing, 53: 2, 952-964.
#' 
#' @export
#' 

segmentation_OTB_LSMS2 <- function(x, inputRstPath, outputSegmRst = NULL, 
                                  MS_SpectralRange = NULL, MS_SpatialRange = NULL, 
                                  LSS_SpectralRange = NULL, LSS_SpatialRange = NULL, 
                                  MinSize = NULL, lsms_maxiter = 10, tilesizex = 2500, 
                                  tilesizey = 2500, otbBinPath = NULL, RAM = 3072, 
                                  verbose = TRUE){
  
  if(missing(x)){
    if(is.null(MS_SpectralRange) || is.null(MS_SpatialRange) || 
       is.null(LSS_SpectralRange) || is.null(LSS_SpatialRange) || is.null(MinSize)){
      stop("If segmentation parameters are not defined in x then these must be passed on all the following 
           parameters: MS_SpectralRange, MS_SpatialRange, LSS_SpectralRange, LSS_SpatialRange, and MinSize! 
           Check documentation for more details.")
    }else{
      # Set x from detailed parameter list
      x <- c(MS_SpectralRange, MS_SpatialRange, 
             LSS_SpectralRange, LSS_SpatialRange, MinSize)
    }
  }
  
  if(is.null(outputSegmRst)){
    # Output segmentation raster file
    tmpDir <- getwd()
    
    # Make temp files path
    tmp_FilteredRange <- repBSlash(paste(tmpDir,"/otb_filt_range_",randString(6),".tif",sep=""))
    tmp_FilteredSpatial <- repBSlash(paste(tmpDir,"/otb_filt_spatial_",randString(6),".tif",sep=""))
    tmp_Segmentation <- repBSlash(paste(tmpDir,"/otb_segm_init_",randString(6),".tif",sep=""))
    tmp_SegmentationMerged <- repBSlash(paste(tmpDir,"/otb_segm_merge_",randString(6),".tif",sep=""))
    
  }else{
    # Output segmentation raster file
    tmpDir <- dirname(outputSegmRst)
    
    # Make temp files path
    tmp_FilteredRange <- repBSlash(paste(tmpDir,"/otb_filt_range_",randString(6),".tif",sep=""))
    tmp_FilteredSpatial <- repBSlash(paste(tmpDir,"/otb_filt_spatial_",randString(6),".tif",sep=""))
    tmp_Segmentation <- repBSlash(paste(tmpDir,"/otb_segm_init_",randString(6),".tif",sep=""))
    tmp_SegmentationMerged <- outputSegmRst
    
  }
  
  # Use OTB binary path?
  if(is.null(otbBinPath)){
    prefix=""
  }else{
    prefix=paste(otbBinPath,"/",sep="")
  }
  
  cmd_1 <- paste(prefix,"otbcli_MeanShiftSmoothing",
               " -in ",inputRstPath,
               " -fout ",tmp_FilteredRange,
               " -foutpos ",tmp_FilteredSpatial,
               " -ranger ",x[1],
               " -spatialr ",x[2],
               " -maxiter ",lsms_maxiter,
               " -ram ",RAM,
               " -modesearch 0", sep="")
  
  cmd_2 <- paste(prefix,"otbcli_LSMSSegmentation",
               " -in ",tmp_FilteredRange,
               " -inpos ",tmp_FilteredSpatial,
               " -out  ",tmp_Segmentation," uint32",
               " -ranger ",x[3],
               " -spatialr ",x[4],
               " -minsize 0",
               " -tilesizex ",tilesizex,
               " -tilesizey ",tilesizey, sep="")
  
  cmd_3 <- paste(prefix,"otbcli_LSMSSmallRegionsMerging",
               " -in ",tmp_FilteredRange,
               " -inseg ",tmp_Segmentation,
               " -out ",tmp_SegmentationMerged," uint32",
               " -minsize ",x[5],
               " -tilesizex ",tilesizex,
               " -tilesizey ",tilesizey, 
               " -ram ",RAM, sep="")
  
  
  if(verbose) checkPrintSegmentationParams(x,segmentMethod="OTB_LSMS2")
  
  ## Run command lines #1 - Mean shift ... #2 LSMS Segmentation ... #3 Region merging
  if(.Platform$OS.type=="windows"){
    ## Changed system() option to \code{show.output.on.console = TRUE} to avoid 
    ## errors happening on windows-10...
    sysOut_1 <- try(system(cmd_1, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                           show.output.on.console = TRUE))
    sysOut_2 <- try(system(cmd_2, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                           show.output.on.console = TRUE))
    sysOut_3 <- try(system(cmd_3, ignore.stdout = TRUE, ignore.stderr = TRUE, 
                           show.output.on.console = TRUE))
  }else{
    sysOut_1 <- try(system(cmd_1, ignore.stdout = TRUE, ignore.stderr = TRUE))
    sysOut_2 <- try(system(cmd_2, ignore.stdout = TRUE, ignore.stderr = TRUE))
    sysOut_3 <- try(system(cmd_3, ignore.stdout = TRUE, ignore.stderr = TRUE))
  } 
  
  if(!inherits(sysOut_1,"try-error") && 
     !inherits(sysOut_2,"try-error") && 
     !inherits(sysOut_2,"try-error")){
    out <- list(
      FilteredRange = tmp_FilteredRange,
      FilteredSpatial = tmp_FilteredSpatial,
      Segmentation = tmp_Segmentation,
      segm = tmp_SegmentationMerged)
    
    class(out) <- "SOptim.SegmentationResult"
    return(out)
  }
  else{
    on.exit(doCleanUpActions(c(
      FilteredRange = tmp_FilteredRange,
      FilteredSpatial = tmp_FilteredSpatial,
      Segmentation = tmp_Segmentation,
      segm = tmp_SegmentationMerged
    )))
    warning("Unable to perform segmentaton with method OTB_LSMS (two-parameter set)!")
    return(NA)
  }
}



#' A generic dispatcher/wrapper function for running multiple segmentation methods
#' 
#' Runs the appropriate segmentation method with the parameters defined in \code{...}.
#' 
#' @param segmentMethod Character string used to define the segmentation method. 
#' Available options are:     
#' \itemize{
#'   \item \code{"SAGA_SRG"} - SAGA Simple Region Growing;
#'   \item \code{"GRASS_RG"} - GRASS Region Growing;
#'   \item \code{"ArcGIS_MShift"} - ArcGIS Mean Shift algorithm;
#'   \item \code{"Terralib_Baatz"} - TerraLib Baatz algorithm;
#'   \item \code{"Terralib_MRGrow"} - TerraLib Mean Region Growing;
#'   \item \code{"RSGISLib_Shep"} - RSGISLib Shepherd algorithm;
#'   \item \code{"OTB_LSMS"} - OTB Large Scale Mean Shift algorithm;
#'   \item \code{"OTB_LSMS2"} - OTB Large Scale Mean Shift algorithm 
#'   with two separate sets of parameters, one for mean-shift smoothing 
#'   and another for large-scale segmentation step;
#' }
#' 
#' @param ... Specific parameters to pass to each segmentation method.
#' 
#' @seealso 
#' Check out more information about each specific segmentation method at: 
#' \itemize{
#'     \item \code{\link{segmentation_SAGA_SRG}} 
#'     \item \code{\link{segmentation_GRASS_RG}} 
#'     \item \code{\link{segmentation_ArcGIS_MShift}} 
#'     \item \code{\link{segmentation_Terralib_Baatz}} 
#'     \item \code{\link{segmentation_Terralib_MRGrow}}
#'     \item \code{\link{segmentation_RSGISLib_Shep}}
#'     \item \code{\link{segmentation_OTB_LSMS}}
#'     \item \code{\link{segmentation_OTB_LSMS2}}
#' }
#' 
#' @export
#' 

segmentationGeneric <- function(segmentMethod, ...){
  
  if(segmentMethod == "SAGA_SRG"){
    segmentationOutput <- segmentation_SAGA_SRG(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "GRASS_RG"){
    segmentationOutput <- segmentation_GRASS_RG(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "ArcGIS_MShift"){
    segmentationOutput <- segmentation_ArcGIS_MShift(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "Terralib_Baatz"){
    segmentationOutput <- segmentation_Terralib_Baatz(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "Terralib_MRGrow"){
    segmentationOutput <- segmentation_Terralib_MRGrow(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "RSGISLib_Shep"){
    segmentationOutput <- segmentation_RSGISLib_Shep(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "OTB_LSMS"){
    segmentationOutput <- segmentation_OTB_LSMS(...)
    return(segmentationOutput)
  }
  
  else if(segmentMethod == "OTB_LSMS2"){
    segmentationOutput <- segmentation_OTB_LSMS2(...)
    return(segmentationOutput)
  }
  
  else{
    stop("Unknown segmentation method defined! Please review input parameters")
  }
}
