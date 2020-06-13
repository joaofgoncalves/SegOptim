
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
#' An useful function to remove NA values for vector, matrices or data frames. By default 
#' removes all NA values in each column of a matrix or data frame.
#' 
#' @param x Either a vector, matrix or data frame.
#' 
#' @return Returns the original object without \code{NA} values. An attribute named 
#' \code{"na.action"} is returned as a list containing the original indices that were 
#' removed.
#' 
#' @details 
#' This function was adapted from IDPmisc package 
#' (URL: \url{https://github.com/cran/IDPmisc/blob/master/R/NaRV.omit.R}).
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
    ri <- (!apply(x.num, MARGIN = 1, function(x) sum(is.infinite(x))>0) & 
             !apply(x, MARGIN = 1, function(x) sum(is.na(x))>0))
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
#' Finds the path to python scripts used to perform image segmentation in ArcGIS or 
#' RSGISLib based on the package location or other user-defined places.
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
#' An useful function to verify the name of segmentation parameters to be optimized and 
#' to print them during GA optimization.
#' 
#' @param x A character vector containing parameter names.
#' @param segmentMethod The name of the segmentation method. Check available options on 
#' \code{\link{segmentationGeneric}}.
#' @param rnd An integer defining the number of decimal plates.
#' @param print Boolean defining whether or not to print the parameters.
#' 
#' @return Prints a message with the segmentation parameters (returns NULL).
#' 
#' @examples 
#' checkPrintSegmentationParams(x = c(0.1, 5), segmentMethod = "GRASS_RG", 
#'                              rnd = 5, print = TRUE)
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


#' Perform clean-up actions
#' 
#' Remove temporary files or directories. Useful for cleaning files that result from image 
#' segmentation procedures during optimization.
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

