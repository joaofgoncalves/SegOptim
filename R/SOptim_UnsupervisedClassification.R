

#' Determine and (randomly) adjust the number of samples per stratum 
#' 
#' Allows to calculate the number of samples per stratum with allocation 
#' proportional to size
#' 
#' @param propStrata A \code{table} object with the proportion of each stratum 
#' in terms of area of relative frequency.
#' 
#' @param n Total number of samples.
#' 
#' @param minSamp Should a minimum sample size per stratum be enforced? (default: TRUE)
#' 
#' @param minSizeSet Minimum sample size per stratum (default: 10).
#' 
#' @return 
#' A \code{table} object with the number of samples by stratum.
#' 
#' @seealso \code{\link[base]{table}}
#' 
#' @examples 
#' 
#' st <- sample(1:5,1000,replace=TRUE)
#' tb <- table(st)/length(st)
#' 
#' numSampPerStrata(propStrata=tb, n=100, minSamp = TRUE, minSizeSet = 10)
#' 
#' @export
#' 

numSampPerStrata<-function(propStrata, n, minSamp = TRUE, minSizeSet = 10){
  
  if(!inherits(propStrata,"table"))
    stop("propStrata must be an object of class table")
  
  if(sum(propStrata) > 1)
    stop("sum(propStrata) must be equal to 1")
  
  nSamp<-round(propStrata*n)
  diffN<-n-sum(nSamp)
  
  if(diffN==0){
    
    if(minSamp){
      nSamp[nSamp<minSizeSet]<-minSizeSet
    }
    
    return(nSamp)
  }else if(diffN>0){
    ##
    #  The number of samples is less than the target	
    for(i in 1:diffN){
      selS<-sample(1:length(propStrata),1)
      nSamp[selS]<-nSamp[selS]+1
    }
    
    if(minSamp){
      nSamp[nSamp<minSizeSet]<-minSizeSet
    }
    
    return(nSamp)
  }else{
    ##
    #  The number of samples is greater than the target
    repeat{
      selS<-sample(1:length(propStrata),1)
      if(nSamp[selS]!=0){
        nSamp[selS]<-nSamp[selS]-1
      }else{
        next
      }
      
      if((n-sum(nSamp))==0){
        break
      }
    }
    
    if(minSamp){
      nSamp[nSamp<minSizeSet]<-minSizeSet
    }
    
    return(nSamp)
  }
}		


#' Performs Stratified Random Sampling
#' 
#' An auxiliar function to perform stratified random sampling of a set of integers.
#' 
#' @param x Vector of integer indices
#' 
#' @param strata	A vector containing the stratum number/code for each element of \code{x} 
#' (unique elements in strata must equal the names property of \code{nsps}).
#' 
#' @param nsps An object of class \code{table} indicating the number of samples per stratum (check 
#' \link{numSampPerStrata}).
#' 
#' @return 
#' A vector of indices with the selected elements of \code{x}.
#' 
#' @seealso \code{\link[base]{sample}}
#' 
#' @examples 
#' 
#' st <- sample(1:5,1000,replace=TRUE)
#' tb <- table(st)/length(st)
#' ind <- 1:1000
#' 
#' nsps <- numSampPerStrata(propStrata=tb, n=100, minSamp = TRUE, minSizeSet = 10)
#' 
#' StRS(x = ind, strata = st, nsps = nsps)
#' 
#' @export
#' 


StRS <- function(x, strata, nsps){
  
  if(!inherits(nsps,"table"))
    stop("nsps must be an object of class table!")

  if(length(strata)!=length(x))
    stop("strata and x must have the same vector length!")
  
  samples <- vector()
  strataNames <- as.integer(names(nsps))
  
  for(sn in strataNames){
    n <- nsps[as.character(sn)]
    
    if(n!=0){
      samples <- c(samples, sample(x[strata==sn], n, replace = FALSE))	
    }else{
      next
    }
  }
  return(samples)
}



#' Perform clustering on a RasterStack object and calculate internal clustering criteria
#' 
#' Runs a clustering/unsupervised learning/classification algorithm on a multi-layer \code{RasterStack} object.      
#' It also allows calculating internal clustering performance criteria based on function 
#' \code{\link[clusterCrit]{intCriteria}} using a stratified random sample of output pixels. 
#' 
#' @param inRst An input \code{RasterStack} object. By default all layers/bands will be used to perform the k-means 
#' clustering.
#' 
#' @param k An integer vector defining the number of clusters. If more than one value is given then 
#' \code{kmeansRaster} will run one time for each \code{k} value.
#' 
#' @param writeRasterData Write clustered raster data to file (i.e., to outRst)? (default:TRUE)
#' 
#' @param outRst Name of the output raster. For each value in \code{k} the file name will get a suffix 
#' in this form: \code{"filename_method_nc_k.ext"}.
#' 
#' @param getRstStack Get an output \code{RasterStack} object with clustering solutions? (default: \code{FALSE}). 
#' If \code{FALSE} then some memory saving is expected.
#' 
#' @param method A string defining the method used for clustering. Available options are: 
#' \itemize{
#'    \item \strong{\code{"kmeans"}}: for k-means partitioning clustering from stats package (default); 
#'    \item \strong{\code{"hardcl"}}: for hard-competitive learning; 
#'    \item \strong{\code{"neuralgas"}}: for neural-gas algorithm both from cclust package, and; 
#'    \item \strong{\code{"clara"}}: from cluster package.
#' }
#' 
#' @param verbose Print progress messages? (default: TRUE).
#' 
#' @param calcIntCriteria Calculate internal clustering criteria? (default: FALSE).
#' 
#' @param crit A character vector containing the names of the indices to compute. Check available indices 
#' at \code{\link[clusterCrit]{intCriteria}} or running \code{getCriteriaNames(TRUE)}.
#' 
#' @param intCritSampSize Number of sample pixels to use for internal criteria calculation (default: 20000). 
#' Increasing this number will make estimations better however at the expense of more memory.
#' 
#' @param ... Further parameters passed to each of clustering functions \code{\link[stats]{kmeans}}, 
#' \code{\link[cclust]{cclust}} or, \code{\link[cluster]{clara}} (for clara, except the following 
#' paramaters which are pre-defined to: \code{keep.data = FALSE}, and \code{medoids.x = FALSE} to 
#' save memory and speed-up calculations).
#'  
#' @return 
#' If \code{getRstStack=TRUE} then a \code{RasterStack} object with all clustering solutions will be returned. 
#' If \code{getRstStack=TRUE} and \code{calcIntCriteria=TRUE} then an additional matrix named \code{intClustCrit} with 
#' one row by k value will be returned with the requested clustering indices. If \code{getRstStack=FALSE} then only the matrix 
#' with clustering indices will be returned.
#' 
#' @seealso 
#' See \code{\link[clusterCrit]{intCriteria}}, \code{\link[clusterCrit]{getCriteriaNames}} for more details on 
#' clustering criteria.      
#' Check out clustering functions at: \code{\link[stats]{kmeans}}, \code{\link[cclust]{cclust}}, and, 
#' \code{\link[cluster]{clara}}.
#' 
#' 
#' @importFrom clusterCrit intCriteria
#' @importFrom raster stack
#' @importFrom raster values
#' @importFrom raster writeRaster
#' @importFrom raster raster
#' @importFrom raster dataType
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @importFrom stats kmeans
#' @importFrom cclust cclust
#' @importFrom cluster clara
#' @importFrom stats complete.cases
#' 
#' @examples 
#' 
#' library(raster)
#' 
#' r1 <- raster(nrows=10, ncols=10, vals=rnorm(100))
#' r2 <- raster(nrows=10, ncols=10, vals=rnorm(100))
#' r3 <- raster(nrows=10, ncols=10, vals=rnorm(100))
#' 
#' rstStack <- stack(r1, r2, r3)
#' 
#' clusteringRaster(rstStack, k = 2:10, writeRasterData = FALSE, getRstStack = TRUE, 
#'                  method="kmeans", calcIntCriteria = FALSE, crit = c("Silhouette"), 
#'                  intCritSampSize = 20000, verbose = TRUE)
#'
#' 
#' @export
#' 

clusteringRaster <- function(inRst, k, writeRasterData = TRUE, outRst, getRstStack = TRUE, 
                             method="kmeans", calcIntCriteria = FALSE,
                             crit = c("Silhouette"), intCritSampSize = 20000, 
                             verbose = TRUE, ...){ 
  
  if(missing(inRst))
    stop("inRst is not defined!")
  if(missing(k))
    stop("k is not defined!")
  if(writeRasterData){
    if(missing(outRst)){
      stop("outRst is not defined!")
    }
  }
    
  if(is.character(inRst)){
    if(file.exists(inRst)){
      inRst <- raster::stack(inRst)
    }else{
      stop("File path in inRst does not exists! Please review parameters.")
    }
  }
  
  if(min(k) < 2){
    stop("k values can not be lower than 2!")
  }

  if(!inherits(inRst,c("RasterStack","RasterBrick"))){
    stop("inRst must be an object of class RasterStack or RasterBrick!")
  }
  
  if(verbose) cat("-> Loading raster data .... ")
  rstMatrix <- as.matrix(values(inRst))
  mode(rstMatrix) <- "numeric"
  
  if(verbose) cat("done.\n\n")
  
  if(verbose) cat("-> Removing NA values .... ")
  idx <- complete.cases(rstMatrix)
  rstMatrix <- rstMatrix[idx,]
  
  if(verbose) cat("done.\n\n")
  
  if(calcIntCriteria){
    #intCritKM <- list()
    intCritRes <- matrix(NA, nrow = length(k), ncol = length(crit), dimnames = list(paste("Nclust",k,sep="_"), crit))
  } 
  
  if(verbose) cat("-> Performing ",method," clustering:\n") ## ------------------------------------------------------------------------------ ##
  
  i <- 0 
  for(nc in k){
    
    i<-i+1
    if(verbose) cat("k=",nc,"... ",sep="")
    
    ## ---- Run clustering algorithms ----
    
    # ---- K-means clustering
    if(method == "kmeans"){
      
      clustSol <- stats::kmeans(rstMatrix, centers = nc, ...)
      clustSolVecName <- "cluster"
      
    # ---- Hard competitive learning algorithm 
    }else if(method == "hardcl"){
      
      clustSol <- cclust::cclust(rstMatrix, centers = nc, method="hardcl", ...)
      clustSolVecName <- "cluster"
      
    # ---- Neural gas algorithm
    }else if(method == "neuralgas"){
      
      clustSol <- cclust::cclust(rstMatrix, centers = nc, method="neuralgas", ...)
      clustSolVecName <- "cluster"
    
    # --- CLARA algorithm
    }else if (method == "clara"){
      
      clustSol <- cluster::clara(rstMatrix, k = nc, keep.data = FALSE, medoids.x = FALSE, ...)
      clustSolVecName <- "clustering"
      
    }else{
      stop("Clustering method not found! Please review input parameters")
    }
    
    
    ## ---- Calculate internal performance criteria metrics ---- 
    
    if(calcIntCriteria){
      
      if(verbose) cat("calculating clustering performance criteria ....")
      
      # Calculate frequency data for stratified sampling
      clust_tb <- table(clustSol[[clustSolVecName]])
      clust_tb <- clust_tb / sum(clust_tb)
      nsps <- numSampPerStrata(clust_tb, n = intCritSampSize, minSamp = 10, minSizeSet = 20)
      strs_idx <- StRS(x=1:nrow(rstMatrix), strata = clustSol[[clustSolVecName]], nsps = nsps)
      
      # Calculate the internal criteria cluster validity index
      intcrit <- try(clusterCrit::intCriteria(traj = rstMatrix[strs_idx, ], 
                                              part = as.integer(clustSol[[clustSolVecName]][strs_idx]), 
                                              crit = crit))
      
      if(!inherits(intcrit,"try-error")){
        intCritRes[i, ] <- unlist(intcrit)
      }
      
      if(verbose) cat("done.\n")
      
    }
    
    # Create new vector to allocate cluster number and NA values
    clustVec <- vector(mode = "integer", length = ncell(inRst))
    clustVec[-idx] <- NA
    clustVec[clustVec==0] <- NA ## Avoid strange errors with 0's on vector labelling
    clustVec[idx] <- as.integer(clustSol[[clustSolVecName]])
    
    # Put values into new raster
    newRst <- raster::raster(inRst)
    raster::values(newRst) <- clustVec
    raster::dataType(newRst) <- "INT1U"
    
    if(getRstStack){
      if(i==1){
        rstStack <- newRst
      }else{
        rstStack <- stack(rstStack, newRst)
      }
    }
    
    if(writeRasterData){
      ext <- tools::file_ext(outRst)
      fn <- tools::file_path_sans_ext(outRst)
      raster::writeRaster(newRst, filename = paste(fn,"_",method,"_nc_",nc,".",ext,sep=""), datatype = "INT1U")
    } 
    
  } ## --------------------------------------------------------------------------------------------------------------------- ##
  
  if(verbose) cat("\ndone.\n\n")
  
  if(getRstStack && calcIntCriteria){
    #names(intCritKM) <- paste("k",k,sep="_")
    attr(rstStack,"intClustCrit") <- intCritRes
    return(rstStack)
  }
  
  if(!getRstStack && calcIntCriteria){
    #names(intCritKM) <- paste("k",k,sep="_")
    return(intCritRes)
  }
}

