

#' Draw a random value for a given parameter
#' 
#' An auxiliary function used in \link{randomSearchOptim} to draw a random 
#' value of a parameter using an input range.
#' 
#' @param paramList A list object containing parameter ranges
#' 
#' @param n Number of values to draw from \link[stats]{runif}.
#' 
#' @return A matrix containing the parameter grid to be tested
#' 
#' @seealso \link[stats]{runif}
#' 
#' @examples 
#' 
#' params <- list("thresh"  = c(0.1,0.5),
#'                "minsize" = c(10,20))
#'
#' drawRandom(paramList = params, n = 10)
#' 
#' 
#' @importFrom stats runif
#' 
#' @export

drawRandom <- function(paramList, n){
  
  paramGrid <- matrix(unlist(lapply(paramList, 
                                    FUN = function(x, n) runif(n, x[1], x[2]), n = n)), 
                      byrow = FALSE, nrow = n)
  colnames(paramGrid) <- names(paramList)
  return(paramGrid)
}


#' Adjust parameter ranges
#' 
#' An auxiliary function used in \link{randomSearchOptim} to adjust ranges around a 
#' point. This adjustement is required to draw a random set of points in the vicinity of the best 
#' central point in a given iteration.
#' 
#' @param paramList An input list containing the parameter ranges
#' 
#' @param centralPoint A vector containing coordinates (parameter values) for the best point. 
#' It must have the same size as \code{length(paramList)}. 
#' 
#' @param neighSizeProp Size of the neighbourhood for a given parameter, i.e., a real value contained 
#' in ]0, 1] used to multiply the range size as: \eqn{neighSize = (max_{range} - min_{range}) \times neighSizeProp}.
#' 
#' @return A list object containing the updated ranges.
#' 
#' @examples 
#' 
#' params <- list("thresh"  = c(0.1,0.5), "minsize" = c(10,20))
#' 
#' adjustRangesFromCentralPoint(paramList = params, 
#' centralPoint = c(0.15, 12), neighSizeProp = 0.025)
#' 
#' @export

adjustRangesFromCentralPoint <- function(paramList, centralPoint, neighSizeProp = 0.025){
  
  if(length(centralPoint) != length(paramList)){
    stop("Different size between centralPoint and paramList")
  }
  
  paramIncrements <- lapply(paramList, FUN = function(x, ns) (x[2] - x[1]) * ns, ns = neighSizeProp)
  newParamList <- paramList
  
  for(i in 1:length(paramList)){
    
    # Minimum and max values
    minVal <- paramList[[i]][1]
    maxVal <- paramList[[i]][2]
    newMinVal <- centralPoint[i] - paramIncrements[[i]][1]
    newMaxVal <- centralPoint[i] + paramIncrements[[i]][1]
    
    # Check if new values are within the user-defined range
    if(newMinVal < minVal) newMinVal <- minVal
    if(newMaxVal > maxVal) newMaxVal <- maxVal
    # Set values to export
    newParamList[[i]][1] <- newMinVal
    newParamList[[i]][2] <- newMaxVal
    
  }
  return(newParamList)
}


#' Iterate the fitness function
#' 
#' A wrapper function used to iterate an input fitness/objective function in 
#' sequential or parallel form. Uses as input a matrix of parameters which represent 
#' points randomly drawn for a certain range of parameter values.
#' 
#' @param fitFunc A fitness/objective function object to iterate (the first input of 
#' this function must be a vector of parameters to be tested (named \code{x}), following 
#' the order of elements in a parameter list).
#' 
#' @param paramGrid A matrix object containing random parameters to be tested generated 
#' by \link{drawRandom}.
#' 
#' @param parallel Use parallel loop? (default: FALSE). Uses \%dorng\% as the foreach backend.
#' 
#' @param ... Additional parameters passed to the fitness function.
#' 
#' @return A vector with the fitness/objective function values. In case of error it will return \code{NA} as 
#' output.
#' 
#' @import doRNG
#' 

iterateFitnessFunc <- function(fitFunc, paramGrid, parallel=FALSE, ...){
  
  if(!parallel){
    fitValues <- vector(mode="numeric", length = nrow(paramGrid))
    
    for(i in 1:nrow(paramGrid)){
      fitValue <- try(fitFunc(x = paramGrid[i, ], ...))
      
      if(inherits(fitValue, "try-error")){
        fitValues[i] <- NA
      }else{
        fitValues[i] <- fitValue
      }
    }
    return(fitValues)
  } 
  else{
    ## Parallel version
    fitValues <- foreach(i = 1:nrow(paramGrid), .combine = "c") %dorng%{
      fitValue <- try(fitFunc(x = paramGrid[i, ],...))
      if(inherits(fitValue, "try-error")){
        NA
      }else{
        fitValue
      }
    }
    return(fitValues)
  }
}



#' Perform random search optimization
#' 
#' A simple function to perform iterative random search optimization using neighbourhood 
#' fixed size adjustement. See details section for a description of the algorithm. 
#' 
#' @inheritParams iterateFitnessFunc
#' @inheritParams adjustRangesFromCentralPoint
#' 
#' @param numIter Number of iterations used to run the algorithm (default: 250).
#' 
#' @param nneigh Number of neighbors (or parameter combinations) to generate in the vicinity 
#' of the best (default: 5).
#' 
#' @param initNeighs Number of parameter combinations to randomly draw from paramList at initialization 
#' (default: 5 * nneigh)
#' 
#' @param iter Number of sucessive iterations used to stop the algorithm if no improvement is found 
#' (default: 25).
#' 
#' @param maximize Should the value of the fitness function be optimized? (default: TRUE)
#' 
#' @param parallel Use a paralell backend? Use an integer value to define the number of 
#' instances to use (default: FALSE). Uses \link[GA]{startParallel} to initiate the parallel backend. 
#' 
#' @param seed Set seed value? Integer value (default: NULL).
#' 
#' @param verbose Print output messages? This will be passed along to other functions 
#' as well (default: TRUE).
#' 
#' @return A list object of size two containing:        
#' \itemize{
#'    \item bestFitValue - The value of fitness function for the best solution
#'    \item bestParams - Parameters used to generate the best solution 
#' } 
#' 
#' @details 
#' 
#' The algorithm proceeds as follow (for maximization):         
#' 
#' \preformatted{           
#' 
#' for i in 1:numIter\{        
#'         
#'   if i=1 then start\{           
#'     
#'     Randomly draw n = initNeighs parameter combinations bounded by paramList:       
#'       randParamGrid <- drawRandom(paramList)
#'       
#'     Run the fitness/objective function: 
#'       fitValues <- fitFunc(randParamGrid)
#'     
#'     Set the best output of the fitness function: 
#'       bestFitValue <- fitValues[bestIdx]               
#'     
#'     Set the best parameter combination: 
#'       bestParam <- randParamGrid[bestIdx, ]         
#'     
#'     Adjust parameter ranges to the neighbourhood of bestParam: 
#'       bestParamList <- adjustParamRangesNeigh(bestParam, neighSizeProp)    
#'     
#'     Set the stopping counter variable: 
#'       stopCounter <- 0         
#'   \}
#'             
#'   if i > 1\{
#'     
#'     Randomly draw n = nneigh parameter combinations bounded by bestParamList:
#'       randParamGrid <- drawRandom(bestParamList)   
#'     
#'     Run the fitness/objective function: 
#'       fitValues <- fitFunc(randParamGrid)
#'     
#'     Set the best output of the fitness function for iteration i: 
#'       bestFitValue_i <- fitValues[bestIdx]         
#'                       
#'     Check if fitness value improved and update them in that case: 
#'       if(bestFitValue_i > bestFitValue)\{                 
#'                      
#'       Set new best fit value: 
#'         bestFitValue <- bestFitValue_i        
#'       
#'       Set new best parameter combination: 
#'         bestParam <- randParamGrid[bestIdx, ]         
#'       
#'       Adjust parameter ranges: 
#'         bestParamList <- adjustParamRangesNeigh(bestParam, neighSizeProp)          
#'       
#'       Reset the stopping counter on update: 
#'         stopCounter <- 0           
#'       
#'     \}else\{        
#'       Update the stop counter variable: 
#'         stopCounter <- stopCounter + 1          
#'     \}      
#'    
#'     if(stopCounter >= iter)\{
#'        return(bestFitValue, bestParamList)             
#'     \}                       
#'   \}                   
#' \}
#' } 
#'             
#'   
#' The neighbourhood range vector of the best set of parameters for a given iteration is defined as:  
#'                     
#' \eqn{neighRange = [min_{range}(bestParam) - neighSize; max_{range}(bestParam) + neighSize;] }
#' where,                 
#' \eqn{neighSize = (max_{range}(bestParam) - min_{range}(bestParam)) \times neighSizeProp}
#' 
#' If any parameter in neighRange is outside the initial ranges then it is ajusted for these values. 
#' 
#' @examples
#' 
#' fitFunc <- function(x) return(cos(3*pi*x) / x)
#'  
#' bestFit <- randomSearchOptim(fitFunc, paramList=list(x=c(0, 1)), numIter=500, nneigh = 5, 
#'                   neighSizeProp = 0.01, maximize = FALSE, iter = 50, 
#'                  parallel = FALSE, seed=NULL, verbose = TRUE)
#'
#' @export
#'                 

randomSearchOptim <- function(fitFunc, paramList, numIter = 250, nneigh = 5, 
                              initNeighs = (5 * nneigh), neighSizeProp = 0.025,  iter = 25, 
                              maximize = TRUE, parallel = FALSE, seed = NULL, verbose = TRUE, ...){
  
  # Set seed value
  if(!is.null(seed))
    set.seed(seed)
  
  
  if(verbose && parallel){
    
    # Start parallel computing (if needed) ------------------------------------------
    
    if(verbose && parallel) cat("-> Starting cluster for parallel execution...\n")
    
    if(is.logical(parallel)){ 
      # Start cluster with all available cores
      if(parallel){
        parallel <- GA::startParallel(parallel) 
      } # Don't use parallel
      else{
        parallel <- FALSE
      }  
    }
    else{ 
      # Integer with number of cores or cluster object
      parallel <- GA::startParallel(parallel) 
    }
    
    if(parallel) on.exit(parallel::stopCluster(attr(parallel, "cluster")))
    
    if(verbose && parallel) cat("done.\n\n")
  }
  

  for(i in 1:numIter){

    ## Start the algorithm ----------------------------------------------------------------
    ##
    if(i==1){
      
      # Draw random 
      randParamGrid <- drawRandom(paramList, n = nneigh)

      # Run fitness
      fitValuesNeigh <- suppressMessages(iterateFitnessFunc(fitFunc, randParamGrid, parallel, ...))

      if(all(is.na(fitValuesNeigh))){
        stop("All rounds returned invalid values for the fitness/objective function. Please review input parameter ranges.
             You may also want to increase nneigh and rerun.")
      }
      
      # Pick best
      # set as central point
      idxBest <- ifelse(maximize, which.max(fitValuesNeigh), which.min(fitValuesNeigh)) 
      bestFitValue <- fitValuesNeigh[idxBest]
      bestParams <- randParamGrid[idxBest, ]
      
      # Adjust ranges
      bestParamRanges <- adjustRangesFromCentralPoint(paramList, bestParams, neighSizeProp)
      
      if(!is.null(iter)){
        iterStop <- 0
      }
    
    ## Run until i=numIter or for n = iter iterations without improvement ------------------
    ##
    }else{
      
      # Draw random 
      randParamGrid <- drawRandom(bestParamRanges, n = nneigh)
      
      # Run fitness
      fitValuesNeigh <- suppressMessages(iterateFitnessFunc(fitFunc, randParamGrid, parallel, ...))
      
      if(all(is.na(fitValuesNeigh))){
        stop("All rounds returned invalid values for the fitness/objective function. Please review input parameter ranges. 
             You may also want to increase nneigh and rerun.")
      }
      
      # Pick best -----------------------
      # set as central point
      idxBest <- ifelse(maximize, which.max(fitValuesNeigh), which.min(fitValuesNeigh)) 
      bestFitValue_i <- fitValuesNeigh[idxBest]
      bestParams_i <- randParamGrid[idxBest, ]
    
      if(maximize)
        updateBest <- ifelse(bestFitValue_i > bestFitValue, TRUE, FALSE)
      else
        updateBest <- ifelse(bestFitValue_i < bestFitValue, TRUE, FALSE)
      
      
      # Update the best values ---------
      #
      if(updateBest){
        
        # Adjust best fit value
        bestFitValue <- bestFitValue_i
        # Adjust best parameter set
        bestParams <- bestParams_i
        # Adjust ranges
        bestParamRanges <- adjustRangesFromCentralPoint(paramList, bestParams, neighSizeProp)

        if(!is.null(iter)){
          iterStop <- 0
        }
      }else if(!is.null(iter)){
        iterStop <- iterStop + 1
      }
    }

    # Print iteration results
    if(verbose){
      cat("[ Iter:",i,"] Best value:", bestFitValue, "\n")
      cat("Best parameters:\n")
      print(bestParams)
      cat("\n\n")
    }
    
    if(!is.null(iter)){
      if(iterStop >= iter){
        return(list(bestFitValue = bestFitValue, 
                    bestParams = bestParams))
      }
    }
  }
  
  return(list(bestFitValue = bestFitValue, 
              bestParams = bestParams))
}



