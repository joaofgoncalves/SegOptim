

#' Perform data balancing
#' 
#' An internal wrapper function for performing data balancing for single-class problems. 
#' Two methods are available via the \code{unbalanced} package: i) over-sampling 
#' \code{\link[unbalanced]{ubOver}} or, ii) under-sampling \code{\link[unbalanced]{ubUnder}}
#' 
#' @param x A data frame or matrix for balancing (by default the two first columns are 
#' assumed to be the segment ID and the train labels).The response variable of the 
#' unbalanced dataset (i.e., the column named as "train") must be a binary factor 
#' where the majority class is coded as 0's and the minority (the class of interest) as 1's.  
#' 
#' @param method A string defining the method to apply. Either "ubOver" for over-sampling 
#' the minority class or "ubUnder" for under-sampling the majority class.
#' 
#' @return A data frame with balanced classes.
#' 
#' @examples 
#' 
#' DF <- data.frame(SID=1:100,train=c(rep(0,90),rep(1,10)),matrix(rnorm(1000),10,100))
#' 
#' DF.over<-dataBalancing(x=DF, method="ubOver")
#' DF.under<-dataBalancing(x=DF,method="ubUnder")
#' 
#' @export
#' @import unbalanced

dataBalancing<-function(x, method){
  
  if(method == "ubOver"){
    newDF <- unbalanced::ubOver(X=x[,-2], Y= x[,"train"])
    newDF <- data.frame(train=newDF$Y,newDF$X)
    return(newDF)
    
  }else if(method == "ubUnder"){
    
    newDF <- unbalanced::ubUnder(X=x[,-2], Y= x[,"train"])
    newDF <- data.frame(train=newDF$Y,newDF$X)
    return(newDF)
    
  }else{
    stop("Unknown data balancing method! Please review option balanceMethod")
  }
}



#' Balance multi-class data
#' 
#' A simple function to perform data balancing for multi-class datasets and 
#' classification problems. It does over-sampling (or sampling with replacement) 
#' if the frequency value of the target class is below n and down-sampling (or 
#' sampling without replacement) if the frequency is above n.
#' 
#' @param x A data frame to balance.
#'  
#' @param class The name or the position of the column identifying the class 
#' (must be a factor variable).
#'  
#' @param n Number of rows/observations to extract from each class? (integer)
#'  
#' @return A data.frame with n rows by class.
#'  
#' @note This function is just a rough fix for handling unbalanced datasets, 
#' use with care...

balanceMulticlassData <- function(x, class, n){
  
  if(!is.factor(x[,class]))
    stop("The variable in class must be a factor")
  
  tb <- table(x[,class])
  levs<-names(tb)
  ind<-(1:nrow(x))
  
  i<-0
  for(lev in levs){
    i<-i+1
    
    if(tb[lev] < n)
      replace <- TRUE
    else
      replace <- FALSE
    
    ind_n <- ind[x[,class] == lev]
    
    if(i==1){
      out <- x[sample(ind_n, n, replace=replace),]
    }
    else{
      out <- rbind(out, x[sample(ind_n, n, replace=replace),])
    }
  }
  
  return(out)
}
