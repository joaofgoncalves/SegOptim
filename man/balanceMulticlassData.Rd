% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_DataBalancing.R
\name{balanceMulticlassData}
\alias{balanceMulticlassData}
\title{Balance multi-class data}
\usage{
balanceMulticlassData(x, class, n)
}
\arguments{
\item{x}{A data frame to balance.}

\item{class}{The name or the position of the column identifying the class 
(the column must be a factor variable).}

\item{n}{Number of rows/observations to extract from each class? (integer)}
}
\value{
A data.frame with n rows by class.
}
\description{
A simple function to perform data balancing for multi-class datasets and 
classification problems. It does over-sampling (or sampling with replacement) 
if the frequency value of the target class is below n and down-sampling (or 
sampling without replacement) if the frequency is above n.
}
\note{
This function is just a rough fix for handling unbalanced datasets, 
use with care!...
}
