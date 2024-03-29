% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOptim_PerformanceEval.R
\name{generateConfusionMatrix}
\alias{generateConfusionMatrix}
\title{Generate a confusion matrix}
\usage{
generateConfusionMatrix(obs, pred)
}
\arguments{
\item{obs}{A factor or integer vector with observed values (class labels).}

\item{pred}{A factor or integer vector with predicted values (class labels).}
}
\value{
A matrix object with columns correspoding to observed and rows to 
predicted values/classes.
}
\description{
A function used to generate a confusion matrix for classification evaluation.
}
\examples{
 
obs <- sample(0:1, 20, replace = TRUE)
pred <- sample(0:1, 20, replace = TRUE)
generateConfusionMatrix(obs, pred)

}
