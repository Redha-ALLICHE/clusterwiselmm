% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_response.R
\name{predict_response}
\alias{predict_response}
\title{Returns the predicted response of the clusterwise lmm to the data given a clustering}
\usage{
predict_response(data, clusters, models)
}
\arguments{
\item{data}{data frame containing the variables in which the clmm model has been trained.}

\item{clusters}{vector that indicates the clustering. Must contain a number of clusters equal to
the number of models.}

\item{models}{object of class clmm that contains a list of k lmm models.}
}
\value{
vector containing the prediction.
}
\description{
Returns the predicted response of the clusterwise lmm to the data given a clustering
}
