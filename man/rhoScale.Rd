% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhoScale.R
\name{rhoScale}
\alias{rhoScale}
\title{rhoScale: scale lambda_z depending on magnitude. For internal package use only.}
\usage{
rhoScale(K, p, rhoVec, itrs = 10000)
}
\arguments{
\item{K}{An integer - number of tasks}

\item{p}{An integer - dimension of covariates}

\item{rhoVec}{A vector of integers}

\item{itrs}{An integer}
}
\value{
A matrix or datafame with lambda_z hyperparameter scaled appropriately depending on sparsity level.
}
\description{
rhoScale: scale lambda_z depending on magnitude. For internal package use only.
}
