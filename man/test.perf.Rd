% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.perf.R
\name{test.perf}
\alias{test.perf}
\alias{test.perf.exh}
\alias{test.perf.exhlist}
\alias{test.perf.exhcv}
\alias{test.perf.exhcvlist}
\title{Performance on a test data set}
\usage{
test.perf(object, ...)

\method{test.perf}{exh}(object, newdata = object$test.data, ...)

\method{test.perf}{exhlist}(object, ...)

\method{test.perf}{exhcv}(object, ...)

\method{test.perf}{exhcvlist}(object, ...)
}
\arguments{
\item{object}{the model}

\item{...}{additional arguments to and from other methods}

\item{newdata}{a \code{\link{data.frame}} containing the test data}
}
\description{
Calculates the fit on a test data.
}
