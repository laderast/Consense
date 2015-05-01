\name{kmeansOutput}
\alias{kmeansOutput}
\alias{dianaOutput}
\alias{agglomOutput}
\alias{somOutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Clustering Output functions}
\description{
  Wrapper functions for clustering methods that pass on
  appropriate parameters and produce standardized output for
  \code{\link{betweenmetrics}} and \code{\link{withinmetrics}}.
}
\usage{
  kmeansOutput(matrix, params)
  dianaOutput(matrix, params)
  agglomOutput(matrix, params)
  somOutput(matrix, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{ A matrix of expression values.  Commonly, this corresponds
   to the columns corresponding to samples and the rows corresponding
   to genes. }
  \item{params}{ A list of params supplied to the function.}
}
\details{
  In order to add a clustering method, you must add a wrapper function
   that implements the interface clusterOutput(data, params).  Params can be 
   passed onto the function by rewriting methodlistAuto (the easy way) or
   by producing a necessary param list by adding code to getClusterInfo
   (the more difficult way).  The wrapper function must translate 
   the output format of the clustering method into the clustering output 
   format described above.
}
\value{
\item{clustering}{A data frame with only one cluster whose row values are
    factors that correspond to the cluster that gene belongs to.  Note
    that these must be factors; otherwise the clustering cannot be utilized
    by the rest of the Consense package}
}
\author{Ted Laderas (laderast@ohsu.edu}
\keyword{ cluster }% at least one, from doc/KEYWORDS
