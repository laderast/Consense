\name{CompareClustering}
\alias{CompareClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare Clusterings}
\description{
  Main framework for comparing clustering functions.
}
\usage{
CompareClustering(data, verbose = TRUE, auto = FALSE, kinput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A matrix of values to cluster by row.  To cluster
      by column, simply transpose the matrix.  However, note that
      the row names must be unique, as the framework uses the
      row names in calculating the between-method metrics. }
  \item{verbose}{ If TRUE, show progress of execution.  If not,
	  run without showing progress of execution. }
  \item{auto}{ If FALSE, run getClusterInfo to query user
      as to parameters.  If TRUE, use kinput as number of clusters
      for methodlistAuto }
  \item{kinput}{ For automated clustering, a value for k (number
	  of clusters) to be input to the clustering methods.  If
	  left blank or has a character value, the default mode is 
	  to allow users to hand cut their clusters using 
	  \code{\link{inspectTree}}.}
}
\value{
    \item{report}{A list containing the metrics. report$within
      contains a matrix of all the within-method metrics
      calculated.  report$between contains the two matrices
      comparing all methods: between$jaccardresult 
      contains the Jaccard Indices, and between$variationresult
      contains all Variation of information metrics.}
    \item{params}{A list of lists containing all parameters 
      input to the clustering methods. Note that many of these         
      parameters are unique to the clustering method. params[[i]]
      contains a list of parameters for method i. (Example:
      params[[i]]$id will give the identifier for the method 
      used to label the output by the ClusterReport.)}
    \item{clusters}{A list of lists. clusters[[i]] contains a
      vector corresponding to the cluster assignments for the
      method specified in params[[i]].}
}
\author{Ted Laderas (laderast@ohsu.edu)}

\seealso{ \code{\link{ClusterReport}}, \code{\link{clusterList}},
    \code{\link{methodlistAuto}}, \code{\link{getClusterInfo}},
    \code{\link{autoCompareClustering}}
}

\examples{
  data(chocellcycle)
    ##run with input geometry of 5 and verbose output
    results <- CompareClustering(chocellcycle, verbose=TRUE, auto = TRUE, kinput=5)
    ##display results
    ClusterReport(results)
}

\keyword{cluster}% at least one, from doc/KEYWORDS