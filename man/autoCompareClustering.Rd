\name{autoCompareClustering}
\alias{autoCompareClustering}
\title{Compare Clusterings Automatically}
\description{Given a number of clusters k as input,
	cluster a dataset using multiple methods.
}
\usage{autoCompareClustering(data, kinput)
}
\arguments{
  \item{data}{A matrix of values to cluster by row.  To cluster
    by column, simply transpose the matrix.  However, note that
    the row names must be unique, as the framework uses the
    row names in calculating the between-method metrics.}
  \item{kinput}{A desired geometry (number of clusters) k, greater 
    than 1 and less than n (number of genes or samples).}
}
\value{
  \item{resultset}{A result set that can be read by 
  \code{\link{ClusterReport}} or \code{\line{IndexGene}}.}
}
\author{Ted Laderas (laderast@ohsu.edu)}
\details{This is a wrapper function for \code{\link{CompareClustering}} 
  that has suitable defaults for automatically clustering and producing
  results for a given k (number of clusters).  In the case of
  hierarchical methods, the dendrograms will be pruned at a height 
  that yields that number of clusters.

   Note that for SOM, we are essentially limiting it to a one dimensional
   map (k x 1). If two dimensional maps are required, 
   \code{\link{CompareClustering}} is a more appropriate function.
}
\seealso{
  \code{link{CompareClusterings}}
}
\examples{
  ##load data
  data(chocellcycle)
  ##look for 5 clusters
  resultset <- autoCompareClustering(chocellcycle, kinput=5)
  ##examine results
  ClusterReport(resultset, "test")
}
\keyword{cluster}