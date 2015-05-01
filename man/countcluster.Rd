\name{countcluster}
\alias{countcluster}
\title{produce a vector of cluster counts of each  cluster}
\description{counts the number within each cluster and produces
   a vector count.  Internal function.
}
\usage{countcluster(output)
}
\arguments{
  \item{output}{a clustering result as supplied by \code{\link{clusterList}}.}
}
\value{
  \item{count}{A vector of counts for each cluster.}
}
\keyword{cluster}