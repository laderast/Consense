\name{withinmetrics}
\alias{withinmetrics}
\title{Within-Method Metrics}
\description{Framework for calculating the within-method metrics
  for a list of methods.
}
\usage{withinmetrics(clusterlist, data)}
}
\arguments{
  \item{clusterlist}{a list of lists which contains clusterings 
    and parameters as supplied by clusterList().}
  \item{data}{A matrix of expression values.}
}
\details{Given a list of parameters and clusterings as supplied 
  by clusterList, calculates and returns a matrix with all of the
  relevant within-method clustering metrics.

  Currently, withinresult calculates number of clusters, 
  homogeneity, separation and average silhouette width.  Note 
  that this average silhouette width is correlation-based.  See
  silhouette() in the cluster package for more information.
}
\value{
  \item{withinresult}{a matrix whose columns correspond to the
    metrics calculated and whose rows correspond to the methods
    these methods were calculated on.}
}
\author{Ted Laderas (laderast@ohsu.edu)}
\seealso{
  \code{\link{clusterList}}
  \code{\link{homogeneity}}
  \code{\link{separation}}
  \code{\link{silhouette}}
}
\keyword{cluster}