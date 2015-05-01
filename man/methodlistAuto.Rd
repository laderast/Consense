\name{methodlistAuto}
\alias{methodlistAuto}
\title{methodlistAuto}
\description{Given an input kinput (number of clusters), generate
   a methodlist for use by \code{link{clusterList}}.
}
\usage{methodlistAuto(methodlistAuto(kinput)}
\arguments{
  \item{kinput}{A desired geometry (number of clusters) k, greater 
    than 1 and less than n (number of genes or samples).}
}
\value{
  \item{methodlist}{A list of methods and associated parameters
    passed on to \code{link{clusterList}}.}
}
\details{Extending this code is potentially the easiest way to
  add a new method for benchmarking.  Details to follow.
}
\author{Ted Laderas (laderast@ohsu.edu)}
\seealso{
  \code{\link{getClusterInfo}}
}
\keyword{cluster}
