\name{betweenmetrics}
\alias{betweenmetrics}
\title{Between-Method Metrics}
\description{Given a list of clusterings, calculates the between
  method metrics between them.
}
\usage{betweenmetrics(clusterlist)
}
\arguments{
  \item{clusterlist}{a list of lists which contains clusterings 
    and parameters as supplied by \code{clusterList}.}
}
\value{A list of the following matrices:
  \item{jaccardresult}{a matrix that contains all possible 
    comparisons between clusterings using the jaccard index.
    currently this is formatted much like a distance matrix,
    with the columns corresponding to the first 1:n-1 methods
    and the rows corresponding to the 2:n methods.}
  \item{variationresult}{a matrix that contains all possible
    comparison between clusterings using the variation of
    information.  Similarly formatted to the matrix for
    jaccardresult.}
}
\details{Calculates the between-method metrics between all
  clusterings as supplied by \code{clusterlist}.   
}
\author{Ted Laderas (laderast@ohsu.edu)}
\seealso{
  \code{\link{clusterList}}
  \code{\link{variationInformation}}
  \code{\link{jaccard}}
}
\keyword{cluster}

