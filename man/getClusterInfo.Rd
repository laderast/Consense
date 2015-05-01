\name{getClusterInfo}
\alias{getClusterInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get Clustering Info }
\description{
  Query the user about appropriate parameters for clustering.
}
\usage{
getClusterInfo(useGUI = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{useGUI}{ Currently not implemented.  This is based on
   TkWidgets code and is too buggy.  Leave as false.}
}
\details{
  This program will query user as to which methods to use on
  the command line.  If hierarchical methods are used, the program
  will ask the user to either input a desired number of clusters or
  if they want the option of handcutting their clusters.
}
\value{
   \item{methodlist}{a list of methods and associated parameters
    passed on to \code{link{clusterList}}.}
}
\author{ Ted Laderas (laderast@ohsu.edu) }
\seealso{\code{\link{methodlistAuto}}}
\keyword{ cluster }% at least one, from doc/KEYWORDS
