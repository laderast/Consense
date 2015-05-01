\name{ClusterReport}
\alias{ClusterReport}

\title{ Cluster Report }
\description{
  After the result set has been returned by
    CompareClustering, generates all tables and relevant 
    figures for that result set.
}
\usage{
ClusterReport(clusterresults, write=FALSE, filebase)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterresults}{ A result set from CompareClustering. }
  \item{write} {If TRUE, write cluster results to a file.  If FALSE, display cluster results only.}
  \item{filebase}{ A base file name.  Files will be saved in the 
   current directory.}
}

\value{
  The within metrics are displayed as a table and the cluster
  results are saved as a graph.

  Files are saved in the current working directory.
    Currently the following files are saved:
    \item{filebase within.txt}{A tab delimited text file of
      the various metrics, as described in withinmetrics().}
    \item{filebase clusters.txt}{A tab-delimted text file of
      all clusterings by assignment.}
    \item{filebase jaccard.jpg}{A Jpeg of the metaclustering
      dendrogram for the Jaccard Metric, comparing all methods.  
      Note that the clustering is actually done on (1-Jaccard).}
    \item{filebase jaccard.txt}{A text file of the actual Jaccard 
      metrics in tab-delimited format.}
    \item{filebase variation.jpg}{A Jpeg of the metaclustering
      dendrogram for the Variation of Information metric,
      comparing all methods.}
    \item{filebase variation.txt}{A text file of the actual 
      Variation metrics in tab-delimited format.}

}
\author{ Ted Laderas (laderast@ohsu.edu) }
\seealso{ \code{\link{withinmetrics}}, \code{\link{betweenmetrics}},
    \code{\link{CompareClustering}} 
}
\examples{
    data(choresults)
    #display results and save files to current working directory
    ClusterReport(choresults, write=TRUE, filebase="choresults")
}
\keyword{ cluster }% at least one, from doc/KEYWORDS
