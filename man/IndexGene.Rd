\name{IndexGene}
\alias{IndexGene}
\title{Index Gene}
\description{Find all the genes that cluster with an index gene of interest.}
\usage{IndexGene(clusterresults, indexgene)}
\arguments{
  \item{clusterresults}{A set of clustering results produced by the use of
	\code{\link{autoCompareClustering}} or \code{\link{CompareClustering}}.}
  \item{indexgene}{Gene Identifier for gene of interest.  Note that 		
	this identifier must exist as a gene name in the result set. 
	If not, the function will return an error.}
}
\value{
  \item{rankedmatrix}{A matrix of genes sorted by frequency (i.e., 		
	the number of clustering methods in which that gene clustered
	with the index gene).  Rows correspond to the genes. The
	first column gives the frequency, the next columns show the
	"votes" of each method (i.e., whether the gene clustered
	with the index gene in that method or not).}
}

\author{Ted Laderas (laderast@ohsu.edu)}

\details{This function works on a result set as output by 
  \code{autoCompareClustering} or \code{CompareClustering}.
  Index genes can be any gene that is of interest to the user.
  This function allows users to cut across methods and see
  where they agree, leveraging the power of several clustering
  methods at once.  This can be of interest when attempting
  to infer function of an unknown gene based on its membership
  of a cluster.
}
\seealso{ \code{\link{ClusterReport}}
}
\examples{
  ##Load Cho data result set
  data(choresults)
  ##MYO1 == YHRO23w
  gene.list <- IndexGene(choresults, "YHR023w")
  ##show index gene list
  gene.list
}
\keyword{cluster}
