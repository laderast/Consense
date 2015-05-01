\name{homogeneity}
  \alias{homogeneity}
  \title{Calculates the homogeneity of a clustering.}
  \description{Given cluster output from a suitable clustering function,
    calculate the Homogeneity (from Shamir, et al.) of the
    clusterings. 
		
    Homogeneity is a measure of the within-cluster variance.
    See Shamir, et al, for how it is calculated.	     
  }
  \usage{homogeneity(clustering, centers, distfunc="euclidean")} 
  \arguments{
    \item{clustering}{Cluster output from a clustering function.
    the output should be in matrix format with the cluster assignments being 
    the first column in that matrix, with the actual data in remaining 
    columns.}
    \item{centers}{Cluster Centers as returned by \code{\link{ClusterCenters}}.
    } 
    \item{distfunc}{Distance function to be used.  Currently only "euclidean"
      or "correlation" are supported.}
  }
  \value{\item{homogeneity}{An unbounded, non-negative scalar that represents
    the homogeneity of the clustering.}
  }
  \references{
    Shamir, et al. Algorithmic Approaches to Clustering Gene Expression
    Data. in \emph{Current Topics in Computational Molecular Biology}.
    MIT Press: Boston.
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \examples{
    ##calculate homogeneity for Cho et al's clusters
    data(chocellcycle)
    data(choclusters)
    cho <- cbind(as.numeric(choclusters), chocellcycle)
    cent <- clusterCenters(as.data.frame(cho))
    homogeneity(cho, cent)
  }
  \keyword{cluster}