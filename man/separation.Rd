\name{separation}
  \alias{separation}
  \title{Average Separation of a clustering.}
  \description{Given cluster output from a suitable clustering function,
    calculate the separation (from Shamir, et al.) of the
    clusterings. Homogeneity can be thought of as a measure of the
    average between-cluster distance within a clustering.			     
  }
  \usage{separation(clustering, centers)}
  \arguments{
    \item{clustering}{Cluster output from a clustering function.
      the output should be in matrix format with the cluster assignments being 
      the first column in that matrix, with the actual data in remaining 
      columns.}
    \item{centers}{Cluster centers from \code{\link{clusterCenters}}.}
  }
  \value{\item{separation}{An unbounded, non-negative scalar that represents
    the separation of the clustering.}
  }
  \references{
    Shamir, et al. Algorithmic Approaches to Clustering Gene Expression
    Data. in \emph{Current Topics in Computational Molecular Biology}.
    MIT Press: Boston.
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \examples{##=don't run
    ##calculate separation for Cho et al's clusters
    data(chocellcycle)
    data(choclusters)
    cho <- cbind(as.numeric(choclusters), chocellcycle)
    cent <- clusterCenters(as.data.frame(cho))
    separation(cho, cent)
  }
  \keyword{cluster}