\name{clusterCenters}
  \alias{clusterCenters}
  \title{Cluster Center function}
  \description{
    Given a matrix of data with its cluster assignments,
    return a matrix of the cluster centers.  Note that 
    several euclidean-based methods will return cluster
    centers.
  }
  \usage{clusterCenters(x)}
  \arguments{
    \item{x}{a data frame whose first column consists of
      cluster assignments. Note these cluster assignments
      must be in the form of integers.}
  }
  \value{
    \item{centers}{A matrix whose rows correspond to the vector
      mean (the cluster center) of all the elements in a 
      particular cluster. For example, centers[1,] contains the 
      vector mean for all the elements with cluster assignment 1. 

      Note that if there are no elements in a cluster (as in the
      case of SOM with an unused map), that row will have NAs.}		
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \examples{
  ## = don't run
  ##clusterassign is a vector of cluster assignments
  ##such as the output from kmeans
  ##data is matrix of actual data

  #example of use using k-means
  x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
  cl <- kmeans(x, 2, 20)
  clusterassign <- cl$cluster

  clustmatrix <- cbind(clusterassign, x)
  centers <- clusterCenters(as.data.frame(clustmatrix))
  }
  \keyword{cluster}