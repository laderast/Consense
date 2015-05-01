\name{clusterList}
  \alias{clusterList}
  \title{Cluster List}
  \description{Takes a list of clustering methods and parameters 
    and produces the clusterings.}
  \usage{clusterList(data, methodlist)}
  \arguments{
    \item{data}{A data matrix to be clustered.  Note that if
      there are missing values, how the missing values are
      handled (interpolation, etc.) is dependent on the 
      clustering method.  For an example, look at hclust().}
    \item{methodlist}{A list of methods and their associated
      parameter.  Note that the cluster output functions 
      are passed into clusterList, which does the actual 
      clustering.}
  }
  \value{A cluster list. A list of lists, each slot containing
    the following:
    \item{clustering}{A one column data frame whose first column 
      corresponds to the cluster assignments. The row names and 
      ordering remain the same.}
    \item{params}{a list of parameters provided to that method.
      currently provided by getClusterInfo().}    
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \details{see \code{\link{ClusterOutput}} for more details about
  writing wrapper functions to get your clustering method to
  work with this function.
  }
  \seealso{
    \code{\link{getClusterInfo}}
    \code{\link{methodlistAuto}}
  }  
  \keyword{cluster}
