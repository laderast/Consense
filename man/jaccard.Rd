\name{jaccard}
  \alias{jaccard}
  \title{Jaccard Index}
  \description{Given two clusterings C and C', this function calculates 
    the Jaccard Index between the two clusterings.  The jaccard index
    compares the two clusterings by counting and comparing pairs of elements
    across the two clusterings.}
  \usage{jaccard(clustering1, clustering2)}
  \arguments{
    \item{clustering1}{A clustering as defined by the cluster 
      output functions of this package.  See ?outputcluster() 
      for more details.}
    \item{clustering2}{same as clustering 1.}
  }
  \value{
    \item{jaccard index}{Jaccard index comparing clustering 1 and 2. A 
      value that ranges from 0-1.  The more the clusterings are in
      agreement, the closer the Jaccard index will be to 1.}
  }
  \details{Given two clusterings C and C', the Jaccard Index will compare 
    the elements in common between them.  This is done by generating an 
    contingency table, whose Nij entry correpsonds to how many elements 
    in common cluster Ci has in common with cluster C'j.  
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \seealso{
    \code{\link{variationInformation}}
  }
  \references{
  Dudoit, S. and Fridlyand, J. A prediction-based resampling method for
  estimating the number of clusters in a dataset. \emph{Genome Biology.} 
  2002. 3: p. RESEARCH036.

  Jain, A.K. and Dubes R.C. \emph{Algorithms for Clustering Data.} 
  Englewood Cliffs, NJ: Prentice Hall. 1988.
  } 

  \examples{
  data(choresults)
  clusts <- choresults$clusters
  jaccard(as.data.frame(clusts[[c("UPGMACOR")]]), 
    as.data.frame(clusts[[c("UPGMAEUC")]]))
  }

  \keyword{cluster}
