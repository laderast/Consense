\name{variationInformation}
  \alias{variationInformation}
  \title{Variation of Information}
  \description{Given two clusterings, this function calculates 
    the Variation of Information between them.  Variation of 
    Information can be thought of as what information content 
    is lost and what information content is gained by choosing 
    one clustering over the other clustering. 
  }
  \usage{variationInformation(clustering1, clustering2)}
  \arguments{
    \item{clustering1}{A clustering as defined by the cluster 
      output functions of this package.  See \code{\link{outputcluster}} 
      for more details.}
    \item{clustering2}{same as clustering 1.}
  }
  \value{
    \item{variation}{Variation of Information. A value that ranges
      from 0-1.  The more the clusterings are in agreement, the
      closer this value is to 0.}
  }
  \author{Ted Laderas (laderast@ohsu.edu)}
  \details{The Variation of Information (from Meila) is a quantity 
    based on the entropy H(C) of a clustering C, the entropy
    H(C') of the second clustering C', and the mutual 
    information I(C,C') between these two clusterings.  See 
    Meila for more information on how entropy and mutual
    information are calculated.  
	
    Variation of Information can be thought of as what
    information content is lost and what information content
    is gained by choosing one clustering over the other
    clustering.  Unlike more conventional methods of counting
    cluster membership (such as Jaccard Index), the value can 
    be interpreted when there are different numbers of clusters 
    in the two clusterings. 

    The Variation of information is calculated by the following
    equation: \eqn{H(C) + H(C') - 2I(C, C')}
  }
  \references{Meila, M. Comparing Clusterings. Technical Report 418. 2002,
    University of Washington Statistics Department: Seattle.
  }
  \seealso{
    \code{\link{jaccard}}
  }
  \examples{
    data(choresults)
    clusts <- choresults$clusters
    #calculate Variation of Information from clusterings
    variationInformation(as.data.frame(clusts[[c("UPGMACOR")]]), 
    as.data.frame(clusts[[c("UPGMAEUC")]]))
    }
  \keyword{cluster}