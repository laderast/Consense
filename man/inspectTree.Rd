\name{inspectTree}
\alias{inspectTree}
\title{Inspect Dendrograms}
\description{Inspect a clustering dendrogram and cut it into a clustering.
}
\usage{inspectTree(tree, id)
}
\arguments{
  \item{tree}{A dendrogram as produced by hclust or as.hclust}
  \item{id}{An identifier to be displayed on the plot.}
}
\value{
  \item{clustering}{}
}
\details{This is an internal function to the Consense package 
  that utilizes \code{\link{identify}} to plot and prune clusterings by
  selecting branch points. When the user is finished, the program
  transforms the clusterings into the format required by Consense.
}
\keyword{cluster}
