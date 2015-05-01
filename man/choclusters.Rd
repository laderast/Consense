\name{choclusters}
  \docType{data}
  \alias{choclusters}
  \title{Original Cho Cell Cycle Cluster Assignments}
  \description{
    This is the set of the original cluster assignments assigned
    by Cho in his paper.  This dataset consisted of 384 genes 
    separated by Cho into 5 clusters by phase. Genes whose 
    transcripts peaked in a particular phase of the cell cycle were 
    classified in a cluster belonging to that phase. For example, 
    a gene whose transcript peaked in the early G1 phase was classified 
    as belonging to the Early G1 cluster. Genes whose transcripts peaked 
    in multiple phases were removed by Yeung for this subset. Because 
    this dataset was separated visually by peak, we do not expect 
    clustering methods to completely reproduce the original clusterings.
    
    Here is a legend to the assignments:
    1 - early G1 phase
    2 - late G1 phase
    3 - S phase
    4 - G2 phase
    5 - M phase
   }
  \usage{data(choclusters)}
  \format{A one column matrix of factors with cluster assignments
    for each gene.}
  \source{ \url{http://faculty.washington.edu/kayee/cluster/} 
   }
  \references{
     Cho, R.J., et al., \emph{A genome-wide transcriptional analysis of 
     the mitotic cell cycle.} Mol Cell, 1998. 2:65-73

     Yeung, K.Y., D.R. Haynor, and W.L. Ruzzo, \emph{Validating 
     clustering for gene expression data.} Bioinformatics, 2001. 
     17:309-18.
  }
  \keyword{datasets}
