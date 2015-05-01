\name{chocellcycle}
  \docType{data}
  \alias{chocellcycle}
  \title{Cho Yeast Cell Cycle Dataset}
  \description{
    A small subset of data derived from the Cho yeast cell cycle 
    dataset by Yeung. The Cho dataset consists of 17 time points 
    sampled every 10 minutes from yeast cells undergoing mitotic 
    division.  The yeast cells were synchronized to start 
    in the late G1 phase by raising their temperature to 37ºC, 
    which halted them in the late G1 phase.  Shifting their 
    temperature to 25ºC reinstated the cell cycle.  

    The 17 time points roughly correspond to two full cell cycles. 
    The Cho dataset was part of Eisen’s original dataset for his 
    paper showing the success of clustering. Therefore, we would 
    expect that the clustering methods should pick up relatively 
    similar clustering patterns.  

    This data has been subsetted and normalized using processes 
    described in \url{http://faculty.washington.edu/kayee/cluster/README_cellcycle.txt}.
   }
  \usage{data(chocellcycle)}
  \format{A matrix of 384 genes (rows) and 17 time points (columns).}
  \source{\url{http://faculty.washington.edu/kayee/cluster/}}
  \references{
     Yeung, K.Y., D.R. Haynor, and W.L. Ruzzo, \emph{Validating 
     clustering for gene expression data.} Bioinformatics, 2001. 
     17:309-18.

     Cho, R.J., et al., \emph{A genome-wide transcriptional analysis of 
     the mitotic cell cycle.} Mol Cell, 1998. 2:65-73
  }
  \keyword{datasets}
