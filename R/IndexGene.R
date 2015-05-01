"IndexGene" <-
function(clusterresults, indexgene) {

  #function takes as input resultset and index genename
  indexgene <- as.character(indexgene)
  clusts <- clusterresults$clusters

  #check to see if indexgene exists in resultset
  exist <- as.numeric(intersect(rownames(clusts[[1]]), indexgene))
  if(length(exist) == 0) {stop("indexgene is not valid")}
  
  methodnames <- names(clusts)
  
  #initialize data structures
  namelist <- list()
  dict <- vector()
  totalnum <- length(clusts)

  for(i in 1:length(clusts))
    {
    #indexcluster is the cluster number that contains the
    #indexgene
      indexcluster <- as.numeric(clusts[[i]][c(indexgene),])
      #print(i)
    #separate list of names by cluster
      by1 <- by(clusts[[i]], clusts[[i]], function(x) {names(x)})

    #grab all the names with indexcluster
      nam <- suppressWarnings(by1[[c(as.character(indexcluster))]])
      #print(nam)

    #put the names in a list
      namelist[[i]] <- nam

    #add names to the dictionary vector   
      dict <- c(dict, as.vector(nam))
   }

  #take only unique entries and use them as dictionary
  dict <- suppressWarnings(unique(as.vector(dict)))
  #dict <- sort(dict)

  #initialize scoring matrix
  #all values zero
  scoremat <- matrix(ncol = length(methodnames), nrow = length(dict), 0)
  rownames(scoremat) <- dict
  colnames(scoremat) <- methodnames

  #calculate intersection between names[[i]] and dictionary
  #make those entries have a value of 1 (a vote of confidence)
  for(i in 1:length(namelist)){
    int <- intersect(dict, namelist[[i]])
    scoremat[c(int), i] <- 1
  }

  freq <- apply(scoremat, 1, mean) * 100
  #sort by decreasing frequency
  freq <- sort(freq, decreasing = TRUE)
  freq.names <- names(freq)
  scoremat <- cbind(freq, scoremat[c(freq.names),])
  scoremat
}

