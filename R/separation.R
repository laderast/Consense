"separation" <-
function(clustering, centers) {
  if(is.matrix(centers))
{
#countcluster takes a clustering and produces
#a vector of counts
countcluster <- function(output) {
countclust <- as.vector(table(output[,1]))
countclust}

#initialize matrix for calculating separation
#first, need to calculate distance 
#between cluster centers
sepmatrix <- dist(centers)

#grab the factor names of the cluster assignments
levs <- levels(as.factor(clustering[,1]))
L <- length(levs)
#first corresponds to the first 1:L-1 factors
        first <- levs[1:(L-1)]
#second corresponds to the 2:L cluster numbers
second <- levs[2:L]

##if number of clusters is >= 2
##calculate separation
if(L >= 2) {

countclust <- countcluster(clustering)
names(countclust) <- levs
#columns of clusters (NCi) consists of cluster counts
#from cluster 1 to L-1
colclust <- as.matrix(countclust[c(first)])
#rowclust (NCj) consists of cluster counts
#from cluster 2 to L
rowclust <- t(countclust[c(second)])

#Produce matrix of Nci*Ncj terms
#by outer multiplication
prod <- colclust %*% rowclust

suppressWarnings(sepmatrix <- sepmatrix * prod)

#print(sepmatrix)
#print(prod)

separation <- sum(sepmatrix)/sum(prod)
}

##if number of clusters is 1, return 0
else {separation <- 0}

separation
      }
  else{
    stop("input is not a matrix")}

}

