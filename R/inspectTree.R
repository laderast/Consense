"inspectTree" <-
function(tree, id)
{
##show dendrogram
plot(tree, main=id)
##allow user to identify clusters

#print instructions for each dendrogram
cat(id, ":\n")
cat("Cut tree into clusters by carefully clicking\n")
cat("Branch points.  Right-click STOP when done.\n")

#print(length(tree$order))

#identify clusters 
clusts <- identify(tree, MAXCLUSTER=length(tree$order))
clustmat <- matrix(ncol=2)

#parse clusts object
for(i in 1:length(clusts))
{

minimat <- as.matrix(clusts[[i]])
vec <- vector(length=nrow(minimat)) + i
assign <- cbind(minimat, as.matrix(vec))
clustmat <- rbind(clustmat, assign)
}

##remove first row of clustermatrix
clustmat <- clustmat[-1,]
##order by gene
clustmat <- clustmat[order(clustmat[,1]),]
##return ordered vector of cluster assignments
clustmat <- as.matrix(clustmat[,2])
clustmat
}

