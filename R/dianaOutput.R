"dianaOutput" <-
function(matrix, params) {

require(cluster)
##if(params$method == "diana"){
clustMethod <- params$clustMethod
distfunc <- params$distfunc
k <- params$k
id <- params$id
weights <- params$weights
##}

##else{
##clustMethod <- "average"
##distfunc <- "euclidean"
##height <- 2
##}

##check to see if input is a matrix
##otherwise, stop
if(is.matrix(matrix)) {

##calculate appropriate distance matrix
distmatrix <- switch(distfunc, euclidean = dist(matrix), 
correlation = as.dist(1 - cor(t(matrix))), weightedeuclidean=distweight(matrix, weights=weights))

##do clustering
tree <- as.hclust(diana(x = distmatrix, diss = TRUE))

##if k is numeric
##cut tree at height that corresponds to cluster number
if(is.numeric(k)) {
tmp <- cutree(as.hclust(tree), k=k)}

##else, inspect tree to cut into clusters
else {tmp <- inspectTree(tree, id)}

##output data file with cluster info
#output <- as.data.frame(cbind(tmp, matrix))
                output <- as.data.frame(tmp)
                
##sort output file
#output <- output[order(output[,1]),]

##output file
output

}

else{
stop("Input is not a matrix.")
}
}

