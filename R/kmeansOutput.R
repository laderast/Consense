"kmeansOutput" <-
function(matrix, params) {

if(params$method == "kmeans") {
k <- params$k
iterations <- params$iterations
}

else {stop("improper params")}

## checks input to see if it's a matrix
## otherwise, stop
if(is.matrix(matrix)) {

##do k-means clustering
tmp <- kmeans(matrix, k, iterations)

##produce output file
#output <- as.data.frame(cbind(tmp$cluster, matrix))
                output <- as.data.frame(tmp$cluster)
##sort output file
#output <- output[order(output[,1]),]

##output file
output
}

else {
stop("Input is not a matrix.")}

}

