"somOutput" <-
function(matrix, params) {

require(som)
if(params$method == "som"){
xdim <- params$xdim
ydim <- params$ydim}

else {stop("improper input to method")}

##somvector takes input from som$visual 
##(x,y coords of cluster) and converts
##that to a cluster number
somvector <- function(somnum, xdim, ydim) {
n <- nrow(somnum)
##validate input 
##if not validated, stop
if(ncol(somnum) == 2) 
{
##initialize cluster number vector
somvector <- vector(length=(1:n))

for(i in 1:nrow(somnum))
{ somvector[i] <-somnum[i,1] + somnum[i,2] +
    (somnum[i,1] * (ydim-1)) + 1
}

##output vector
somvector
}

else { stop("Improper Input to somInput.")}

}

#main body of function
if(is.matrix(matrix))
{
##perform som
som <- som(matrix, xdim, ydim)
##convert coords to cluster numbers
somvec <- somvector(som$visual[,1:2], xdim, ydim)
##produce output file

somvec <- as.matrix(somvec)
#output <- as.data.frame(cbind(somvec, matrix))
        output <- as.data.frame(somvec)
        
#output <- output[order(output[,1]),]

output
}

else {
stop("Input is not a matrix")}
}

