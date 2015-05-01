"clusterCenters" <-
function(x) {
##function clusterCenters takes as input data.frame
##output from various output functions (kmeansOutput, etc)

##grab row length of matrix
n <- ncol(x)
#max <- max(x[1])
rn <- levels(as.factor(x[,1]))
print(rn)
max <- length(rn)

if(is.data.frame(x)) {

##calculate vector centers of each cluster
agg <- aggregate(x[,2:n], list(cluster=x[,1]), mean)
        agg.frame <- as.data.frame(agg)
        rn <- as.character(agg.frame[,1])
        centers <- as.matrix(agg.frame[,2:n])
        rownames(centers) <- rn
        
##initialize matrix for centers
##centers <- matrix(0, max, n-1)
        
#p <- ncol(agg)

##transfer cluster info to matrix
#for (i in as.numeric(rn)) 
  #   {centers[as.numeric(levels(agg$cluster))[i],] <- 
#as.matrix(agg[i,2:ncol(agg)])
#  } 
##grabs vector info out of list
        
##return matrix of cluster centers
centers
}

else { stop("input is not a data frame")}

}

