"methodlistAuto" <-
function(kinput, heights, weights=NA){
     i <- 1
     methodlist <- list()
#take kinput as number of clusters 
            k <- kinput


id <- "UPGMAEUC"
func <- agglomOutput
method <- "upgma"
clustMethod <- "average"
distfunc <- "euclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k)
methodlist[[id]] <- params 

id <- "UPGMACOR"
func <- agglomOutput
method <- "upgma"
clustMethod <- "average"                
distfunc <- "correlation"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k = k)
methodlist[[id]] <- params 
                   
 id <- "DIANAEUC"
func <- dianaOutput
method <- "diana"
distfunc <- "euclidean"
params <- list(id = id, method=method, func=func, distfunc = distfunc, k = k)
methodlist[[id]] <- params 

id <- "DIANACOR"
func <- dianaOutput
method <- "diana"
distfunc <- "correlation"
params <-
                  list(id = id, method=method, func=func, distfunc = distfunc, k=k)
methodlist[[id]] <- params 

id <- "WARDEUC"
func <- agglomOutput
method <- "ward"
clustMethod <- "ward"
distfunc <- "euclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k)
methodlist[[id]] <- params 

id <- "WARDCOR"
func <- agglomOutput
method <- "ward"
clustMethod <- "ward"
distfunc <- "correlation"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k)
methodlist[[id]] <- params 

id <- "COMPLETEEUC"
func <- agglomOutput
method <- "complete"
clustMethod <- "complete"
distfunc <- "euclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k)
methodlist[[id]] <- params 


id <- "COMPLETECOR"
func <- agglomOutput
method <- "ward"
clustMethod <- "ward"
distfunc <- "correlation"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k)
methodlist[[id]] <- params 


if(!is.na(weights[1]) && length(weights)>0){
id <- "UPGMAEUCW"
func <- agglomOutput
method <- "upgma"
clustMethod <- "average"
distfunc <- "weightedeuclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k, weights=weights)
methodlist[[id]] <- params 

id <- "DIANAEUCW"
func <- dianaOutput
method <- "diana"
distfunc <- "weightedeuclidean"
params <- list(id = id, method=method, func=func, 
	distfunc = distfunc, k = k, weights=weights)
methodlist[[id]] <- params 

id <- "WARDEUCW"
func <- agglomOutput
method <- "ward"
clustMethod <- "ward"
distfunc <- "weightedeuclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k, weights=weights)
methodlist[[id]] <- params 

id <- "COMPLETEEUCW"
func <- agglomOutput
method <- "complete"
clustMethod <- "complete"
distfunc <- "weightedeuclidean"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod, k=k, weights=weights)
methodlist[[id]] <- params 

}



id <- "SOM"
func <- somOutput
method <- "som"
distfunc <- "euclidean"
xdim <- kinput
ydim <- 1
params <-
                  list(id=id, method=method, func=func, xdim=xdim, ydim=ydim,
                  distfunc=distfunc)
methodlist[[id]] <- params

id <- "KMEANS"
func <- kmeansOutput
method <- "kmeans"
distfunc <- "euclidean"
iterations <- 10000
params <- list(id = id, method=method, func=func, k=k, 
iterations = iterations, distfunc=distfunc)
methodlist[[id]] <- params



methodlist
}



getAllClusterMethods <- function() {
	methods <- names(methodlistAuto(kinput=1, weights=c(1,1,1)))
	methods
	}