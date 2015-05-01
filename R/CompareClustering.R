"CompareClustering" <-
function(data, verbose=TRUE, auto=FALSE, autoMethods = "all", kinput, weights=NA) {

        #load necessary libraries
library(Biobase)
#library(tkWidgets)
library(cluster)

      #generate list of methods and parameters

if(auto == FALSE)
        {methodlist <- getClusterInfo()}
else
  {	
  if(!is.na(weights[1])){
  methodlist <- methodlistAuto(kinput, weights=weights)}
  
  else{ methodlist <- methodlistAuto(kinput) }

  ##subset method list to only those methods picked
  if(autoMethods != "all" && is.vector(autoMethods)){
		methodlist <- methodlist[autoMethods]
	}
  }

        #Generate Clusterings
        if(verbose == TRUE) {
          print("Generating Clustering Results")
        }
clusterlist <- clusterList(data, methodlist)

        #generate within-method metrics
        if(verbose == TRUE) {
          print("Generating Within Method Metrics")
        }
        
results <- withinmetrics(clusterlist, data)
        within <- results$withinresult
        silvalues <- results$silvalues

        #generate between-method metrics
        if(verbose == TRUE) {
          print("Generating Between-method Metrics")
        }
        
between <- betweenmetrics(clusterlist)

        #parse clustering format
        clusters <- list()
params <- list()
        methodnames <- vector()
for(i in 1:length(clusterlist)){
clusters[[i]] <- as.matrix(as.factor(clusterlist[[i]]$clustering[,1]))
row.names(clusters[[i]]) <- row.names(data)
params[[i]] <- clusterlist[[i]]$params
                methodnames[i] <- params[[i]]$id
}

        names(clusters) <- methodnames
        names(silvalues) <- methodnames
        names(params) <- methodnames

        #return results
results <- list(report=list(within = within, between = between), 
clusters=clusters, params=params, silvalues = silvalues)
results

}

RunMetrics <- function(clusterlist, data, verbose=FALSE) {

	if(verbose==TRUE){
	   print("Generating within-method metrics")
	}

	results <- withinmetrics(clusterlist, data, dataFormat="list")
        within <- results$withinresult
        silvalues <- results$silvalues

        #generate between-method metrics
        if(verbose == TRUE) {
          print("Generating Between-method Metrics")
        }
        
	between <- betweenmetrics(clusterlist, dataFormat="list")

        #parse clustering format
        clusters <- list()
	params <- list()
        methodnames <- vector()
	for(i in 1:length(clusterlist)){
		clusters[[i]] <- as.matrix(as.factor(clusterlist[[i]]))
		row.names(clusters[[i]]) <- row.names(data)
		#params[[i]] <- clusterlist[[i]]$params
                methodnames[i] <- names(params[i])
	}

        names(clusters) <- methodnames
        names(silvalues) <- methodnames
        names(params) <- methodnames

        #return results
	results <- list(report=list(within = within, between = between), 
	clusters=clusters, params=params, silvalues = silvalues)
	results

}