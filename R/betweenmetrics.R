"betweenmetrics" <- function(clusterlist, dataFormat="clusterList") {

##list of between-method metrics
between <- c("jaccard", "variationinformation")
n <- length(clusterlist)

##initialize matrices for results
jaccardresult <- matrix(ncol = n, nrow = n)
variationresult <- matrix(ncol = n, nrow = n)
##initialize column and row names for results
methodnames1 <- vector()
methodnames2 <- vector()

for(i in 1:n){
          
	#compare each clustering to each other
		
	if(dataFormat == "clusterList"){
                clustering1 <- clusterlist[[i]]$clustering
	}
		
	if(dataFormat == "list"){
		clustering1 <- data.frame(clusterlist[[i]])
	}

	id1 <- names(clusterlist[i])
	methodnames1[i] <- id1
	
	for(j in 1:n){

		id2 <- names(clusterlist[j])
		methodnames2[j] <- id2

		if(i>j){
			if(dataFormat == "clusterList"){
				clustering2 <- clusterlist[[j]]$clustering
			}
			if(dataFormat == "list"){
				clustering2 <- data.frame(clusterlist[[j]])}
  
			#produce jaccard index between clustering i
			#and clustering j
			jacc <- jaccard(clustering1, clustering2)
			jaccardresult[i,j] <- jacc
  
			#produce variation of information between clustering
			#i and clustering j
			variationinformation <- 
				variationInformation(clustering1, clustering2)
			variationresult[i,j] <- variationinformation
			}
		}
	}

colnames(jaccardresult) <- methodnames1
rownames(jaccardresult) <- methodnames2

colnames(variationresult) <- methodnames1
rownames(variationresult) <- methodnames2

clusters <- list()


betweenresult <- list(jaccardresult=as.dist(jaccardresult), 
variationresult=as.dist(variationresult))

betweenresult
}