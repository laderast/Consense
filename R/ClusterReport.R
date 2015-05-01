"ClusterReport" <-
function(clusterresults, write=FALSE, filebase){
withinmetrics <- clusterresults$report$within
jaccard <- clusterresults$report$between$jaccardresult
variation <- clusterresults$report$between$variationresult

print(withinmetrics)
cat("Displaying Between-method metrics in Plot Window\n")
plot(hclust(as.dist(variation)), main = "Variation of Information")
not <- readline("Press Any Key to Continue:")
plot(hclust(as.dist(1-jaccard)), main = "Jaccard Index")


        #if(!exists(filebase))
        #  {
        #   ask for base file name
  #  filebase <- 
  #as.character(readline("Enter base file name for report: "))
        #  }

if(write==TRUE){        
        #concatenate file names
variationfile <- 
paste(filebase, "variation.jpg")
        variationtxt <- paste(filebase, "variation.txt")
jaccardfile <- 
paste(filebase, "jaccard.jpg")
        jaccardtxt <- paste(filebase, "jaccard.txt")
withinfile <- 
paste(filebase, "within.txt")
clusterfile <- 
paste(filebase, "clusters.txt")

write.table(as.matrix(variation), variationtxt, sep = "\t")
jpeg(file = variationfile)
plot(hclust(as.dist(variation)), main = "Variation of Information")
dev.off()
        
        write.table(as.matrix(jaccard), jaccardtxt, sep = "\t")

jpeg(file = jaccardfile)
plot(hclust(as.dist(1-jaccard)), main = "Jaccard Index")
dev.off()


write.table(withinmetrics, withinfile, sep = "\t")

numalgorithms <- length(clusterresults$clusters)
numgenes <- length(clusterresults$clusters[[1]])

clusters <- matrix(nrow = numgenes, 
ncol = numalgorithms)

clusters.names <- vector()

for(i in 1:numalgorithms) { 
clusters[,i] <- 
as.matrix(clusterresults$clusters[[i]])
  clusters.names[i] <- names(clusterresults[i])
}

rownames(clusters) <- 
row.names(as.matrix(clusterresults$cluster[[1]]))
colnames(clusters) <- 
clusters.names

write.table(clusters, clusterfile, sep = "\t")

WD <- getwd()

print(paste("Within Metrics have been saved as ", WD, "/", withinfile, sep=""))
print(paste("Clusters have been saved as ", WD, "/", clusterfile, sep=""))
print(paste("Variation Graph has been saved as ", WD, "/", variationfile, sep =""))
print(paste("Variation Matrix has been saved as ", WD, "/", variationtxt, sep=""))
print(paste("Jaccard Graph has been saved as ", WD, "/", jaccardfile, sep=""))
print(paste("Jaccard Matrix has been saved as ", WD, "/" , jaccardtxt, sep=""))


   }

}

