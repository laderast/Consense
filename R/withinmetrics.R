"withinmetrics" <-
function(clusterlist, data, dataFormat="clusterList") {

#list of metrics here
withinnames <- c("n-clusters", "homogeneity", "separation", "silhouette")
row <- length(clusterlist)
col <- length(withinnames)
        silvalues <- list()

        #initialize within-method matrix
withinresult <- matrix(ncol = col, nrow = row)
colnames(withinresult) <- withinnames
methodnames <- vector()

#calculate correlation distance matrix
#needed for silhouette index calculation
#data <- clusterlist[[1]]$clustering
#data <- data[,2:ncol(data)]
cordist <- as.dist(1 - cor(t(data)))

i<-1

for(nam in names(clusterlist)){
if(dataFormat=="clusterList"){
clustering <- data.frame(clusterlist[[nam]]$clustering, data)
}
if(dataFormat=="list"){
clustering <- data.frame(as.numeric(clusterlist[[nam]]), data)
#print(clustering[1:5,])
}

#grab clustering id
id <- names(clusterlist[nam])
n <- length(levels(as.factor(clustering[,1])))
#calcuate metrics
#calculate silhouette value 
sil <- silhouette(clustering[,1], cordist)
    rownames(sil) <- rownames(clustering)
                silvalues[[nam]] <- sil
                
#return average silhouette value
summ <- summary(sil)

avgsil <- as.numeric(summ$avg.width)
#return NA if there is no avgsil value
if(length(avgsil) == 0)
avgsil <- NA

#sort clustering by assignment
clustering <- clustering[order(clustering[,1]),]
#calculate cluster centers
centers <- clusterCenters(clustering)

#grab number of clusters
#calculate separation
sep <- separation(clustering, centers)
hom <- homogeneity(clustering, centers)

if(!is.numeric(sep) || length(sep)==0)
sep <- NA

if(!is.numeric(hom)||length(hom)==0)
hom <- NA

#save metrics to withinmetric table

#print(hom)
#print(sep)

withinresult[i,] <- c(n, hom, sep, avgsil)
methodnames[i] <- id 

i <- i +1
}

rownames(withinresult) <- methodnames
results <- list(withinresult= withinresult, silvalues=silvalues)
        results
}

