"homogeneity" <-
function(clustering, centers, distfunc="euclidean") {

##initialize count
tally <- c(0)
n <- nrow(clustering)
      
euclidean <- function(clustering, centers) {
for(i in 1:n) {
  #print(i)
                clusternum <- as.numeric(clustering[i,1])

#print(clusternum)

val <- sqrt(sum((clustering[i, -1] - centers[clusternum,])^2)) 
#print(val)

tally <- tally + val


}


#print(tally)
tally

}

correlation <- function (clustering, centers) {

for(i in 1:n) {


tally <- tally + (1-cor(clustering[i, c(2:ncol(clustering))], 
centers[c(clusternum),])) 
}

tally

}

tally <- switch(distfunc, euclidean=euclidean(clustering, centers),
correlation = correlation(clustering, centers))

hom <- tally / n

hom

}

