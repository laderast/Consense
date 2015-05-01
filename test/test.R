test = matrix(c(1,1,1,2,3,4,1,1,1), ncol=3, byrow=TRUE)
test
weights = 1/c(1,2,3)

#note that weighted distance for each entry is multiplied 
#by the square of the difference
#that is, distance = sqrt(pow(feature.massDiff,2)*w_m + 
#pow(feature.netDiff,2)*w_n + pow(feature.driftTimeDiff,2)*w_d)
#so weight terms are multiplied, rather than divided
distweight(test[1:2,], weights=1/c(1,4,9))^2
distweight(test, weights=1/c(1,4,9))^2


#####
#test weighted distance in agglomOutput

params <- list()
weights <- c(rep(1,5),2,2,rep(1,10))
params[["weights"]] <- weights
params[["distfunc"]] <- "weightedeuclidean"
params[["clustMethod"]] <- "average"
params[["id"]] <- "UPGMAEUCW"
params[["k"]] <- 5

data(chocellcycle)
test <- agglomOutput(chocellcycle, params)

#####
#test euclidean distance
params <- list()
#weights <- c(rep(1,5),2,2,rep(1,10))
#params[["weights"]] <- weights
params[["distfunc"]] <- "euclidean"
params[["clustMethod"]] <- "average"
params[["id"]] <- "UPGMAEUC"
params[["k"]] <- 5

test <- cbind(test, agglomOutput(chocellcycle, params))

params <- list()
#weights <- c(rep(1,5),2,2,rep(1,10))
#params[["weights"]] <- weights
params[["distfunc"]] <- "correlation"
params[["clustMethod"]] <- "average"
params[["id"]] <- "UPGMACOR"
params[["k"]] <- 5

test <- cbind(test, agglomOutput(chocellcycle, params))

###test dianaOutput

params <- list()
weights <- c(rep(1,5),2,2,rep(1,10))
params[["weights"]] <- weights
params[["distfunc"]] <- "weightedeuclidean"
params[["clustMethod"]] <- "average"
params[["id"]] <- "UPGMAEUCW"
params[["k"]] <- 5

test <- dianaOutput(chocellcycle, params)

params[["distfunc"]] <- "euclidean"
test <- cbind(test, dianaOutput(chocellcycle, params))

params[["distfunc"]] <- "correlation"
test <- cbind(test, dianaOutput(chocellcycle, params))

####
####test method subsetting

#show all methods
getAllClusterMethods()

#specify weights
weights <- c(rep(1,5),2,2,rep(1,10))
data(chocellcycle)

methods <- c("UPGMAEUC",  "UPGMACOR",  "DIANAEUC",  "DIANACOR",  "UPGMAEUCW", "DIANAEUCW", "WARDCOR")
results <- autoCompareClustering(chocellcycle, kinput =5, weights=weights, autoMethods=methods)
ClusterReport(results)
