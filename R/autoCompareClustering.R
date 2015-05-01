"autoCompareClustering" <-
function(data, kinput, autoMethods="all", weights=NA) {

       results <- CompareClustering(data, verbose=FALSE, auto=TRUE, autoMethods=autoMethods, kinput=kinput, weights=weights)
 results
}

