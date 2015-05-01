"clusterList" <-
function(data, methodlist) {
##clusterList takes data and a methodlist and
##outputs a series of clusterings

   n <- length(methodlist)
   if(n > 0 && is.list(methodlist)){

clustlist <- list()
for(nam in names(methodlist))
{
  params <- as.list(methodlist[[nam]])  

  func <- function(data, params){}

  func <- methodlist[[nam]]$func

  ##do the clustering with proper output
  clustering <- func(data, methodlist[[nam]])

  ##
  clustlist[[nam]] <- list(clustering = clustering, params = params)
}

clustlist
      }

   else{stop("improper input to clusterlist")}

}

