"getClusterInfo" <-
function(useGUI=FALSE) {

    upgma <- list()
    diana <- list()
    som <- list()
    kmeans <- list()
            
     #else, ask for input from command line
       cat("Choose your clustering methods:\n") 

       answer <- readline("Use upgma clustering? (y/n) ")
       if(substr(answer, 1, 1) == "n")
         {
           upgma$use <- FALSE
         }
       else {upgma$use <- TRUE
             answer <- readline("Use Euclidean Distance? (y/n) ")
             if(substr(answer, 1, 1) == "n")
               {upgma$euclidean <- FALSE}
             else{upgma$euclidean <- TRUE}

             answer <- readline("Use Correlation Distance? (y/n) ")
             if(substr(answer, 1, 1) == "n")
               {upgma$correlation <- FALSE}
             else{upgma$correlation <- TRUE}
           }
 
       answer <- readline("Use Diana clustering? (y/n)")
       if(substr(answer, 1, 1) == "n")
         {diana$use <- FALSE}
       else{diana$use <- TRUE
             answer <- readline("Use Euclidean Distance? (y/n) ")
             if(substr(answer, 1, 1) == "n")
               {diana$euclidean <- FALSE}
             else{diana$euclidean <- TRUE}

             answer <- readline("Use Correlation Distance? (y/n) ")
             if(substr(answer, 1, 1) == "n")
               {diana$correlation <- FALSE}
             else{diana$correlation <- TRUE}
          }

kans <- "c"
 
      answer <- readline("Use Self-Organized Maps? (y/n) ")
        if(substr(answer, 1, 1) == "n")
          {  som$use <- FALSE
          }
       else {som$use <- TRUE
             som$xdim <- as.numeric(readline("Enter X dimension of map: "))
             som$ydim <- as.numeric(readline("Enter Y dimension of map: "))
          }
 
       answer <- readline("Use K-means clustering? (y/n)")
         if(substr(answer, 1, 1) == "n")
          {kmeans$use <- FALSE 
          }
       else {kmeans$use <- TRUE
             kmeans$k <- 
as.numeric(readline("Enter number of clusters k: "))
             kmeans$iterations <- 
as.numeric(readline("Enter Maximum Iterations (Default 10000):"))
             if(is.na(kmeans$iterations))
               {kmeans$iterations <- 10000}
          }
       
     

    
     i <- 1
     methodlist <- list()
 
     if((upgma$use == TRUE) && (upgma$euclidean == TRUE)) {
id <- "UPGMAEUC"
func <- agglomOutput
method <- "upgma"
clustMethod <- "average"
distfunc <- "euclidean"
k <- kans
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, k=k,
clustMethod = clustMethod)
methodlist[[i]] <- params 
i <- i + 1
}

if((upgma$use == TRUE) && (upgma$correlation == TRUE)) {
id <- "UPGMACOR"
func <- agglomOutput
method <- "upgma"
k <- kans
clustMethod <- "average"                
distfunc <- "correlation"
params <- list(id = id, method=method, func=func, 
distfunc = distfunc, 
clustMethod = clustMethod)
methodlist[[i]] <- params 
i <- i + 1
}
                          
 if((diana$use == TRUE) && (diana$euclidean == TRUE)) {
id <- "DIANAEUC"
func <- dianaOutput
method <- "diana"
distfunc <- "euclidean"
params <- list(id = id, method=method, func=func, distfunc = distfunc)
methodlist[[i]] <- params 
i <- i + 1
}


 if((diana$use == TRUE) && (diana$correlation == TRUE)) {
id <- "DIANACOR"
func <- dianaOutput
method <- "diana"
distfunc <- "correlation"
params <-
                  list(id = id, method=method, func=func, distfunc = distfunc)
methodlist[[i]] <- params 
i <- i + 1
}


if(som$use == TRUE) {
id <- "SOM1"
func <- somOutput
method <- "som"
distfunc <- "euclidean"
xdim <- som$xdim
ydim <- som$ydim
params <-
                  list(id=id, method=method, func=func, xdim=xdim, ydim=ydim,
                  distfunc=distfunc)
methodlist[[i]] <- params
i <- i + 1
}

if(kmeans$use == TRUE) {
id <- "KMEANS1"
func <- kmeansOutput
method <- "kmeans"
distfunc <- "euclidean"
k <- kmeans$k
iterations <- kmeans$iterations
params <- list(id = id, method=method, func=func, k=k, 
iterations = iterations, distfunc=distfunc)
methodlist[[i]] <- params
i <- i + 1
}
        
methodlist
}

