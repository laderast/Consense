"variationInformation" <-
function(clustering1, clustering2) {
##function intersect returns number in 
##intersection between sets x and  y
intersect <- function(x, y) length(y[match(x, y, nomatch = 0)])

n <- nrow(clustering1)

if(is.data.frame(clustering1) && is.data.frame(clustering2))
{

##separate the data frame names by their individual clusters
#by1 <- by(clustering1, clustering1[1], 
#function(x) {row.names(x)})

#by2 <- by(clustering2, clustering2[1], 
#function(x) {row.names(x)})

prob1 <- countcluster(clustering1)/n
prob2 <- countcluster(clustering2)/n

#contingency <- contingencytable(by1, by2)
contingency <- table(clustering1[,1], clustering2[,1])

jointprob <- contingency/n

probproduct <- as.matrix(prob1) %*% t(prob2)

      ##return zero if probproduct = 0
      ##else, return log10(jointprob/probproduct)
logprob <- ifelse(contingency==0,0,log10(jointprob/probproduct))

      ##calculate mutual information
##sum over the product of the 
      ##jointprob + logprob matrices
mutual <- sum(jointprob * logprob)

##calculate entropy for each clustering
entropy1 <- -sum(prob1 * log10(prob1))
entropy2 <- -sum(prob2 * log10(prob2))

#finally, calculate variation of information
variation <- entropy1 + entropy2 - (2*mutual)

#return variation of information
variation

}

else  {stop("improper input to function")}
}

