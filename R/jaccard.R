"jaccard" <-
function(clustering1, clustering2)
{
if(is.data.frame(clustering1) && is.data.frame(clustering2)
&& (nrow(clustering1) == nrow(clustering2)))
{

##separate the data frame names by their individual clusters
by1 <- by(clustering1, clustering1[1], 
function(x) {row.names(x)})

by2 <- by(clustering2, clustering2[1], 
function(x) {row.names(x)})

#contingency <- contingencytable(by1, by2)

contingency <- table(clustering1[,1], clustering2[,1])

Z <- sum(contingency^2)
n <- nrow(clustering1)

sumsquarerow <- sum(colSums(contingency)^2)
sumsquarecol <- sum(rowSums(contingency)^2)

jaccard <- (Z - n) / ( sumsquarerow + sumsquarecol - Z - n)

jaccard

}

else  {stop("Improper input to function")}

}

