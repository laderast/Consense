#  File src/library/stats/R/dist.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

distweight <- function(x, method="euclidean", diag=FALSE, upper=FALSE, weights=NA)
{
    ## account for possible spellings of euclid?an
    if(!is.na(pmatch(method, "euclidian")))
	method <- "euclidean"

    METHODS <- c("euclidean", "maximum",
		 "manhattan", "canberra", "binary", "minkowski")
    method <- pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid distance method")
    if(method == -1)
	stop("ambiguous distance method")

    if(length(weights) > ncol(x))
	stop("length of weight vector not equal to number of columns in input matrix")
	
    if(length(weights)==0 || is.na(weights))
	stop("must specify weights")

    N <- nrow(x <- as.matrix(x))
    d <- .C("R_distmod",
	    x = as.double(x),
	    nr= N,
	    nc= ncol(x),
	    d = double(N*(N - 1)/2),
	    diag  = as.integer(FALSE),
	    method= as.integer(method),
	    p = as.double(weights),
	    DUP = FALSE, NAOK=TRUE, PACKAGE="Consense")$d
    attr(d, "Size") <- N
    attr(d, "Labels") <- dimnames(x)[[1L]]
    attr(d, "Diag") <- diag
    attr(d, "Upper") <- upper
    attr(d, "method") <- METHODS[method]
    if(method == 6) attr(d, "p") <- p
    attr(d, "call") <- match.call()
    class(d) <- "dist"
    return(d)
}

