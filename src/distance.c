/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2001  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
 *  Copyright (C) 2002, 2004  The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include <R.h>
#include <Rmath.h>
#include <float.h>
#include "mva.h"
#include "stats.h"

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#ifdef R_160_and_older
#define both_non_NA both_FINITE
#else
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
#endif

static double R_weightedeuclidean(double *x, int nr, int nc, int i1, int i2, double *p)
{
    double dev, dist;
    int count, j, weightind;

    count= 0;
    dist = 0;
    weightind=0;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    dev = (x[i1] - x[i2]);
	    if(!ISNAN(dev)) {
		dist += ((dev * dev)*p[weightind]);
		count++;
	    }
	    
	}
	weightind++;
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return sqrt(dist);
}


enum { EUCLIDEAN=1, MAXIMUM, MANHATTAN, CANBERRA, BINARY, MINKOWSKI };
/* == 1,2,..., defined by order in the R function dist */

void R_distmod(double *x, int *nr, int *nc, double *d, int *diag,
		int *method, double *p)
{
    int dc, i, j, ij;
    /*double (*distfun)(double*, int, int, int, int) = NULL;	
	
    distfun = R_weightedeuclidean; */
    dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */
    ij = 0;
    for(j = 0 ; j <= *nr ; j++)
	for(i = j+dc ; i < *nr ; i++)
	    d[ij++] = R_weightedeuclidean(x, *nr, *nc, i, j, p) ;
};


