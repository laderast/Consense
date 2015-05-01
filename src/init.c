
#include <R.h>
#include <Rinternals.h>

#include "mva.h"
#include "stats.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}
#define FDEF(name)  {#name, (DL_FUNC) &F77_SUB(name), sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static const R_CMethodDef CEntries[]  = {
    {"R_distmod", (DL_FUNC) &R_distmod, 7},
        {NULL, NULL, 0}
};


void R_init_Consense(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}