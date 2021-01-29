#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(frun_cemaneige)(int *, double *, double *, double *, double *, int *, double *, int *, double *, int *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr1a)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr2m)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr4h)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr5h)(int *, double *, double *, int *, double *, int *, double *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr4j)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr5j)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_gr6j)(int *, double *, double *, int *, double *, int *, double *, int *, int *, double *, double *);
extern void F77_NAME(frun_pe_oudin)(int *, double *, double *, double *, double *);

static const R_FortranMethodDef FortranEntries[] = {
    {"frun_cemaneige", (DL_FUNC) &F77_NAME(frun_cemaneige), 14},
    {"frun_gr1a",      (DL_FUNC) &F77_NAME(frun_gr1a),      11},
    {"frun_gr2m",      (DL_FUNC) &F77_NAME(frun_gr2m),      11},
    {"frun_gr4h",      (DL_FUNC) &F77_NAME(frun_gr4h),      11},
    {"frun_gr5h",      (DL_FUNC) &F77_NAME(frun_gr5h),      12},
    {"frun_gr4j",      (DL_FUNC) &F77_NAME(frun_gr4j),      11},
    {"frun_gr5j",      (DL_FUNC) &F77_NAME(frun_gr5j),      11},
    {"frun_gr6j",      (DL_FUNC) &F77_NAME(frun_gr6j),      11},
    {"frun_pe_oudin",  (DL_FUNC) &F77_NAME(frun_pe_oudin),   5},
    {NULL, NULL, 0}
};

void R_init_airGR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
