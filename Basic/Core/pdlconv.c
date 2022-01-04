#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define XCODE(code, datatype, ctype, ppsym, shortctype, defbval) \
    ctype *ap = (ctype *) a->data; \
    ctype *pp = (ctype *) a->vafftrans->from->data; \
    pp += a->vafftrans->offs; \
    for(i=0; i<a->nvals; i++) { \
        code; \
        for(j=0; j<a->ndims; j++) { \
            pp += a->vafftrans->incs[j]; \
            if((j < a->ndims - 1 && \
                (i+1) % a->dimincs[j+1]) || \
               j == a->ndims - 1) \
                break; \
            pp -= a->vafftrans->incs[j] * \
                a->dims[j]; \
        } \
        ap ++; \
    }

#define VAFF_IO(name, X) \
pdl_error pdl_ ## name(pdl *a) { \
	pdl_error PDL_err = {0, NULL, 0}; \
	PDL_Indx i; \
	int j; \
	int intype = a->datatype; \
	if(!PDL_VAFFOK(a)) { \
		return pdl_make_error_simple(PDL_EUSERERROR, "pdl_" #name " without vaffine"); \
	} \
	PDL_ENSURE_ALLOCATED(a); \
    PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, intype, X, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype)) \
    return PDL_err; \
}

#define X(...) XCODE(*ap = *pp, __VA_ARGS__)
VAFF_IO(readdata_vaffine, X)
#undef X
#define X(...) XCODE(*pp = *ap, __VA_ARGS__)
VAFF_IO(writebackdata_vaffine, X)
#undef X
#undef XCODE

/* Various conversion utilities for pdl data types */

/* Change the type of all the data in a pdl struct, either changing the
   original perl structure or making a temporary copy  */

/* 
 * it seems this does not have to be aware of bad values
 * (at least in the current scheme)
 */

pdl_error pdl_converttype( pdl* a, int targtype ) {
    pdl_error PDL_err = {0, NULL, 0};
    void* b;     /* Scratch data ptr */
    PDL_Indx   i;
    PDLDEBUG_f(printf("pdl_converttype %p, %d, %d\n", (void*)a, a->datatype,
	targtype));
    if(a->state & PDL_DONTTOUCHDATA)
      return pdl_make_error_simple(PDL_EUSERERROR, "Trying to converttype magical (mmaped?) pdl");

    int intype = a->datatype;
    if (intype == targtype)
       return PDL_err;

    int diffsize = pdl_howbig(targtype) != pdl_howbig(a->datatype);

    STRLEN nbytes = a->nvals * pdl_howbig(targtype); /* Size of converted data */

    if (diffsize) {
       b = a->data;                      /* pointer to old data */
       a->data     = pdl_smalloc(nbytes); /* Space for changed data */
    }
    else{
       b = a->data; /* In place */
    }

#define X_OUTER(datatype_from, ctype_from, ppsym_from, shortctype_from, defbval_from) \
    ctype_from *bb = (ctype_from *) b; \
    i = a->nvals; \
    PDL_GENERICSWITCH2(PDL_TYPELIST2_ALL_, targtype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", targtype))
#define X_INNER(datatype_to, ctype_to, ppsym_to, shortctype_to, defbval_to) \
    ctype_to *aa = (ctype_to *) a->data; \
    aa += i-1; bb += i-1; \
    while (i--) \
      *aa-- = (ctype_to) *bb--;
    PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, intype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype))
#undef X_INNER
#undef X_OUTER

    /* Store new data */
    if (diffsize) {
       STRLEN n_a;
       SV *bar = a->datasv;
       sv_setpvn( bar, (char*) a->data, nbytes );
       a->data = (void*) SvPV(bar, n_a);
    }

    a->datatype = targtype;
    return PDL_err;
}
