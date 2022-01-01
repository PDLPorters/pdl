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

pdl_error pdl_converttype( pdl* a, int targtype ) {
    pdl_error PDL_err = {0, NULL, 0};
    void* b;     /* Scratch data ptr */
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

#define THIS_ISBAD(from_badval_isnan, from_badval, from_val) \
  ((from_badval_isnan) \
    ? isnan((double)(from_val)) \
    : (from_val) == (from_badval))
#define X_OUTER(datatype_from, ctype_from, ppsym_from, shortctype_from, defbval_from) \
    PDL_Indx i = a->nvals; \
    ctype_from *bb = (ctype_from *) b; \
    ctype_from from_badval = pdl_get_pdl_badvalue(a).value.ppsym_from; \
    char from_badval_isnan = PDL_ISNAN_##ppsym_from(from_badval); \
    PDL_GENERICSWITCH2(PDL_TYPELIST2_ALL_, targtype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", targtype))
#define X_INNER(datatype_to, ctype_to, ppsym_to, shortctype_to, defbval_to) \
    ctype_to *aa = (ctype_to *) a->data; \
    aa += i-1; bb += i-1; \
    if (a->state & PDL_BADVAL) { \
      ctype_to to_badval = defbval_to; \
      a->has_badvalue = 0; \
      while (i--) { \
        *aa-- = THIS_ISBAD(from_badval_isnan, from_badval, *bb) \
          ? to_badval : (ctype_to) *bb; \
        bb--; \
      } \
    } else \
      while (i--) \
        *aa-- = (ctype_to) *bb--;
    PDL_GENERICSWITCH(PDL_TYPELIST2_ALL, intype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype))
#undef X_INNER
#undef X_OUTER
#undef THIS_ISBAD

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
