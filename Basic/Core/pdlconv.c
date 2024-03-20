#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#define XCODE(code, datatype, ctype, ...) \
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
  PDL_Indx i, j; \
  int intype = a->datatype; \
  if (!a->vafftrans) \
    return pdl_make_error_simple(PDL_EUSERERROR, "pdl_" #name " without vafftrans"); \
  if (a->nvals && !a->data) \
    return pdl_make_error_simple(PDL_EUSERERROR, "pdl_" #name " non-empty with NULL data"); \
  PDL_GENERICSWITCH(PDL_TYPELIST_ALL, intype, X, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype)) \
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
    PDLDEBUG_f(printf("pdl_converttype to %d: ", targtype); pdl_dump(a));
    if (a->state & PDL_DONTTOUCHDATA)
      return pdl_make_error_simple(PDL_EUSERERROR, "Trying to converttype magical (mmaped?) pdl");
    if (!a->data)
      return pdl_make_error(PDL_EUSERERROR, "converttype called with NULL data on pdl %p", a);

    int intype = a->datatype;
    if (intype == targtype)
       return PDL_err;

    STRLEN nbytes = a->nvals * pdl_howbig(targtype); /* Size of converted data */
    STRLEN ncurr = a->nvals * pdl_howbig(intype);
    PDL_Value value;
    char diffsize = ncurr != nbytes,
      was_useheap = (ncurr > sizeof(value)),
      will_useheap = (nbytes > sizeof(value));

    void *data_from_void = a->data, *data_to_void = a->data;
    if (diffsize)
       data_to_void = will_useheap ? pdl_smalloc(nbytes) : &value;

#define THIS_ISBAD(from_badval_isnan, from_badval, from_val) \
  ((from_badval_isnan) \
    ? isnan((double)(from_val)) \
    : (from_val) == (from_badval))
#define X_OUTER(datatype_from, ctype_from, ppsym_from, ...) \
    PDL_Indx i = a->nvals; \
    ctype_from *data_from_typed = (ctype_from *) data_from_void; \
    ctype_from from_badval = pdl_get_pdl_badvalue(a).value.ppsym_from; \
    char from_badval_isnan = PDL_ISNAN_##ppsym_from(from_badval);
#define X_INNER(datatype_to, ctype_to, ppsym_to, shortctype_to, defbval_to, ...) \
    ctype_to *data_to_typed = (ctype_to *) data_to_void; \
    data_to_typed += i-1; data_from_typed += i-1; \
    if (a->state & PDL_BADVAL) { \
      ctype_to to_badval = defbval_to; \
      a->has_badvalue = 0; \
      while (i--) { \
        *data_to_typed-- = THIS_ISBAD(from_badval_isnan, from_badval, *data_from_typed) \
          ? to_badval : (ctype_to) *data_from_typed; \
        data_from_typed--; \
      } \
    } else \
      while (i--) \
        *data_to_typed-- = (ctype_to) *data_from_typed--;
    PDL_GENERICSWITCH2(PDL_TYPELIST_ALL, intype, X_OUTER, PDL_TYPELIST_ALL_, targtype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype))
#undef X_INNER
#undef X_OUTER
#undef THIS_ISBAD

    /* Store new data */
    if (diffsize) {
      if (!was_useheap && !will_useheap) {
        memmove(&a->value, data_to_void, nbytes);
      } else if (!will_useheap) {
        /* was heap, now not */
        memmove(a->data = &a->value, data_to_void, nbytes);
        SvREFCNT_dec((SV*)a->datasv);
        a->datasv = NULL;
      } else {
        /* now change to be heap */
        if (a->datasv == NULL)
          a->datasv = newSVpvn("", 0);
        (void)SvGROW((SV*)a->datasv, nbytes);
        SvCUR_set((SV*)a->datasv, nbytes);
        memmove(a->data = SvPV_nolen((SV*)a->datasv), data_to_void, nbytes);
      }
    }

    a->datatype = targtype;
    PDLDEBUG_f(printf("pdl_converttype after: "); pdl_dump(a));
    return PDL_err;
}
