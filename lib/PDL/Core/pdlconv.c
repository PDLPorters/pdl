#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

extern struct Core PDL;

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
  pdl_datatypes intype = a->datatype; \
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

pdl_error pdl_converttype( pdl* a, pdl_datatypes targtype ) {
    pdl_error PDL_err = {0, NULL, 0};
    PDLDEBUG_f(printf("pdl_converttype to %d: ", targtype); pdl_dump(a));
    if (a->state & PDL_DONTTOUCHDATA)
      return pdl_make_error_simple(PDL_EUSERERROR, "Trying to converttype magical (mmaped?) pdl");
    if (!a->data)
      return pdl_make_error(PDL_EUSERERROR, "converttype called with NULL data on pdl %p", a);

    PDL_RETERROR(PDL_err, pdl_make_physical(a));
    pdl_datatypes intype = a->datatype;
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
    ctype_from from_badval = a->has_badvalue ? a->badvalue.value.ppsym_from : PDL.bvals.ppsym_from; \
    char from_badval_isnan = PDL_ISNAN_##ppsym_from(from_badval);
#define X_INNER(datatype_to, ctype_to, ppsym_to, shortctype_to, defbval_to, ...) \
    ctype_to *data_to_typed = (ctype_to *) data_to_void; \
    data_to_typed += i-1; data_from_typed += i-1; \
    if (a->state & PDL_BADVAL) { \
      ctype_to to_badval = defbval_to; \
      a->has_badvalue = 0; \
      while (i--) { \
        *data_to_typed-- = \
          THIS_ISBAD(from_badval_isnan, from_badval, *data_from_typed) ? to_badval : \
          PDL_GENTYPE_IS_UNSIGNED_##ppsym_to ? (ctype_to)(intmax_t) *data_from_typed : \
          (ctype_to) *data_from_typed; \
        data_from_typed--; \
      } \
    } else \
      while (i--) \
        *data_to_typed-- = \
          PDL_GENTYPE_IS_UNSIGNED_##ppsym_to ? (ctype_to)(intmax_t) *data_from_typed-- : \
          (ctype_to) *data_from_typed--;
    PDL_GENERICSWITCH2(
      PDL_TYPELIST_ALL, intype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", intype),
      PDL_TYPELIST_ALL_, targtype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", targtype))
#undef X_INNER
#undef X_OUTER

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
    if (a->has_badvalue && a->badvalue.type != a->datatype)
      return pdl_make_error(PDL_EUSERERROR, "Badvalue has type=%d != pdltype=%d", a->badvalue.type, a->datatype);
    return PDL_err;
}

/* generated from:
pp_def(
        'converttypei',
        GlobalNew => 'converttypei_new',
        OtherPars => 'pdl_datatypes totype;',
        Identity => 1,
# Forced types
        FTypes => {CHILD => '$COMP(totype)'},
        Doc => 'internal',
);
*/

/* also in pdlaffine.c */
#define PDL_ALL_GENTYPES { PDL_SB, PDL_B, PDL_S, PDL_US, PDL_L, PDL_UL, PDL_IND, PDL_ULL, PDL_LL, PDL_F, PDL_D, PDL_LD, PDL_CF, PDL_CD, PDL_CLD, -1 }

typedef struct pdl_params_converttypei {
  pdl_datatypes  totype;
} pdl_params_converttypei;

pdl_error pdl_converttypei_redodims(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl *__it = trans->pdls[1];
  pdl_hdr_childcopy(trans);
  PDL_Indx i;
  PDL_RETERROR(PDL_err, pdl_reallocdims(__it, trans->pdls[0]->ndims));
  for (i=0; i<trans->pdls[1]->ndims; i++)
    trans->pdls[1]->dims[i] = trans->pdls[0]->dims[i];
  PDL_RETERROR(PDL_err, pdl_setdims_careful(__it));
  pdl_reallocbroadcastids(trans->pdls[1], trans->pdls[0]->nbroadcastids);
  for (i=0; i<trans->pdls[0]->nbroadcastids; i++)
    trans->pdls[1]->broadcastids[i] = trans->pdls[0]->broadcastids[i];
  trans->dims_redone = 1;
  return PDL_err;
}

#define COPYCONVERT(from, to, ppsym_to, ctype_to) \
  PDL_Indx i, nvals = trans->pdls[1]->nvals; \
  if (trans->bvalflag) \
    for (i=0; i<nvals; i++) \
      to ## _datap[i] = \
        THIS_ISBAD(from ## _badval_isnan, from ## _badval, from ## _datap[i]) ? to ## _badval : \
        PDL_GENTYPE_IS_UNSIGNED_##ppsym_to ? (ctype_to)(intmax_t) from ## _datap[i] : \
        from ## _datap[i]; \
  else \
    for (i=0; i<nvals; i++) to ## _datap[i] = \
      PDL_GENTYPE_IS_UNSIGNED_##ppsym_to ? (ctype_to)(intmax_t) from ## _datap[i] : \
      from ## _datap[i];

pdl_error pdl_converttypei_readdata(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_converttypei *params = trans->params;
  pdl_datatypes fromtype = trans->__datatype, totype = params->totype;
  extern struct Core PDL;
  struct Core *PDLptr = &PDL;
#define PDL PDLptr /* so PDL_DECLARE_PARAMETER_BADVAL can get bvals */
  PDLDEBUG_f(printf("pdl_converttypei_readdata %s=%p to type=%d from parent: ", trans->vtable->name, trans, totype); pdl_dump(trans->pdls[0]));
#define FROMpdl_indx 0
#define TOpdl_indx 1
#define X_OUTER(datatype_from, ctype_from, ppsym_from, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_from, FROMpdl, (trans->pdls[FROMpdl_indx]), 1, ppsym_from)
#define X_INNER(datatype_to, ctype_to, ppsym_to, ...) \
  PDL_DECLARE_PARAMETER_BADVAL(ctype_to, TOpdl, (trans->pdls[TOpdl_indx]), 1, ppsym_to) \
  COPYCONVERT(FROMpdl, TOpdl, ppsym_to, ctype_to)
  PDL_GENERICSWITCH2(
    PDL_TYPELIST_ALL, fromtype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", fromtype),
    PDL_TYPELIST_ALL_, totype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", totype))
  pdl *TOpdl = trans->pdls[TOpdl_indx];
#undef FROMpdl_indx
#undef TOpdl_indx
#undef PDL
  if (TOpdl->has_badvalue && TOpdl->badvalue.type != TOpdl->datatype)
    return pdl_make_error(PDL_EUSERERROR, "Badvalue has type=%d != pdltype=%d", TOpdl->badvalue.type, TOpdl->datatype);
  return PDL_err;
}

pdl_error pdl_converttypei_writebackdata(pdl_trans *trans) {
  pdl_error PDL_err = {0, NULL, 0};
  pdl_params_converttypei *params = trans->params;
  pdl_datatypes fromtype = params->totype, totype = trans->__datatype;
  extern struct Core PDL;
  struct Core *PDLptr = &PDL;
#define PDL PDLptr /* so PDL_DECLARE_PARAMETER_BADVAL can get bvals */
  PDLDEBUG_f(printf("pdl_converttypei_writebackdata %s=%p from child to type=%d: ", trans->vtable->name, trans, totype); pdl_dump(trans->pdls[1]));
#define FROMpdl_indx 1
#define TOpdl_indx 0
  PDL_GENERICSWITCH2(
    PDL_TYPELIST_ALL, fromtype, X_OUTER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", fromtype),
    PDL_TYPELIST_ALL_, totype, X_INNER, return pdl_make_error(PDL_EUSERERROR, "Not a known data type code=%d", totype))
#undef X_INNER
#undef X_OUTER
  pdl *TOpdl = trans->pdls[TOpdl_indx];
#undef FROMpdl_indx
#undef TOpdl_indx
#undef PDL
  if (TOpdl->has_badvalue && TOpdl->badvalue.type != TOpdl->datatype)
    return pdl_make_error(PDL_EUSERERROR, "Badvalue has type=%d != pdltype=%d", TOpdl->badvalue.type, TOpdl->datatype);
  return PDL_err;
}

static pdl_datatypes pdl_converttypei_vtable_gentypes[] = PDL_ALL_GENTYPES;
static char pdl_converttypei_vtable_flags[] = { 0, 0 }; /*CORE21*/
static PDL_Indx pdl_converttypei_vtable_realdims[] = { 0, 0 };
static char *pdl_converttypei_vtable_parnames[] = { "PARENT","CHILD" };
static short pdl_converttypei_vtable_parflags[] = {
  PDL_PARAM_ISPHYS,
  PDL_PARAM_ISCREAT|PDL_PARAM_ISCREATEALWAYS|PDL_PARAM_ISIGNORE|PDL_PARAM_ISOUT|PDL_PARAM_ISPHYS|PDL_PARAM_ISWRITE
};
static pdl_datatypes pdl_converttypei_vtable_partypes[] = { -1, -1 };
static PDL_Indx pdl_converttypei_vtable_realdims_starts[] = { 0, 0 };
static PDL_Indx pdl_converttypei_vtable_realdims_ind_ids[] = { 0 };
static char *pdl_converttypei_vtable_indnames[] = { "" };
pdl_transvtable pdl_converttypei_vtable = {
  PDL_TRANS_BADPROCESS, PDL_ITRANS_TWOWAY|PDL_ITRANS_DO_DATAFLOW_ANY, pdl_converttypei_vtable_gentypes, 1, 2, pdl_converttypei_vtable_flags /*CORE21*/,
  pdl_converttypei_vtable_realdims, pdl_converttypei_vtable_parnames,
  pdl_converttypei_vtable_parflags, pdl_converttypei_vtable_partypes,
  pdl_converttypei_vtable_realdims_starts, pdl_converttypei_vtable_realdims_ind_ids, 0,
  0, pdl_converttypei_vtable_indnames,
  pdl_converttypei_redodims, pdl_converttypei_readdata, pdl_converttypei_writebackdata,
  NULL,
  sizeof(pdl_params_converttypei),"converttypei_new"
};

pdl_error pdl__set_output_type_badvalue(pdl_trans *trans, int recurse_count);
pdl_error pdl__converttypei_new_recprotect(pdl *PARENT, pdl *CHILD, pdl_datatypes totype, int recurse_count) {
  pdl_error PDL_err = {0, NULL, 0};
  PDL_RECURSE_CHECK(recurse_count);
  pdl_trans *trans = pdl_create_trans(&pdl_converttypei_vtable);
  pdl_params_converttypei *params = trans->params;
  trans->pdls[0] = PARENT;
  trans->pdls[1] = CHILD;
  PDL_RETERROR(PDL_err, pdl_trans_check_pdls(trans));
  if (PARENT->state & PDL_BADVAL) {
    trans->bvalflag = 1;
    trans->pdls[1]->state |= PDL_BADVAL;
  }
  trans->__datatype = PARENT->datatype;
  PDL_RETERROR(PDL_err, pdl__set_output_type_badvalue(trans, recurse_count + 1));
  trans->pdls[2] = trans->pdls[1]; /* copy for make_trans_mutual */
  trans->pdls[1]->datatype = params->totype = totype;
  PDL_RETERROR(PDL_err, pdl_make_trans_mutual((pdl_trans *)trans));
  return PDL_err;
}
pdl_error pdl_converttypei_new(pdl *PARENT, pdl *CHILD, pdl_datatypes totype) {
  return pdl__converttypei_new_recprotect(PARENT, CHILD, totype, 0);
}
