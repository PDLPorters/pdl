#ifndef __PDLCORE_H
#define __PDLCORE_H

/* version 20: memory-management changes */
/* on 21, look for comments "CORE21", unify pdl_trans per_pdl_flags, par_flags; remove threadloop #defines; change creating to char; relocate struct pdl.value appropriately, remove pdl_null, safe_indterm, initbroadcaststruct to take trans & remove in[cd]_sizes, remove pdl_{read,writeback}data_affine */
#define PDL_CORE_VERSION 20
#define startbroadcastloop startthreadloop
#define pdl_startbroadcastloop pdl_startthreadloop
#define iterbroadcastloop iterthreadloop
#define pdl_iterbroadcastloop pdl_iterthreadloop
#define get_broadcastdims get_threaddims

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"  /* for the win32 perlCAPI crap */
#include "ppport.h"  /* include this AFTER XSUB.h */

#include <stdint.h>

#if defined(CONTEXT) && defined(__osf__)
#undef CONTEXT
#endif

#ifdef PDL_IN_CORE
#define PDL_CORE_(func) pdl_##func
#else
#define PDL_CORE_(func) PDL->func
#endif

#include "pdl.h"
/* the next one causes trouble in c++ compiles - exclude for now */
#ifndef __cplusplus
#include "pdlmagic.h"
#endif

#define BIGGESTOF(a,b) ( a->nvals>b->nvals ? a->nvals : b->nvals )
#define SVavref(x) (SvROK(x) && SvTYPE(SvRV(x))==SVt_PVAV)

/*  Use our own barf and our own warn.
 *  We defer barf (and warn) handling until after multi-threaded (i.e pthreading)
 *  processing is finished.
 *  This is needed because segfaults happen when perl's croak is called
 *  during one of the spawned pthreads for PDL processing.
 */
#define barf PDL_CORE_(pdl_barf)
#undef warn
#define warn PDL_CORE_(pdl_warn)

PDL_Indx av_ndcheck(AV* av, AV* dims, int level, int *datalevel);
pdl* pdl_from_array(AV* av, AV* dims, int type, pdl* p);
PDL_Anyval pdl_at( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, /* Value at x,y,z,... */
             PDL_Indx *incs, PDL_Indx offset, PDL_Indx ndims);
pdl_error pdl_writebackdata_vaffine(pdl *it);
pdl_error pdl_readdata_vaffine(pdl *it);
pdl_error pdl_dim_checks(pdl_transvtable *vtable, pdl **pdls,
  pdl_broadcast *broadcast, PDL_Indx *creating,
  PDL_Indx *ind_sizes, char load_only);

/* pdlutil.c */
typedef enum {
  PDL_FLAGS_TRANS,
  PDL_FLAGS_PDL,
  PDL_FLAGS_VTABLE
} pdl_flags;
pdl_error pdl_croak_param(pdl_transvtable *transvtable, int paramIndex, char *pat, ...);
void pdl_print_iarr(PDL_Indx *iarr, int n);
void pdl_dump_broadcast(pdl_broadcast *broadcast);
void pdl_dump_broadcasting_info(
  int npdls, PDL_Indx* creating, int target_pthread,
  PDL_Indx *nbroadcastedDims, PDL_Indx **broadcastedDims, PDL_Indx **broadcastedDimSizes,
  int maxPthreadPDL, int maxPthreadDim, int maxPthread
);
void pdl_broadcast_mismatch_msg(
  char *s,
  pdl **pdls, pdl_broadcast *broadcast,
  PDL_Indx i, PDL_Indx j, PDL_Indx nimpl,
  PDL_Indx *realdims,PDL_Indx *creating
);
void pdl_dump_flags_fixspace(int flags, int nspac, pdl_flags type);
void pdl_dump_trans_fixspace(pdl_trans *it, int nspac);
void pdl_dump_anyval(PDL_Anyval v);

#define PDL_CORE_LIST(X) \
  X(SvPDLV, pdl*, ( SV* )) \
  X(SetSV_PDL, void, ( SV *sv, pdl *it )) \
  X(pdlnew, pdl*, ()) \
  X(destroy, pdl_error, (pdl *it)) \
  X(null, pdl*, ()) \
  X(scalar, pdl*, (PDL_Anyval anyval)) \
  X(hard_copy, pdl*, ( pdl* )) \
  X(converttype, pdl_error, ( pdl*, int )) \
  X(smalloc, void*, ( STRLEN )) \
  X(howbig, size_t, ( int )) \
  X(packdims, PDL_Indx*, ( SV* sv, PDL_Indx *ndims )) \
  X(setdims, pdl_error, ( pdl* it, PDL_Indx* dims, PDL_Indx ndims )) \
  X(at0, PDL_Anyval, ( pdl* x )) \
  X(reallocdims, pdl_error, ( pdl *it,PDL_Indx ndims )) \
  X(reallocbroadcastids, pdl_error, ( pdl *it,PDL_Indx ndims )) \
  X(resize_defaultincs, void, ( pdl *it )) /* Make incs out of dims */ \
  X(clearbroadcaststruct, void, (pdl_broadcast *it)) \
  X(initbroadcaststruct, pdl_error, (int nobl,pdl **pdls,PDL_Indx *realdims, \
    PDL_Indx *creating,PDL_Indx npdls,pdl_transvtable *transvtable, \
    pdl_broadcast *broadcast,PDL_Indx *ind_sizes,PDL_Indx *inc_sizes, \
    char *flags, int noPthreadFlag)) \
  X(redodims_default, pdl_error, (pdl_trans *)) \
  X(startbroadcastloop, int, (pdl_broadcast *broadcast,pdl_error (*func)(pdl_trans *), \
    pdl_trans *, pdl_error *)) \
  X(get_threadoffsp, PDL_Indx*, (pdl_broadcast *broadcast)) /* For pthreading */ \
  X(get_broadcastdims, PDL_Indx*, (pdl_broadcast *broadcast)) /* For pthreading */ \
  X(iterbroadcastloop, int, (pdl_broadcast *broadcast, PDL_Indx which)) \
  X(freebroadcaststruct, void, (pdl_broadcast *broadcast)) \
  X(broadcast_create_parameter, pdl_error, (pdl_broadcast *broadcast,PDL_Indx j, \
    PDL_Indx *dims, int temp)) \
  X(add_deletedata_magic, pdl_error,  (pdl *it,void (*func)(pdl *, Size_t param), \
    Size_t param)) /* Automagic destructor */ \
  X(setdims_careful, pdl_error, (pdl *pdl)) \
  X(get_offs, PDL_Anyval, (pdl *pdl,PDL_Indx offs)) \
  X(set, pdl_error, ( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, \
    PDL_Indx *incs, PDL_Indx offs, PDL_Indx ndims, PDL_Anyval value)) \
  X(create_trans, pdl_trans *, (pdl_transvtable *vtable)) \
  X(type_coerce, pdl_error, (pdl_trans *trans)) \
  X(trans_badflag_from_inputs, char, (pdl_trans *trans)) \
  X(get_convertedpdl, pdl *, (pdl *pdl,int type)) \
  X(make_trans_mutual, pdl_error, (pdl_trans *trans)) \
  X(make_physical, pdl_error, (pdl *it)) \
  X(make_physdims, pdl_error, (pdl *it)) \
  X(pdl_barf, void, (const char* pat,...)) \
  X(pdl_warn, void, (const char* pat,...)) \
  X(make_physvaffine, pdl_error, (pdl *it)) \
  X(allocdata, pdl_error, (pdl *it)) \
  X(safe_indterm, PDL_Indx, (PDL_Indx dsz, PDL_Indx at, char *file, int lineno)) \
  X(propagate_badflag, void, (pdl *it, int newval)) \
  X(propagate_badvalue, void, (pdl *it)) \
  X(changed, pdl_error, (pdl *it, int what, int recursing)) \
  X(get_pdl_badvalue, PDL_Anyval, (pdl *it)) \
  X(get_badvalue, PDL_Anyval, (int datatype)) \
  X(set_datatype, pdl_error, (pdl *a, int datatype)) \
  X(hdr_copy, SV *, (SV *hdrp)) \
  X(hdr_childcopy, void, (pdl_trans *trans)) \
  X(readdata_affine, pdl_error, (pdl_trans *trans)) \
  X(writebackdata_affine, pdl_error, (pdl_trans *trans)) \
  X(affine_new, pdl_error, (pdl *par,pdl *child,PDL_Indx offs,PDL_Indx *dims,PDL_Indx ndims,PDL_Indx *incs,PDL_Indx nincs)) \
  X(converttypei_new, pdl_error, (pdl *par,pdl *child,int type)) \
  X(dump, void, (pdl *it)) \
  X(sever, pdl_error, (pdl *a)) \
  X(slice_args_parse_sv, pdl_slice_args*, ( SV* )) \
  X(online_cpus, int, ()) \
  X(magic_get_thread, int, (pdl *)) \
  X(pdl_seed, uint64_t, ()) \
  X(trans_check_pdls, pdl_error, (pdl_trans *trans)) \
  X(make_error, pdl_error, (pdl_error_type e, const char *fmt, ...)) \
  X(make_error_simple, pdl_error, (pdl_error_type e, const char *msg)) \
  X(barf_if_error, void, (pdl_error err)) \
  X(error_accumulate, pdl_error, (pdl_error err_current, pdl_error err_new)) \
  X(packpdls, pdl **, ( SV* sv, PDL_Indx *npdls )) \
  X(unpackpdls, SV*, ( pdl **, PDL_Indx npdls )) \
  X(packstrings, char **, ( SV* sv, PDL_Indx *nstrings )) \

/*************** Function prototypes *********************/
#define X(sym, rettype, args) \
  rettype pdl_ ## sym args;
PDL_CORE_LIST(X)
#undef X

#define X(symbol, ctype, ppsym, ...) \
PDL_Indx pdl_setav_ ## ppsym(ctype * pdata, AV* av, \
  PDL_Indx* pdims, PDL_Indx ndims, PDL_Indx level, ctype undefval, pdl *p);
PDL_TYPELIST_ALL(X)
#undef X

/* Structure to hold pointers core PDL routines so as to be used by many modules */

#if defined(PDL_clean_namespace) || defined(PDL_OLD_API)
#error PDL_clean_namespace and PDL_OLD_API defines have been removed. Use PDL->pdlnew() instead of PDL->new().
#endif

struct Core {
  I32    Version;
  badvals bvals;  /* store the default bad values */

#define X(sym, rettype, args) \
  rettype (*sym) args;
  PDL_CORE_LIST(X)
#undef X
};

typedef struct Core Core;

#define PDL_DECLARE_PARAMETER(type, name, pdlname, nullcheck, ppsym) \
  type *name ## _datap = ((type *)(PDL_REPRP(pdlname))); \
  if ((nullcheck) && pdlname->nvals > 0 && !name ## _datap) \
    return PDL_CORE_(make_error)(PDL_EUSERERROR, "parameter " #name "=%p got NULL data", pdlname); \

#define PDL_DECLARE_PARAMETER_BADVAL(type, name, pdlname, nullcheck, ppsym) \
  PDL_DECLARE_PARAMETER(type, name, pdlname, nullcheck, ppsym) \
  PDL_Anyval name ## _anyval_badval = PDL_CORE_(get_pdl_badvalue)(pdlname); (void)name ## _anyval_badval; \
  type name ## _badval = name ## _anyval_badval.value.ppsym; (void)name ## _badval; \
  char name ## _badval_isnan = PDL_ISNAN_ ## ppsym(name ## _badval); (void) name ## _badval_isnan; \

/* __PDLCORE_H */
#endif
