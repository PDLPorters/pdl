#ifndef __PDLCORE_H
#define __PDLCORE_H

/*
# version 18: zap twod, complex sorters, reorganised
*/

#define PDL_CORE_VERSION 18

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"  /* for the win32 perlCAPI crap */
#include "ppport.h"  /* include this AFTER XSUB.h */

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

#define PDL_TMP  0        /* Flags */
#define PDL_PERM 1

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

void pdl_makescratchhash(pdl *ret, PDL_Anyval data);
PDL_Indx av_ndcheck(AV* av, AV* dims, int level, int *datalevel);
pdl* pdl_from_array(AV* av, AV* dims, int type, pdl* p);
PDL_Anyval pdl_at( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, /* Value at x,y,z,... */
             PDL_Indx *incs, PDL_Indx offset, PDL_Indx ndims);
void pdl_vafftrans_free(pdl *it);
void pdl_vafftrans_remove(pdl * it);
void pdl_vafftrans_alloc(pdl *it);
void pdl_croak_param(pdl_transvtable *transvtable, int paramIndex, char *pat, ...);
void pdl_writebackdata_vaffine(pdl *it);
void pdl_readdata_vaffine(pdl *it);

#define PDL_CORE_LIST(X) \
  X(SvPDLV, pdl*, ( SV* )) \
  X(SetSV_PDL, void, ( SV *sv, pdl *it )) \
  X(pdlnew, pdl*, ()) \
  X(create, pdl*, (int type)) \
  X(destroy, void, (pdl *it)) \
  X(null, pdl*, ()) \
  X(hard_copy, pdl*, ( pdl* )) \
  X(converttype, void, ( pdl*, int )) \
  X(smalloc, void*, ( STRLEN )) \
  X(howbig, size_t, ( int )) \
  X(packdims, PDL_Indx*, ( SV* sv, PDL_Indx *ndims )) \
  X(setdims, void, ( pdl* it, PDL_Indx* dims, PDL_Indx ndims )) \
  X(grow, void, ( pdl* a, PDL_Indx newsize)) \
  X(at0, PDL_Anyval, ( pdl* x )) \
  X(reallocdims, void, ( pdl *it,PDL_Indx ndims )) \
  X(reallocthreadids, void, ( pdl *it,PDL_Indx ndims )) \
  X(resize_defaultincs, void, ( pdl *it )) /* Make incs out of dims */ \
  X(clearthreadstruct, void, (pdl_thread *it)) \
  X(initthreadstruct, void, (int nobl,pdl **pdls,PDL_Indx *realdims, \
    PDL_Indx *creating,PDL_Indx npdls,pdl_transvtable *transvtable, \
    pdl_thread *thread,PDL_Indx *ind_sizes,PDL_Indx *inc_sizes, \
    char *flags, int noPthreadFlag)) \
  X(default_redodims, void, (pdl_trans *)) \
  X(startthreadloop, int, (pdl_thread *thread,void (*func)(pdl_trans *), \
    pdl_trans *)) \
  X(get_threadoffsp, PDL_Indx*, (pdl_thread *thread)) /* For pthreading */ \
  X(get_threaddims, PDL_Indx*, (pdl_thread *thread)) /* For pthreading */ \
  X(iterthreadloop, int, (pdl_thread *thread, PDL_Indx which)) \
  X(freethreadloop, void, (pdl_thread *thread)) \
  X(thread_create_parameter, void, (pdl_thread *thread,PDL_Indx j, \
    PDL_Indx *dims, int temp)) \
  X(add_deletedata_magic, void,  (pdl *it,void (*func)(pdl *, Size_t param), \
    Size_t param)) /* Automagic destructor */ \
  X(setdims_careful, void, (pdl *pdl)) \
  X(put_offs, void, (pdl *pdl,PDL_Indx offs, PDL_Anyval val)) \
  X(get_offs, PDL_Anyval, (pdl *pdl,PDL_Indx offs)) \
  X(get, PDL_Anyval, (pdl *pdl,PDL_Indx *inds)) \
  X(set, void, ( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, \
    PDL_Indx *incs, PDL_Indx offs, PDL_Indx ndims, PDL_Anyval value)) \
  X(set_trans_childtrans, void, (pdl *it, pdl_trans *trans,PDL_Indx nth)) \
  X(set_trans_parenttrans, void, (pdl *it, pdl_trans *trans,PDL_Indx nth)) \
  X(create_trans, pdl_trans *, (size_t sz, short flags, pdl_transvtable *vtable)) \
  X(dim_checks, void, (pdl_transvtable *vtable, pdl **pdls, \
    pdl_thread *pdlthread, PDL_Indx *creating, PDL_Indx *ind_sizes)) \
  X(type_coerce, void, (pdl_trans *trans)) \
  X(get_convertedpdl, pdl *, (pdl *pdl,int type)) \
  X(make_trans_mutual, void, (pdl_trans *trans)) \
  X(make_physical, void, (pdl *it)) \
  X(make_physdims, void, (pdl *it)) \
  X(pdl_barf, void, (const char* pat,...)) \
  X(pdl_warn, void, (const char* pat,...)) \
  X(make_physvaffine, void, (pdl *it)) \
  X(allocdata, void, (pdl *it)) \
  X(safe_indterm, PDL_Indx, (PDL_Indx dsz, PDL_Indx at, char *file, int lineno)) \
  X(propagate_badflag, void, (pdl *it, int newval)) \
  X(propagate_badvalue, void, (pdl *it)) \
  X(children_changesoon, void, (pdl *it, int what)) \
  X(changed, void, (pdl *it, int what, int recursing)) \
  X(vaffinechanged, void, (pdl *it, int what)) \
  X(get_pdl_badvalue, PDL_Anyval, (pdl *it)) \
  X(get_badvalue, PDL_Anyval, (int datatype)) \
  X(set_datatype, void, (pdl *a, int datatype)) \
  X(hdr_copy, SV *, (SV *hdrp)) \
  X(hdr_childcopy, void, (pdl_trans *trans)) \
  X(dump, void, (pdl *it)) \
  X(sever, pdl *, (pdl *a))

/*************** Function prototypes *********************/
#define X(sym, rettype, args) \
  rettype pdl_ ## sym args;
PDL_CORE_LIST(X)
#undef X

#define X(symbol, ctype, ppsym, shortctype, defbval) \
PDL_Indx pdl_setav_ ## ppsym(ctype * pdata, AV* av, \
	PDL_Indx* pdims, PDL_Long ndims, int level, ctype undefval, pdl *p);
PDL_GENERICLIST(X)
#undef X

/* Structure to hold pointers core PDL routines so as to be used by many modules */

#if defined(PDL_clean_namespace) || defined(PDL_OLD_API)
#error PDL_clean_namespace and PDL_OLD_API defines have been removed. Use PDL->pdlnew() instead of PDL->new().
#endif

struct Core {
  I32    Version;
  badvals bvals;  /* store the default bad values */
/* Affine trans. THESE ARE SET IN ONE OF THE OTHER Basic MODULES
   and not in Core.xs ! */
  void (*readdata_affine)(pdl_trans *tr);
  void (*writebackdata_affine)(pdl_trans *tr);
  void (*affine_new)(pdl *par,pdl *child,PDL_Indx offs,SV *dims,SV *incs);
  /* Converttype. Similar */
  void (*converttypei_new)(pdl *par,pdl *child,int type);

#define X(sym, rettype, args) \
  rettype (*sym) args;
  PDL_CORE_LIST(X)
#undef X
};

typedef struct Core Core;

/* __PDLCORE_H */
#endif
