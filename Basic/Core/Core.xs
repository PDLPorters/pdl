#ifndef WIN32
#include <unistd.h>
#include <fcntl.h>
#endif

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#if defined(CONTEXT)
#undef CONTEXT
#endif

#include "pdl.h"      /* Data structure declarations */
#define PDL_IN_CORE /* access funcs directly not through PDL-> */
#include "pdlcore.h"  /* Core declarations */
#include "pdlperl.h"

#define TRANS_PDLS(from, to) \
    pdl_transvtable *vtable = trans->vtable; \
    if (!vtable) croak("This transformation doesn't have a vtable!"); \
    PDL_Indx i; \
    EXTEND(SP, to - from); \
    for (i=from; i<to; i++) { \
      SV *sv = sv_newmortal(); \
      if (!trans->pdls[i]->sv) trans->pdls[i]->state |= PDL_DYNLANG_NODESTROY; \
      pdl_SetSV_PDL(sv, trans->pdls[i]); \
      PUSHs(sv); \
    }

#define PDL_FLAG_COMMA(f) f,
#define PDL_FLAG_STRCOMMA(f) #f,
#define PDL_FLAG_DUMP(macro, flagvar) \
    int flagval[] = { \
      macro(PDL_FLAG_COMMA) \
      0 \
    }; \
    char *flagchar[] = { \
      macro(PDL_FLAG_STRCOMMA) \
      NULL \
    }; \
    int i, f = flagvar; \
    for (i=0; flagval[i]!=0; i++) \
      if (f & flagval[i]) \
        XPUSHs(sv_2mortal(newSVpv(flagchar[i], 0)));

#define setflag(reg,flagval,val) (val?(reg |= flagval):(reg &= ~flagval))

Core PDL; /* Struct holding pointers to shared C routines */

int pdl_debugging=0;
int pdl_autopthread_targ   = 0; /* No auto-pthreading unless set using the set_autopthread_targ */
int pdl_autopthread_actual = 0;
PDL_Indx pdl_autopthread_dim = -1;
int pdl_autopthread_size   = 1;

char *_dims_from_args(AV *av, SV **svs, IV n) {
  IV i;
  for (i = 0; i < n; i++) {
    SV *sv = svs[i];
    if (!SvROK(sv)) {
      if (SvTRUE(sv) && SvIV(sv) < 0) return "Dimensions must be non-negative";
      if (SvTRUE(sv))
        SvREFCNT_inc(sv); /* stack entries are mortal */
      else
        sv = newSViv(0);
      av_push(av, sv);
      continue;
    }
    if (SvROK(sv) && !sv_derived_from(sv, "PDL")) return "Trying to use non-ndarray as dimensions?";
    pdl *p = pdl_SvPDLV(sv);
    if (!p) return "Failed to get PDL from arg";
    if (p->ndims > 1) return "Trying to use multi-dim ndarray as dimensions?";
    PDL_Indx nvals = p->nvals, v;
    if (nvals > 10) warn("creating > 10 dim ndarray (ndarray arg)!");
    for (v = 0; v < nvals; v++) {
      PDL_Anyval anyval = { PDL_INVALID, {0} };
      ANYVAL_FROM_CTYPE_OFFSET(anyval, p->datatype, PDL_REPRP(p), PDL_REPROFFS(p)+v);
      if (anyval.type < 0) return "Error getting value from ndarray";
      SV *dv = newSV(0);
      ANYVAL_TO_SV(dv, anyval);
      if (SvIV(dv) < 0) return "Dimensions must be non-negative";
      av_push(av, dv);
    }
  }
  return NULL;
}

static inline SV *pdl2avref(pdl *x, char flatten) {
  int stop = 0, badflag = (x->state & PDL_BADVAL) > 0;
  volatile PDL_Anyval pdl_val = { PDL_INVALID, {0} }; /* same reason as below */
  volatile PDL_Anyval pdl_badval = { PDL_INVALID, {0} };
  if (badflag) {
    if (!(x->has_badvalue && x->badvalue.type != x->datatype)) {
      if (x->has_badvalue)
        pdl_badval = x->badvalue;
      else {
#define X(datatype, ctype, ppsym, ...) \
         pdl_badval.type = datatype; pdl_badval.value.ppsym = PDL.bvals.ppsym;
        PDL_GENERICSWITCH(PDL_TYPELIST_ALL, x->datatype, X, )
#undef X
      }
    }
    if (pdl_badval.type < 0) barf("Error getting badvalue, type=%d", pdl_badval.type);
  }
  pdl_barf_if_error(pdl_make_physvaffine( x ));
  if (!x->nvals) return newRV_noinc((SV *)newAV());
  void *data = PDL_REPRP(x);
  PDL_Indx ind, inds[!x->ndims ? 1 : x->ndims];
  AV *avs[(flatten || !x->ndims) ? 1 : x->ndims];
  if (flatten || !x->ndims) {
    inds[0] = 0;
    avs[0] = newAV();
    av_extend(avs[0], flatten ? x->nvals : 1);
    if (flatten) for (ind=1; ind < x->ndims; ind++) inds[ind] = 0;
  } else
    for (ind=x->ndims-1; ind >= 0; ind--) {
      inds[ind] = 0;
      avs[ind] = newAV();
      av_extend(avs[ind], x->dims[ind]);
      if (ind < x->ndims-1) av_store(avs[ind+1], 0, newRV_noinc((SV *)avs[ind]));
    }
  PDL_Indx *incs = PDL_REPRINCS(x), offs = PDL_REPROFFS(x), lind = 0;
  while (!stop) {
    pdl_val.type = PDL_INVALID;
    PDL_Indx ioff = pdl_get_offset(inds, x->dims, incs, offs, x->ndims);
    if (ioff >= 0)
      ANYVAL_FROM_CTYPE_OFFSET(pdl_val, x->datatype, data, ioff);
    if (pdl_val.type < 0) croak("Position out of range");
    SV *sv;
    if (badflag) {
      /* volatile because gcc optimiser otherwise won't recalc for complex double when long-double code added */
      volatile int isbad = ANYVAL_ISBAD(pdl_val, pdl_badval);
      if (isbad == -1) croak("ANYVAL_ISBAD error on types %d, %d", pdl_val.type, pdl_badval.type);
      if (isbad)
        sv = newSVpvn( "BAD", 3 );
      else {
        sv = newSV(0);
        ANYVAL_TO_SV(sv, pdl_val);
      }
    } else {
      sv = newSV(0);
      ANYVAL_TO_SV(sv, pdl_val);
    }
    av_store( avs[0], flatten ? lind++ : inds[0], sv );
    stop = 1;
    char didwrap[x->ndims];
    for (ind = 0; ind < x->ndims; ind++) didwrap[ind] = 0;
    for (ind = 0; ind < x->ndims; ind++) {
      if (++(inds[ind]) < x->dims[ind]) {
        stop = 0; break;
      }
      inds[ind] = 0;
      didwrap[ind] = 1;
    }
    if (stop) break;
    if (flatten) continue;
    for (ind=x->ndims-2; ind >= 0; ind--) { /* never redo outer so -2 */
      if (!didwrap[ind]) continue;
      avs[ind] = newAV();
      av_extend(avs[ind], x->dims[ind]);
      av_store(avs[ind+1], inds[ind+1], newRV_noinc((SV *)avs[ind]));
    }
  }
  return newRV_noinc((SV *)avs[(flatten || !x->ndims) ? 0 : x->ndims-1]);
}

MODULE = PDL::Core     PACKAGE = PDL

# Destroy a PDL - note if a hash do nothing, the $$x{PDL} component
# will be destroyed anyway on a separate call

void
DESTROY(sv)
  SV *	sv;
  PREINIT:
    pdl *self;
  CODE:
    if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVHV) return;
    self = pdl_SvPDLV(sv);
    PDLDEBUG_f(printf("DESTROYING %p\n",self));
    if (self == NULL) return;
    if (self->state & PDL_DYNLANG_NODESTROY) {
      PDLDEBUG_f(printf(" (actually just setting sv to NULL)\n"));
      self->state &= ~PDL_DYNLANG_NODESTROY;
      self->sv = NULL;
      return;
    }
    pdl_barf_if_error(pdl_destroy(self));

SV *
new_from_specification(invoc, ...)
  SV *invoc;
  ALIAS:
    PDL::zeroes = 1
  CODE:
    char ispdl = ix && SvROK(invoc) && sv_derived_from(invoc, "PDL");
    pdl *pdl_given = NULL;
    if (ispdl) {
      if (!(pdl_given = pdl_SvPDLV(invoc))) barf("Failed to get PDL from arg");
      if (pdl_given->state & PDL_INPLACE) {
        amagic_call(invoc, sv_2mortal(newSViv(0)), concat_ass_amg, AMGf_assign);
        ST(0) = invoc;
        XSRETURN(1);
      }
    }
    IV argstart = 1, type = PDL_D;
    if (items > 1 && sv_derived_from(ST(1), "PDL::Type")) {
      argstart++;
      AV *type_av = (AV *)SvRV(ST(1));
      if (!type_av) barf("Arg 1 not a reference");
      if (SvTYPE((SV *)type_av) != SVt_PVAV) barf("Arg 1 not an array-ref");
      SV **firstval = av_fetch(type_av, 0, TRUE);
      if (!firstval) barf("Failed to get type elt 0");
      type = SvIV(*firstval);
    } else if (ispdl)
      type = pdl_given->datatype;
    ENTER; SAVETMPS;
    SV *dims_ref = NULL;
    if (!(ispdl && items == 1)) {
      AV *dims_av = newAV();
      if (!dims_av) barf("Failed to make AV");
      dims_ref = sv_2mortal(newRV_noinc((SV *)dims_av));
      if (!dims_ref) barf("Failed to make ref to AV");
      char *retstr = _dims_from_args(dims_av, &ST(argstart), items-argstart);
      if (retstr) barf("%s", retstr);
    }
    if (strcmp(SvPV_nolen(invoc), "PDL") == 0) {
      pdl *p = pdl_pdlnew();
      if (!p) barf("Failed to create ndarray");
      p->datatype = type;
      PDL_Indx ndims, *dims;
      if (dims_ref) {
        dims = pdl_packdims(dims_ref, &ndims);
        if (!dims) barf("Failed to unpack dims");
      } else {
        dims = pdl_given->dims;
        ndims = pdl_given->ndims;
      }
      pdl_barf_if_error(pdl_setdims(p, dims, ndims));
      if (ix) pdl_barf_if_error(pdl_make_physical(p));
      pdl_SetSV_PDL(RETVAL = newSV(0), p);
    } else {
      PUSHMARK(SP);
      PUSHs(ST(0));
      PUTBACK;
      int retvals = perl_call_method("initialize", G_SCALAR);
      SPAGAIN;
      if (retvals != 1) barf("initialize returned no values");
      SvREFCNT_inc(RETVAL = POPs);
      PUSHMARK(SP);
      EXTEND(SP, 2); PUSHs(RETVAL); mPUSHi(type);
      PUTBACK;
      perl_call_method("set_datatype", G_VOID);
      SPAGAIN;
      if (!dims_ref) {
        AV *dims_av = newAV();
        if (!dims_av) barf("Failed to make AV");
        dims_ref = sv_2mortal(newRV_noinc((SV *)dims_av));
        if (!dims_ref) barf("Failed to make ref to AV");
        PDL_Indx i, *dims = pdl_given->dims;
        for (i = 0; i < pdl_given->ndims; i++)
          av_push(dims_av, newSViv(dims[i]));
      }
      PUSHMARK(SP);
      EXTEND(SP, 2); PUSHs(RETVAL); PUSHs(dims_ref);
      PUTBACK;
      perl_call_method("setdims", G_VOID);
      SPAGAIN;
    }
    FREETMPS; LEAVE;
  OUTPUT:
    RETVAL

SV *
inplace(self, ...)
  SV *self
  CODE:
    pdl *p = pdl_SvPDLV(self);
    if (!p) barf("Failed to get PDL from arg");
    p->state |= PDL_INPLACE;
    SvREFCNT_inc(RETVAL = self);
  OUTPUT:
    RETVAL

SV *
flowing(self)
  SV *self
  CODE:
    pdl *p = pdl_SvPDLV(self);
    if (!p) barf("Failed to get PDL from arg");
    p->state |= PDL_DATAFLOW_F;
    SvREFCNT_inc(RETVAL = self);
  OUTPUT:
    RETVAL

SV *
topdl(klass, arg1, ...)
  SV *klass;
  SV *arg1;
  CODE:
    if (items > 2 ||
      (!SvROK(arg1) && SvTYPE(arg1) < SVt_PVAV) ||
      (SvROK(arg1) && SvTYPE(SvRV(arg1)) == SVt_PVAV)
    ) {
      PUSHMARK(SP - items); /* this passes current set of args on */
      int retvals = perl_call_method("new", G_SCALAR);
      SPAGAIN;
      if (retvals != 1) barf("new returned no values");
      RETVAL = POPs;
    } else if (SvROK(arg1) && SvOBJECT(SvRV(arg1))) {
      RETVAL = arg1;
    } else {
      barf("Can not convert a %s to a %s", sv_reftype(arg1, 1), SvPV_nolen(klass));
    }
    SvREFCNT_inc(RETVAL);
  OUTPUT:
    RETVAL

int
has_vafftrans(self)
	pdl *self;
	CODE:
	RETVAL = !!self->vafftrans;
	OUTPUT:
	RETVAL

int
has_badvalue(self)
	pdl *self;
	CODE:
	RETVAL = !!self->has_badvalue;
	OUTPUT:
	RETVAL

# Return the transformation object or an undef otherwise.
pdl_trans *
trans_parent(self)
	pdl *self;
	CODE:
	RETVAL = self->trans_parent;
	OUTPUT:
	RETVAL

void
trans_children(self)
  pdl *self
  PPCODE:
    PDL_DECL_CHILDLOOP(self);
    PDL_START_CHILDLOOP(self)
      pdl_trans *t = PDL_CHILDLOOP_THISCHILD(self);
      if (!t) continue;
      SV *sv = sv_newmortal();
      sv_setref_pv(sv, "PDL::Trans", (void*)t);
      XPUSHs(sv);
    PDL_END_CHILDLOOP(self)

INCLUDE_COMMAND: $^X -e "require q{./Dev.pm}; PDL::Core::Dev::generate_core_flags()"

IV
address(self)
  pdl *self;
  CODE:
    RETVAL = PTR2IV(self);
  OUTPUT:
    RETVAL

IV
address_data(self)
  pdl *self;
  CODE:
    RETVAL = PTR2IV(self->data);
  OUTPUT:
    RETVAL

PDL_Indx
nelem_nophys(x)
  pdl *x
  CODE:
    RETVAL = x->nvals;
  OUTPUT:
    RETVAL

# only returns list, not context-aware
void
dimincs_nophys(x)
  pdl *x
  PPCODE:
    EXTEND(SP, x->ndims);
    PDL_Indx i;
    for(i=0; i<x->ndims; i++) mPUSHi(PDL_REPRINC(x,i));

# only returns list, not context-aware
void
dims_nophys(x)
  pdl *x
  PPCODE:
    EXTEND(SP, x->ndims);
    PDL_Indx i;
    for(i=0; i<x->ndims; i++) mPUSHi(x->dims[i]);

# only returns list, not context-aware
void
broadcastids_nophys(x)
  pdl *x
  PPCODE:
    EXTEND(SP, x->nbroadcastids);
    PDL_Indx i;
    for(i=0; i<x->nbroadcastids; i++) mPUSHi(x->broadcastids[i]);

void
firstvals_nophys(x)
  pdl *x
  PPCODE:
    if (!(x->state & PDL_ALLOCATED)) barf("firstvals_nophys called on non-ALLOCATED %p", x);
    PDL_Indx i, maxvals = PDLMIN(10, x->nvals);
    EXTEND(SP, maxvals);
    for(i=0; i<maxvals; i++) {
      PDL_Anyval anyval = { PDL_INVALID, {0} };
      ANYVAL_FROM_CTYPE_OFFSET(anyval, x->datatype, PDL_REPRP(x), PDL_REPROFFS(x)+i);
      if (anyval.type < 0) barf("Error getting value, type=%d", anyval.type);
      SV *sv = sv_newmortal();
      ANYVAL_TO_SV(sv, anyval);
      PUSHs(sv);
      PUTBACK;
    }

IV
vaffine_from(self)
  pdl *self;
  CODE:
    if (!self->vafftrans) barf("vaffine_from called on %p with NULL vafftrans", self);
    RETVAL = PTR2IV(self->vafftrans->from);
  OUTPUT:
    RETVAL

void
flags(x)
  pdl *x
  PPCODE:
    PDL_FLAG_DUMP(PDL_LIST_FLAGS_PDLSTATE, x->state)

int
set_donttouchdata(it,size=-1)
      pdl *it
      IV size
      CODE:
            it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
            if (size >= 0) it->nbytes = size;
            RETVAL = 1;
      OUTPUT:
            RETVAL

IV
nbytes(self)
  pdl *self;
  CODE:
    RETVAL = self->nbytes;
  OUTPUT:
    RETVAL

# Free the datasv if possible
void
freedata(it)
      pdl *it
      CODE:
	if(it->datasv) {
		PDLDEBUG_f(printf("pdl=%p SvREFCNT_dec datasv=%p\n",it,it->datasv));
		SvREFCNT_dec(it->datasv);
		it->datasv=0;
		it->data=0;
	} else if(it->data) {
		die("Trying to free data of pdl with data != 0 and datasv==0");
	}

IV
datasv_refcount(p)
  pdl *p
  CODE:
    if (!p->datasv) barf("NULL datasv");
    RETVAL = SvREFCNT((SV*)p->datasv);
  OUTPUT:
    RETVAL

int
set_data_by_offset(it,orig,offset)
      pdl *it
      pdl *orig
      STRLEN offset
      CODE:
              it->data = ((char *) orig->data) + offset;
              it->datasv = orig->datasv;
              (void)SvREFCNT_inc(it->datasv);
              it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
              RETVAL = 1;
      OUTPUT:
              RETVAL

PDL_Indx
nelem(x)
	pdl *x
	CODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		RETVAL = x->nvals;
	OUTPUT:
		RETVAL


# Call my howbig function

int
howbig_c(datatype)
   int	datatype
   CODE:
     RETVAL = pdl_howbig(datatype);
   OUTPUT:
     RETVAL


int
set_autopthread_targ(i)
	int i;
	CODE:
	RETVAL = i;
	pdl_autopthread_targ = i;
	OUTPUT:
	RETVAL

int
get_autopthread_targ()
	CODE:
	RETVAL = pdl_autopthread_targ;
	OUTPUT:
	RETVAL


int
set_autopthread_size(i)
	int i;
	CODE:
	RETVAL = i;
	pdl_autopthread_size = i;
	OUTPUT:
	RETVAL

int
get_autopthread_size()
	CODE:
	RETVAL = pdl_autopthread_size;
	OUTPUT:
	RETVAL

int
get_autopthread_actual()
	CODE:
	RETVAL = pdl_autopthread_actual;
	OUTPUT:
	RETVAL

int
get_autopthread_dim()
	CODE:
	RETVAL = pdl_autopthread_dim;
	OUTPUT:
	RETVAL

void
_ci(...)
 PPCODE:
  PDL_XS_SCALAR(PDL_CD, C, 0 + 1I)

void
_nan(...)
 PPCODE:
  PDL_XS_SCALAR(PDL_D, D, NAN)

void
_inf(...)
 PPCODE:
  PDL_XS_SCALAR(PDL_D, D, INFINITY)

MODULE = PDL::Core     PACKAGE = PDL::Trans

void
parents(trans)
  pdl_trans *trans
  PPCODE:
    TRANS_PDLS(0, vtable->nparents)

void
children(trans)
  pdl_trans *trans
  PPCODE:
    TRANS_PDLS(vtable->nparents, vtable->npdls)

IV
address(self)
  pdl_trans *self;
  CODE:
    RETVAL = PTR2IV(self);
  OUTPUT:
    RETVAL

void
flags(x)
  pdl_trans *x
  PPCODE:
    PDL_FLAG_DUMP(PDL_LIST_FLAGS_PDLTRANS, x->flags)

pdl_transvtable *
vtable(x)
  pdl_trans *x
  CODE:
    if (!x->vtable) barf("%p has NULL vtable", x);
    RETVAL = x->vtable;
  OUTPUT:
    RETVAL

int
affine(x)
  pdl_trans *x
  CODE:
    RETVAL= !!(x->flags & PDL_ITRANS_ISAFFINE);
  OUTPUT:
    RETVAL

IV
offs(self)
  pdl_trans *self;
  CODE:
    RETVAL = PTR2IV(self->offs);
  OUTPUT:
    RETVAL

void
incs(x)
  pdl_trans *x;
  PPCODE:
    if (!(x->flags & PDL_ITRANS_ISAFFINE)) barf("incs called on non-vaffine trans %p", x);
    PDL_Indx i, max = x->incs ? x->pdls[1]->ndims : 0;
    EXTEND(SP, max);
    for(i=0; i<max; i++) mPUSHi(x->incs[i]);

void
ind_sizes(x)
  pdl_trans *x;
  PPCODE:
    PDL_Indx i, max = x->vtable->ninds;
    EXTEND(SP, max);
    for(i=0; i<max; i++) mPUSHi(x->ind_sizes[i]);

void
inc_sizes(x)
  pdl_trans *x;
PPCODE:
  PDL_Indx i, max = x->vtable->nind_ids; /* CORE21 rename nind_ids */
  EXTEND(SP, max);
  for(i=0; i<max; i++) mPUSHi(x->inc_sizes[i]);

MODULE = PDL::Core     PACKAGE = PDL::Trans::VTable

char *
name(x)
  pdl_transvtable *x;
  CODE:
    RETVAL = x->name;
  OUTPUT:
    RETVAL

void
flags(x)
  pdl_transvtable *x
  PPCODE:
    PDL_FLAG_DUMP(PDL_LIST_FLAGS_PDLVTABLE, x->flags)

void
par_names(x)
  pdl_transvtable *x
  PPCODE:
    EXTEND(SP, 2);
    PDL_Indx i;
    for (i=0; i < 2; i++) {
      AV *av = (AV *)sv_2mortal((SV *)newAV());
      if (!av) barf("Failed to create AV");
      mPUSHs(newRV_inc((SV *)av));
      PDL_Indx start = i==0 ? 0 : x->nparents, j, max = i==0 ? x->nparents : x->npdls;
      av_extend(av, max-start);
      for (j = start; j < max; j++) {
        SV *sv = newSVpv(x->par_names[j], 0);
        if (!sv) barf("Failed to create SV");
        if (!av_store( av, j-start, sv )) {
          SvREFCNT_dec(sv);
          barf("Failed to store SV");
        }
      }
    }

MODULE = PDL::Core     PACKAGE = PDL::Core

IV
seed()
  CODE:
    RETVAL = pdl_pdl_seed();
  OUTPUT:
    RETVAL

int
online_cpus()
  CODE:
    RETVAL = pdl_online_cpus();
  OUTPUT:
    RETVAL

unsigned int
is_scalar_SvPOK(arg)
	SV* arg;
	CODE:
	RETVAL = SvPOK(arg);
	OUTPUT:
	RETVAL


int
set_debugging(i)
	int i;
	CODE:
	RETVAL = pdl_debugging;
	pdl_debugging = i;
	OUTPUT:
	RETVAL


SV *
at_bad_c(x,pos)
   pdl*	x
   PDL_Indx pos_count=0;
   PDL_Indx *pos
   PREINIT:
    PDL_Indx ipos;
    int badflag;
    volatile PDL_Anyval result = { PDL_INVALID, {0} };
   CODE:
    if (pos == NULL)
      barf("Invalid NULL position given");
    pdl_barf_if_error(pdl_make_physvaffine( x ));
    if (pos_count < x->ndims)
      barf("Invalid position: %"IND_FLAG" coordinate%s given for ndarray with %"IND_FLAG" dim%s", pos_count, pos_count == 1 ? "" : "s", x->ndims, x->ndims == 1 ? "" : "s");
    for (ipos=0; ipos < x->ndims; ipos++)
      if (pos[ipos] + ((pos[ipos] < 0) ? x->dims[ipos] : 0) >= x->dims[ipos])
        barf("Position %"IND_FLAG" at dimension %"IND_FLAG" out of range", pos[ipos], ipos);
    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] ndarray is treated as an [3,1,5,1,1,1,....]
     *  infinite dim ndarray
     */
    for (ipos=x->ndims; ipos<pos_count; ipos++)
      if (pos[ipos] != 0)
        barf("Invalid position %"IND_FLAG" at dimension %"IND_FLAG, pos[ipos], ipos);
    PDL_Indx ioff = pdl_get_offset(pos, x->dims, PDL_REPRINCS(x), PDL_REPROFFS(x), x->ndims);
    if (ioff >= 0)
      ANYVAL_FROM_CTYPE_OFFSET(result, x->datatype, PDL_REPRP(x), ioff);
    if (result.type < 0) barf("Position out of range");
   badflag = (x->state & PDL_BADVAL) > 0;
   if (badflag) {
     volatile PDL_Anyval badval = { PDL_INVALID, {0} };
     if (!(x->has_badvalue && x->badvalue.type != x->datatype)) {
       if (x->has_badvalue)
         badval = x->badvalue;
       else {
#define X(datatype, ctype, ppsym, ...) \
          badval.type = datatype; badval.value.ppsym = PDL.bvals.ppsym;
         PDL_GENERICSWITCH(PDL_TYPELIST_ALL, x->datatype, X, )
#undef X
       }
     }
     if (badval.type < 0) barf("Error getting badvalue, type=%d", badval.type);
     int isbad = ANYVAL_ISBAD(result, badval);
     if (isbad == -1) barf("ANYVAL_ISBAD error on types %d, %d", result.type, badval.type);
     if (isbad)
       RETVAL = newSVpvn( "BAD", 3 );
     else {
       RETVAL = newSV(0);
       ANYVAL_TO_SV(RETVAL, result);
     }
   } else {
     RETVAL = newSV(0);
     ANYVAL_TO_SV(RETVAL, result);
   }
    OUTPUT:
     RETVAL

SV *
listref_c(x)
   pdl *x
  CODE:
   RETVAL = pdl2avref(x, 1);
  OUTPUT:
   RETVAL

void
set_c(x,pos,value)
    pdl*	x
    PDL_Indx pos_count=0;
    PDL_Indx *pos
    PDL_Anyval	value
   PREINIT:
    PDL_Indx ipos;
   CODE:
    pdl_barf_if_error(pdl_make_physvaffine( x ));
    if (pos == NULL || pos_count < x->ndims)
       croak("Invalid position");
    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] ndarray is treated as an [3,1,5,1,1,1,....]
     *  infinite dim ndarray
     */
    for (ipos=x->ndims; ipos<pos_count; ipos++)
      if (pos[ipos] != 0)
         croak("Invalid position");
    pdl_barf_if_error(pdl_set(PDL_REPRP(x), x->datatype, pos, x->dims,
        PDL_REPRINCS(x), PDL_REPROFFS(x),
	x->ndims,value));
    pdl_barf_if_error(pdl_changed(PDL_VAFFOK(x)?x->vafftrans->from:x, PDL_PARENTDATACHANGED, 0));

BOOT:
{
   /* Initialize structure of pointers to core C routines */

   PDL.Version     = PDL_CORE_VERSION;
#define X(sym, rettype, args) PDL.sym = pdl_ ## sym;
   PDL_CORE_LIST(X)
#undef X
#define X(symbol, ctype, ppsym, shortctype, defbval, ...) \
  PDL.bvals.ppsym = defbval;
   PDL_TYPELIST_ALL(X)
#undef X
   /*
      "Publish" pointer to this structure in perl variable for use
       by other modules
   */
   sv_setiv(get_sv("PDL::SHARE",TRUE|GV_ADDMULTI), PTR2IV(&PDL));
}

# make ndarray belonging to 'class' and of type 'type'
# from avref 'array_ref' which is checked for being
# rectangular first

SV*
pdl_avref(array_ref, class, type)
     SV* array_ref
     char* class
     int type
  PREINIT:
     AV *dims, *av;
     int datalevel = -1;
     SV* psv;
     pdl* p;
  CODE:
     /* make an ndarray from a Perl array ref */

     if (!SvROK(array_ref))
       croak("pdl_avref: not a reference");


     if (SvTYPE(SvRV(array_ref)) != SVt_PVAV)
       croak("pdl_avref: not an array reference");

     // Expand the array ref to a list, and allocate a Perl list to hold the dimlist
     av = (AV *) SvRV(array_ref);
     dims = (AV *) sv_2mortal( (SV *) newAV());

     av_store(dims,0,newSViv((IV) av_len(av)+1));

     av_ndcheck(av,dims,0,&datalevel);

     /* printf("will make type %s\n",class); */
     /*
	at this stage start making an ndarray and populate it with
	values from the array (which has already been checked in av_check)
     */
     ENTER; SAVETMPS;
     if (strcmp(class,"PDL") == 0) {
        p = pdl_from_array(av,dims,type,NULL); /* populate with data */
        RETVAL = newSV(0);
        pdl_SetSV_PDL(RETVAL,p);
     } else {
       /* call class->initialize method */
       PUSHMARK(SP);
       XPUSHs(sv_2mortal(newSVpv(class, 0)));
       PUTBACK;
       perl_call_method("initialize", G_SCALAR);
       SPAGAIN;
       psv = POPs;
       PUTBACK;
       p = pdl_SvPDLV(psv); /* and get ndarray from returned object */
       RETVAL = psv;
       SvREFCNT_inc(psv);
       pdl_from_array(av,dims,type,p); /* populate ;) */
     }
     FREETMPS; LEAVE;
     OUTPUT:
     RETVAL

MODULE = PDL::Core     PACKAGE = PDL::Core     PREFIX = pdl_

int
pdl_pthreads_enabled()

MODULE = PDL::Core	PACKAGE = PDL	PREFIX = pdl_

int
isnull(self)
	pdl *self;
	CODE:
		RETVAL= !!(self->state & PDL_NOMYDIMS);
	OUTPUT:
		RETVAL

pdl *
make_physical(self)
	pdl *self;
	CODE:
		pdl_barf_if_error(pdl_make_physical(self));
		RETVAL = self;
	OUTPUT:
		RETVAL

pdl *
make_physvaffine(self)
	pdl *self;
	CODE:
		pdl_barf_if_error(pdl_make_physvaffine(self));
		RETVAL = self;
	OUTPUT:
		RETVAL

pdl *
make_physdims(self)
	pdl *self;
	CODE:
		pdl_barf_if_error(pdl_make_physdims(self));
		RETVAL = self;
	OUTPUT:
		RETVAL

pdl *
_convert_int(self, new_dtype)
	pdl *self;
	int new_dtype;
	CODE:
		RETVAL = pdl_get_convertedpdl(self, new_dtype);
		if (!RETVAL) barf("convert error");
	OUTPUT:
		RETVAL

void
set_datatype(a,datatype)
   pdl *a
   int datatype
   CODE:
     pdl_barf_if_error(pdl_set_datatype(a, datatype));

int
get_datatype(self)
	pdl *self
	CODE:
	RETVAL = self->datatype;
	OUTPUT:
	RETVAL

pdl *
pdl_sever(src)
	pdl *src;
	CODE:
		pdl_barf_if_error(pdl_sever(src));
		RETVAL = src;
	OUTPUT:
		RETVAL

void
pdl_dump(x)
  pdl *x;

void
pdl_add_threading_magic(it,nthdim,nthreads)
	pdl *it
	PDL_Indx nthdim
	PDL_Indx nthreads
	CODE:
		pdl_barf_if_error(pdl_add_threading_magic(it,nthdim,nthreads));

void
pdl_remove_threading_magic(it)
	pdl *it
	CODE:
		pdl_barf_if_error(pdl_add_threading_magic(it,-1,-1));

MODULE = PDL::Core	PACKAGE = PDL

PDL_Anyval
sclr(it)
   pdl* it
   CODE:
        /* get the first element of an ndarray and return as
         * Perl scalar (autodetect suitable type IV or NV)
         */
        pdl_barf_if_error(pdl_make_physdims(it));
        if (it->nvals > 1) barf("multielement ndarray in 'sclr' call");
        pdl_barf_if_error(pdl_make_physvaffine( it ));
        RETVAL.type = PDL_INVALID;
        if (it->nvals == 1)
          ANYVAL_FROM_CTYPE_OFFSET(RETVAL, it->datatype, PDL_REPRP(it), PDL_REPROFFS(it));
        if (RETVAL.type < 0) croak("Position out of range");
    OUTPUT:
        RETVAL

SV *
initialize(class)
  SV *class
  CODE:
    HV *bless_stash = SvROK(class)
      ? SvSTASH(SvRV(class)) /* a reference to a class */
      : gv_stashsv(class, 0); /* a class name */
    RETVAL = newSV(0);
    pdl *n = pdl_pdlnew();
    if (!n) pdl_pdl_barf("Error making null pdl");
    pdl_SetSV_PDL(RETVAL,n);   /* set a null PDL to this SV * */
    RETVAL = sv_bless(RETVAL, bless_stash); /* bless appropriately  */
    OUTPUT:
    RETVAL

# undocumented for present. returns PDL still needing dims and datatype
# offset is in bytes, not elements
SV *
new_around_datasv(class, datasv_pointer, offset=0)
  SV *class
  IV datasv_pointer
  IV offset
CODE:
  if (offset < 0)
    pdl_pdl_barf("Tried to new_around_datasv with negative offset=%" IVdf, offset);
  STRLEN sv_len = SvCUR((SV*)datasv_pointer);
  if (offset >= sv_len)
    pdl_pdl_barf("Tried to new_around_datasv with offset=%" IVdf " >= %zd", offset, sv_len);
  HV *bless_stash = SvROK(class)
    ? SvSTASH(SvRV(class)) /* a reference to a class */
    : gv_stashsv(class, 0); /* a class name */
  pdl *n = pdl_pdlnew();
  if (!n) pdl_pdl_barf("Error making null pdl");
  RETVAL = newSV(0);
  pdl_SetSV_PDL(RETVAL,n);   /* set a null PDL to this SV * */
  RETVAL = sv_bless(RETVAL, bless_stash); /* bless appropriately  */
  /* set the datasv to what was supplied */
  n->datasv = (void*)datasv_pointer;
  SvREFCNT_inc((SV*)(datasv_pointer));
  n->data = SvPV_nolen((SV*)datasv_pointer) + offset;
  n->nbytes = sv_len - offset;
  n->state |= PDL_ALLOCATED;
OUTPUT:
  RETVAL

SV *
get_dataref(self)
	pdl *self
	CODE:
	if(self->state & PDL_DONTTOUCHDATA)
	  croak("Trying to get dataref to magical (mmaped?) pdl");
	PDLDEBUG_f(printf("get_dataref %p\n", self));
	pdl_barf_if_error(pdl_make_physical(self));
	if (!self->datasv) {
	  PDLDEBUG_f(printf("get_dataref no datasv\n"));
	  self->datasv = newSVpvn("", 0);
	  (void)SvGROW((SV *)self->datasv, self->nbytes);
	  SvCUR_set((SV *)self->datasv, self->nbytes);
	  memmove(SvPV_nolen((SV*)self->datasv), self->data, self->nbytes);
	}
	RETVAL = newRV(self->datasv);
	PDLDEBUG_f(printf("get_dataref end: "); pdl_dump(self));
	OUTPUT:
	RETVAL

void
upd_data(self, keep_datasv=0)
	pdl *self
	IV keep_datasv
	CODE:
	if(self->state & PDL_DONTTOUCHDATA)
	  croak("Trying to touch dataref of magical (mmaped?) pdl");
	PDLDEBUG_f(printf("upd_data: "); pdl_dump(self));
	if (keep_datasv || !PDL_USESTRUCTVALUE(self)) {
	  self->data = SvPV_nolen((SV*)self->datasv);
	} else if (self->datasv) {
	  PDLDEBUG_f(printf("upd_data zap datasv\n"));
	  memmove(self->data, SvPV_nolen((SV*)self->datasv), self->nbytes);
	  SvREFCNT_dec(self->datasv);
	  self->datasv = NULL;
	} else {
	  PDLDEBUG_f(printf("upd_data datasv gone, maybe reshaped\n"));
	}
	PDLDEBUG_f(printf("upd_data end: "); pdl_dump(self));

int
badflag(x,newval=0)
    pdl *x
    int newval
  CODE:
    if (items>1)
	pdl_propagate_badflag( x, newval );
    RETVAL = ((x->state & PDL_BADVAL) > 0);
  OUTPUT:
    RETVAL

PDL_Indx
getndims(x)
	pdl *x
	ALIAS:
	     PDL::ndims = 1
	CODE:
		(void)ix;
		pdl_barf_if_error(pdl_make_physdims(x));
		RETVAL = x->ndims;
	OUTPUT:
		RETVAL

void
dims(x)
	pdl *x
	PREINIT:
		PDL_Indx i;
		U8 gimme = GIMME_V;
	PPCODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		if (gimme == G_ARRAY) {
			EXTEND(SP, x->ndims);
			for(i=0; i<x->ndims; i++) mPUSHi(x->dims[i]);
		}
		else if (gimme == G_SCALAR) {
			mXPUSHu(x->ndims);
		}

PDL_Indx
getdim(x,y)
	pdl *x
	PDL_Indx y
	ALIAS:
	     PDL::dim = 1
	CODE:
		(void)ix;
		pdl_barf_if_error(pdl_make_physdims(x));
		if (y < 0) y += x->ndims;
		if (y < 0) croak("negative dim index too large");
		RETVAL = y < x->ndims ? x->dims[y] : 1; /* all other dims=1 */
	OUTPUT:
		RETVAL

PDL_Indx
getnbroadcastids(x)
	pdl *x
	CODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		RETVAL = x->nbroadcastids;
	OUTPUT:
		RETVAL

void
broadcastids(x)
	pdl *x
	PREINIT:
		PDL_Indx i;
		U8 gimme = GIMME_V;
	PPCODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		if (gimme == G_ARRAY) {
			EXTEND(SP, x->nbroadcastids);
			for(i=0; i<x->nbroadcastids; i++) mPUSHi(x->broadcastids[i]);
		}
		else if (gimme == G_SCALAR) {
			mXPUSHu(x->nbroadcastids);
		}

PDL_Indx
getbroadcastid(x,y)
	pdl *x
	PDL_Indx y
	CODE:
		if (y < 0 || y >= x->nbroadcastids) barf("requested invalid broadcastid %"IND_FLAG", nbroadcastids=%"IND_FLAG, y, x->nbroadcastids);
		RETVAL = x->broadcastids[y];
	OUTPUT:
		RETVAL

void
setdims(x,dims)
	pdl *x
	PDL_Indx dims_count=0;
	PDL_Indx *dims
	CODE:
		pdl_barf_if_error(pdl_setdims(x,dims,dims_count));

void
dowhenidle()
	CODE:
		pdl_run_delayed_magic();
		XSRETURN(0);

void
bind(p,c)
	pdl *p
	SV *c
	PROTOTYPE: $&
	CODE:
		if (!pdl_add_svmagic(p,c)) croak("Failed to add magic");
		XSRETURN(0);

void
sethdr(p,h)
	pdl *p
	SV *h
      PREINIT:
	CODE:
		if(p->hdrsv == NULL) {
		      p->hdrsv =  &PL_sv_undef; /*(void*) newSViv(0);*/
		}

		/* Throw an error if we're not either undef or hash */
                if ( (h != &PL_sv_undef && h != NULL) &&
		     ( !SvROK(h) || SvTYPE(SvRV(h)) != SVt_PVHV )
		   )
		      croak("Not a HASH reference");

		/* Clear the old header */
		SvREFCNT_dec(p->hdrsv);

		/* Put the new header (or undef) in place */
		if(h == &PL_sv_undef || h == NULL)
		   p->hdrsv = NULL;
		else
		   p->hdrsv = (void*) newRV( (SV*) SvRV(h) );

SV *
hdr(p)
	pdl *p
CODE:
  pdl_barf_if_error(pdl_make_physdims(p));
  /* Make sure that in the undef case we return not */
  /* undef but an empty hash ref. */
  if((p->hdrsv==NULL) || (p->hdrsv == &PL_sv_undef)) {
    p->hdrsv = (void*) newRV_noinc( (SV*)newHV() );
  }
  RETVAL = newRV( (SV*) SvRV((SV*)p->hdrsv) );
OUTPUT:
  RETVAL

SV *
gethdr(p)
  pdl *p
CODE:
  pdl_barf_if_error(pdl_make_physdims(p));
  if((p->hdrsv==NULL) || (p->hdrsv == &PL_sv_undef)) {
      RETVAL = &PL_sv_undef;
  } else {
      RETVAL = newRV( (SV*) SvRV((SV*)p->hdrsv) );
  }
OUTPUT:
  RETVAL

SV *
unpdl(x)
  pdl *x
CODE:
  RETVAL = pdl2avref(x, 0);
OUTPUT:
  RETVAL

void
dog(x, opt=sv_2mortal(newRV_noinc((SV *)newHV())))
  pdl *x
  SV *opt
PPCODE:
  HV *opt_hv = NULL;
  if (!(SvROK(opt) && SvTYPE(opt_hv = (HV*)SvRV(opt)) == SVt_PVHV))
    barf("Usage: $pdl->dog([\\%%opt])");
  pdl_barf_if_error(pdl_make_physdims(x));
  if (x->ndims <= 0) barf("dog: must have at least one dim");
  SV **svp = hv_fetchs(opt_hv, "Break", 0);
  char dobreak = (svp && *svp && SvOK(*svp));
  PDL_Indx *thesedims = x->dims, *theseincs = PDL_REPRINCS(x), ndimsm1 = x->ndims-1;
  PDL_Indx i, howmany = x->dims[ndimsm1], thisoffs = 0, topinc = x->dimincs[ndimsm1];
  EXTEND(SP, howmany);
  for (i = 0; i < howmany; i++, thisoffs += topinc) {
    pdl *childpdl = pdl_pdlnew();
    if (!childpdl) pdl_pdl_barf("Error making null pdl");
    pdl_barf_if_error(pdl_affine_new(x,childpdl,thisoffs,
      thesedims,ndimsm1,theseincs,ndimsm1));
    SV *childsv = sv_newmortal();
    pdl_SetSV_PDL(childsv, childpdl); /* do before sever so .sv true */
    if (dobreak) pdl_barf_if_error(pdl_sever(childpdl));
    PUSHs(childsv);
  }
  XSRETURN(howmany);

void
broadcastover_n(code, pdl1, ...)
    SV *code;
    pdl *pdl1;
   CODE:
    PDL_Indx npdls = items - 1;
    PDL_Indx i,sd;
    pdl *pdls[npdls];
    PDL_Indx realdims[npdls];
    pdl_broadcast pdl_brc;
    pdls[0] = pdl1;
    for(i=1; i<npdls; i++)
	pdls[i] = pdl_SvPDLV(ST(i+1));
    for(i=0; i<npdls; i++) {
	pdl_barf_if_error(pdl_make_physical(pdls[i]));
	realdims[i] = 0;
    }
    PDL_CLRMAGIC(&pdl_brc);
    pdl_brc.gflags = 0; /* avoid uninitialised value use below */
    pdl_barf_if_error(pdl_initbroadcaststruct(0,pdls,realdims,realdims,npdls,NULL,&pdl_brc,NULL,NULL,NULL, 1));
    pdl_error error_ret = {0, NULL, 0};
    if (pdl_startbroadcastloop(&pdl_brc,NULL,NULL,&error_ret) < 0) croak("Error starting broadcastloop");
    pdl_barf_if_error(error_ret);
    sd = pdl_brc.ndims;
    ENTER; SAVETMPS;
    do {
	dSP;
	PUSHMARK(SP);
	EXTEND(SP,items);
	PUSHs(sv_2mortal(newSViv((sd-1))));
	for(i=0; i<npdls; i++) {
            PDL_Anyval anyval = { PDL_INVALID, {0} };
            ANYVAL_FROM_CTYPE_OFFSET(anyval, pdls[i]->datatype, PDL_REPRP(pdls[i]), pdl_brc.offs[i]);
            if (anyval.type < 0) die("Error getting value from ndarray");
            SV *sv = sv_newmortal();
            ANYVAL_TO_SV(sv, anyval);
            PUSHs(sv);
	}
	PUTBACK;
	perl_call_sv(code,G_DISCARD);
	sd = pdl_iterbroadcastloop(&pdl_brc,0);
	if ( sd < 0 ) die("Error in iterbroadcastloop");
    } while( sd );
    FREETMPS; LEAVE;
    pdl_freebroadcaststruct(&pdl_brc);

void
broadcastover(code, realdims, creating, nothers, pdl1, ...)
    PDL_Indx realdims_count=0;
    PDL_Indx creating_count=0;
    SV *code;
    PDL_Indx *realdims;
    PDL_Indx *creating;
    int nothers;
    pdl *pdl1;
   CODE:
    int targs = items - 4;
    if(nothers < 0 || nothers >= targs)
	croak("Usage: broadcastover(sub,realdims,creating,nothers,pdl1[,pdl...][,otherpars..])");
    PDL_Indx npdls = targs-nothers, i,nc=npdls;
    pdl *pdls[npdls], *child[npdls];
    SV *csv[npdls], *others[nothers];
    if (creating_count < npdls) croak("broadcastover: need at least one creating flag per pdl: %"IND_FLAG" pdls, %"IND_FLAG" flags", npdls, creating_count);
    if (realdims_count != npdls) croak("broadcastover: need one realdim flag per pdl: %"IND_FLAG" pdls, %"IND_FLAG" flags", npdls, realdims_count);
    int dtype=0;
    pdls[0] = pdl1;
    for(i=1; i<npdls; i++)
	pdls[i] = pdl_SvPDLV(ST(i+4));
    for(i=0; i<npdls; i++) {
	if (creating[i])
	  nc += realdims[i];
	else {
	  pdl_barf_if_error(pdl_make_physvaffine(pdls[i]));
	  dtype = PDLMAX(dtype,pdls[i]->datatype);
	}
    }
    if (creating_count < nc)
	croak("Not enough dimension info to create pdls");
    for (i=npdls; i<=targs; i++)
	others[i-npdls] = ST(i+4);
    PDLDEBUG_f(for (i=0;i<npdls;i++) { printf("pdl %"IND_FLAG" ",i); pdl_dump(pdls[i]); });
    pdl_broadcast pdl_brc;
    PDL_CLRMAGIC(&pdl_brc);
    pdl_brc.gflags = 0; /* avoid uninitialised value use below */
    pdl_barf_if_error(pdl_initbroadcaststruct(0,pdls,realdims,creating,npdls,
			NULL,&pdl_brc,NULL,NULL,NULL, 1));
    for(i=0, nc=npdls; i<npdls; i++)  /* create as necessary */
      if (creating[i]) {
	PDL_Indx *cp = creating+nc;
	pdls[i]->datatype = dtype;
	pdl_barf_if_error(pdl_broadcast_create_parameter(&pdl_brc,i,cp,0));
	nc += realdims[i];
	pdl_barf_if_error(pdl_make_physical(pdls[i]));
	PDLDEBUG_f(pdl_dump(pdls[i]));
	/* And make it nonnull, now that we've created it */
	pdls[i]->state &= (~PDL_NOMYDIMS);
      }
    pdl_error error_ret = {0, NULL, 0};
    if (pdl_startbroadcastloop(&pdl_brc,NULL,NULL,&error_ret) < 0) croak("Error starting broadcastloop");
    pdl_barf_if_error(error_ret);
    for(i=0; i<npdls; i++) {
	PDL_Indx *thesedims = pdls[i]->dims, *theseincs = PDL_REPRINCS(pdls[i]);
	/* need to make sure we get the vaffine (grand)parent */
	if (PDL_VAFFOK(pdls[i]))
	   pdls[i] = pdls[i]->vafftrans->from;
	child[i]=pdl_pdlnew();
	if (!child[i]) pdl_pdl_barf("Error making null pdl");
	pdl_barf_if_error(pdl_affine_new(pdls[i],child[i],pdl_brc.offs[i],
		thesedims,realdims[i],
		theseincs,realdims[i]));
	pdl_barf_if_error(pdl_make_physvaffine(child[i])); /* make sure we can
					get at the vafftrans */
	csv[i] = sv_newmortal();
	pdl_SetSV_PDL(csv[i], child[i]); /* pdl* into SV* */
    }
    int brcloopval;
    ENTER; SAVETMPS;
    do {  /* the actual broadcastloop */
	pdl_trans *traff;
	dSP;
	PUSHMARK(SP);
	EXTEND(SP,npdls+nothers);
	for(i=0; i<npdls; i++) {
	   /* just twiddle the offset - quick and dirty */
	   /* we must twiddle both !! */
	   traff = child[i]->trans_parent;
	   traff->offs = pdl_brc.offs[i];
	   child[i]->vafftrans->offs = pdl_brc.offs[i];
	   child[i]->state |= PDL_PARENTDATACHANGED;
	   PUSHs(csv[i]);
	}
	for (i=0; i<nothers; i++)
	  PUSHs(others[i]);   /* pass the OtherArgs onto the stack */
	PUTBACK;
	perl_call_sv(code,G_DISCARD);
	brcloopval = pdl_iterbroadcastloop(&pdl_brc,0);
	if ( brcloopval < 0 ) die("Error in iterbroadcastloop");
    } while( brcloopval );
    FREETMPS; LEAVE;
    pdl_freebroadcaststruct(&pdl_brc);
