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
      pdl_SetSV_PDL(sv, trans->pdls[i]); \
      PUSHs(sv); \
    }

#define setflag(reg,flagval,val) (val?(reg |= flagval):(reg &= ~flagval))

Core PDL; /* Struct holding pointers to shared C routines */

int pdl_debugging=0;
int pdl_autopthread_targ   = 0; /* No auto-pthreading unless set using the set_autopthread_targ */
int pdl_autopthread_actual = 0;
PDL_Indx pdl_autopthread_dim = -1;
int pdl_autopthread_size   = 1;

MODULE = PDL::Core     PACKAGE = PDL

# Destroy a PDL - note if a hash do nothing, the $$x{PDL} component
# will be destroyed anyway on a separate call

void
DESTROY(sv)
  SV *	sv;
  PREINIT:
    pdl *self;
  CODE:
    if (  !(  (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVHV) )  ) {
       self = pdl_SvPDLV(sv);
       PDLDEBUG_f(printf("DESTROYING %p\n",(void*)self);)
       if (self != NULL)
          pdl_barf_if_error(pdl_destroy(self));
    }

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

void
set_inplace(self,val)
  pdl *self;
  int val;
  CODE:
    setflag(self->state,PDL_INPLACE,val);

IV
address(self)
  pdl *self;
  CODE:
    RETVAL = PTR2IV(self);
  OUTPUT:
    RETVAL

int
set_donttouchdata(it)
      pdl *it
      CODE:
            it->state |= PDL_DONTTOUCHDATA | PDL_ALLOCATED;
            RETVAL = 1;
      OUTPUT:
            RETVAL

# Free the datasv if possible
void
freedata(it)
      pdl *it
      CODE:
	if(it->datasv) {
		SvREFCNT_dec(it->datasv);
		it->datasv=0;
		it->data=0;
	} else if(it->data) {
		die("Trying to free data of pdl with data != 0 and datasv==0");
	}

int
set_data_by_offset(it,orig,offset)
      pdl *it
      pdl *orig
      STRLEN offset
      CODE:
              it->data = ((char *) orig->data) + offset;
	      it->datasv = orig->sv;
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
  PDL_XS_SCALAR(PDL_CD, 0 + 1I)

void
_nan(...)
 PPCODE:
  PDL_XS_SCALAR(PDL_D, (PDL_Double)NAN)

void
_inf(...)
 PPCODE:
  PDL_XS_SCALAR(PDL_D, INFINITY)

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
sclr_c(it)
   pdl* it
   PREINIT:
	PDL_Anyval result = { PDL_INVALID, {0} };
   CODE:
        /* get the first element of an ndarray and return as
         * Perl scalar (autodetect suitable type IV or NV)
         */
        result = pdl_at0(it);
        if (result.type < 0) croak("Position out of range");
        ANYVAL_TO_SV(RETVAL, result);

    OUTPUT:
        RETVAL


SV *
at_bad_c(x,pos)
   pdl*	x
   PDL_Indx *pos
   PREINIT:
    PDL_Indx ipos;
    int badflag;
    volatile PDL_Anyval result = { PDL_INVALID, {0} };
   CODE:
    pdl_barf_if_error(pdl_make_physvaffine( x ));

    if (pos == NULL || pos_count < x->ndims)
       barf("Invalid position with pos=%p, count=%"IND_FLAG" for ndarray with %"IND_FLAG" dims", pos, pos_count, x->ndims);

    /*  allow additional trailing indices
     *  which must be all zero, i.e. a
     *  [3,1,5] ndarray is treated as an [3,1,5,1,1,1,....]
     *  infinite dim ndarray
     */
    for (ipos=x->ndims; ipos<pos_count; ipos++)
      if (pos[ipos] != 0)
         barf("Invalid position %"IND_FLAG" at dimension %"IND_FLAG, pos[ipos], ipos);

    result=pdl_at(PDL_REPRP(x), x->datatype, pos, x->dims,
        PDL_REPRINCS(x), PDL_REPROFFS(x),
	x->ndims);
    if (result.type < 0) barf("Position %"IND_FLAG" out of range", pos);
   badflag = (x->state & PDL_BADVAL) > 0;
   if (badflag) {
     volatile PDL_Anyval badval = pdl_get_pdl_badvalue(x);
     if (badval.type < 0) barf("Error getting badvalue, type=%d", badval.type);
     int isbad = ANYVAL_ISBAD(result, badval);
     if (isbad == -1) barf("ANYVAL_ISBAD error on types %d, %d", result.type, badval.type);
     if (isbad)
       RETVAL = newSVpvn( "BAD", 3 );
     else
       ANYVAL_TO_SV(RETVAL, result);
   } else
     ANYVAL_TO_SV(RETVAL, result);

    OUTPUT:
     RETVAL


# returns the string 'BAD' if an element is bad
#

SV *
listref_c(x)
   pdl *x
  PREINIT:
   PDL_Indx * incs;
   PDL_Indx offs;
   void *data;
   int ind;
   int lind;
   int stop = 0;
   AV *av;
   SV *sv;
   volatile PDL_Anyval pdl_val = { PDL_INVALID, {0} }; /* same reason as below */
   volatile PDL_Anyval pdl_badval = { PDL_INVALID, {0} };
  CODE:
    /*
    # note:
    #  the badvalue is stored in a PDL_Anyval, but that's what pdl_at()
    #  returns
    */

   int badflag = (x->state & PDL_BADVAL) > 0;
   if (badflag) {
      pdl_badval = pdl_get_pdl_badvalue( x );
      if (pdl_badval.type < 0) barf("Error getting badvalue, type=%d", pdl_badval.type);
   }

   pdl_barf_if_error(pdl_make_physvaffine( x ));
   data = PDL_REPRP(x);
   incs = PDL_REPRINCS(x);
   offs = PDL_REPROFFS(x);
   av = newAV();
   av_extend(av,x->nvals);
   lind=0;
   PDL_Indx inds[x->ndims];
   for(ind=0; ind < x->ndims; ind++) inds[ind] = 0;
   while(!stop) {
      pdl_val = pdl_at( data, x->datatype, inds, x->dims, incs, offs, x->ndims );
      if (pdl_val.type < 0) croak("Position out of range");
      if (badflag) {
	 /* volatile because gcc optimiser otherwise won't recalc for complex double when long-double code added */
	 volatile int isbad = ANYVAL_ISBAD(pdl_val, pdl_badval);
	 if (isbad == -1) croak("ANYVAL_ISBAD error on types %d, %d", pdl_val.type, pdl_badval.type);
	 if (isbad)
	    sv = newSVpvn( "BAD", 3 );
	 else
	    ANYVAL_TO_SV(sv, pdl_val);
      } else {
	 ANYVAL_TO_SV(sv, pdl_val);
      }
      av_store( av, lind, sv );

      lind++;
      stop = 1;
      for(ind = 0; ind < x->ndims; ind++) {
	 if(++(inds[ind]) >= x->dims[ind]) {
       	    inds[ind] = 0;
         } else {
       	    stop = 0; break;
         }
      }
   }
   RETVAL = newRV_noinc((SV *)av);
  OUTPUT:
   RETVAL

void
set_c(x,pos,value)
    pdl*	x
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
  PDL.bvals.shortctype = defbval;
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
	OUTPUT:
		RETVAL

void
set_datatype(a,datatype)
   pdl *a
   int datatype
   CODE:
     pdl_barf_if_error(pdl_set_datatype(a, datatype));

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
	int nthdim
	int nthreads
	CODE:
		pdl_barf_if_error(pdl_add_threading_magic(it,nthdim,nthreads));

void
pdl_remove_threading_magic(it)
	pdl *it
	CODE:
		pdl_barf_if_error(pdl_add_threading_magic(it,-1,-1));

MODULE = PDL::Core	PACKAGE = PDL

SV *
initialize(class)
	SV *class
        PREINIT:
        CODE:
        HV *bless_stash = SvROK(class)
          ? SvSTASH(SvRV(class)) /* a reference to a class */
          : gv_stashsv(class, 0); /* a class name */
        RETVAL = newSV(0);
        pdl *n = pdl_null();
        if (!n) pdl_pdl_barf("Error making null pdl");
        pdl_SetSV_PDL(RETVAL,n);   /* set a null PDL to this SV * */
        RETVAL = sv_bless(RETVAL, bless_stash); /* bless appropriately  */
        OUTPUT:
        RETVAL

SV *
get_dataref(self)
	pdl *self
	CODE:
	if(self->state & PDL_DONTTOUCHDATA) {
		croak("Trying to get dataref to magical (mmaped?) pdl");
	}
	pdl_barf_if_error(pdl_make_physical(self)); /* XXX IS THIS MEMLEAK WITHOUT MORTAL? */
	RETVAL = (newRV(self->datasv));
	OUTPUT:
	RETVAL

int
get_datatype(self)
	pdl *self
	CODE:
	RETVAL = self->datatype;
	OUTPUT:
	RETVAL

void
upd_data(self)
	pdl *self
      PREINIT:
	CODE:
	if(self->state & PDL_DONTTOUCHDATA) {
		croak("Trying to touch dataref of magical (mmaped?) pdl");
	}
       self->data = SvPV_nolen((SV*)self->datasv);

void
set_dataflow_f(self,value)
	pdl *self;
	int value;
	CODE:
	if(value)
		self->state |= PDL_DATAFLOW_F;
	else
		self->state &= ~PDL_DATAFLOW_F;

int
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
dims_c(x)
	pdl *x
	PREINIT:
		PDL_Indx i;
		U8 gimme = GIMME_V;
	PPCODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		if (gimme == G_ARRAY) {
			EXTEND(sp, x->ndims);
			for(i=0; i<x->ndims; i++) mPUSHi(x->dims[i]);
		}
		else if (gimme == G_SCALAR) {
			mXPUSHu(x->ndims);
		}

PDL_Indx
getdim(x,y)
	pdl *x
	int y
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

int
getnthreadids(x)
	pdl *x
	CODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		RETVAL = x->nthreadids;
	OUTPUT:
		RETVAL

void
threadids_c(x)
	pdl *x
	PREINIT:
		PDL_Indx i;
		U8 gimme = GIMME_V;
	PPCODE:
		pdl_barf_if_error(pdl_make_physdims(x));
		if (gimme == G_ARRAY) {
			EXTEND(sp, x->nthreadids);
			for(i=0; i<x->nthreadids; i++) mPUSHi(x->threadids[i]);
		}
		else if (gimme == G_SCALAR) {
			mXPUSHu(x->nthreadids);
		}

int
getthreadid(x,y)
	pdl *x
	int y
	CODE:
		RETVAL = x->threadids[y];
	OUTPUT:
		RETVAL

void
setdims(x,dims)
	pdl *x
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

void
threadover_n(...)
   PREINIT:
   int npdls;
   SV *sv;
   CODE:
    npdls = items - 1;
    if(npdls <= 0)
    	croak("Usage: threadover_n(pdl[,pdl...],sub)");
    int i,sd;
    pdl *pdls[npdls];
    PDL_Indx realdims[npdls];
    pdl_thread pdl_thr;
    SV *code = ST(items-1);
    for(i=0; i<npdls; i++) {
	pdls[i] = pdl_SvPDLV(ST(i));
	/* XXXXXXXX Bad */
	pdl_barf_if_error(pdl_make_physical(pdls[i]));
	realdims[i] = 0;
    }
    PDL_THR_CLRMAGIC(&pdl_thr);
    pdl_barf_if_error(pdl_initthreadstruct(0,pdls,realdims,realdims,npdls,NULL,&pdl_thr,NULL,NULL,NULL, 1));
    pdl_error error_ret = {0, NULL, 0};
    if (pdl_startthreadloop(&pdl_thr,NULL,NULL,&error_ret) < 0) croak("Error starting threadloop");
    pdl_barf_if_error(error_ret);
    sd = pdl_thr.ndims;
    do {
    	dSP;
	PUSHMARK(sp);
	EXTEND(sp,items);
	PUSHs(sv_2mortal(newSViv((sd-1))));
	for(i=0; i<npdls; i++) {
		PDL_Anyval pdl_val = { PDL_INVALID, {0} };
		pdl_val = pdl_get_offs(pdls[i],pdl_thr.offs[i]);
		ANYVAL_TO_SV(sv, pdl_val);
		PUSHs(sv_2mortal(sv));
	}
    	PUTBACK;
	perl_call_sv(code,G_DISCARD);
	sd = pdl_iterthreadloop(&pdl_thr,0);
	if ( sd < 0 ) die("Error in iterthreadloop");
    } while( sd );
    pdl_freethreadstruct(&pdl_thr);

void
threadover(...)
   PREINIT:
    int npdls;
    int targs;
    int nothers = -1;
   CODE:
    targs = items - 4;
    if (items > 0) nothers = SvIV(ST(0));
    if(targs <= 0 || nothers < 0 || nothers >= targs)
	croak("Usage: threadover(nothers,pdl[,pdl...][,otherpars..],realdims,creating,sub)");
    npdls = targs-nothers;
    int i,dtype=0;
    PDL_Indx nc=npdls,nd1,nd2;
    SV* rdimslist = ST(items-3);
    SV* cdimslist = ST(items-2);
    SV *code = ST(items-1);
    pdl_thread pdl_thr;
    pdl *pdls[npdls], *child[npdls];
    SV *csv[npdls], *others[nothers];
    PDL_Indx *creating = pdl_packdims(cdimslist,&nd2);
    if (!creating) croak("Failed to packdims for creating");
    PDL_Indx *realdims = pdl_packdims(rdimslist,&nd1);
    if (!realdims) croak("Failed to packdims for realdims");
    if (nd1 != npdls) croak("threadover: need one realdim flag per pdl!");
    if (nd2 != npdls) croak("threadover: need one creating flag per pdl!");
    for(i=0; i<npdls; i++) {
	pdls[i] = pdl_SvPDLV(ST(i+1));
	if (creating[i])
	  nc += realdims[i];
	else {
	  pdl_barf_if_error(pdl_make_physical(pdls[i])); /* is this what we want?XXX */
	  dtype = PDLMAX(dtype,pdls[i]->datatype);
	}
    }
    for (i=npdls+1; i<=targs; i++)
	others[i-npdls-1] = ST(i);
    if (nd2 < nc)
	croak("Not enough dimension info to create pdls");
    PDLDEBUG_f(for (i=0;i<npdls;i++) { printf("pdl %d ",i); pdl_dump(pdls[i]); });
    PDL_THR_CLRMAGIC(&pdl_thr);
    pdl_barf_if_error(pdl_initthreadstruct(0,pdls,realdims,creating,npdls,
			NULL,&pdl_thr,NULL,NULL,NULL, 1));
    for(i=0, nc=npdls; i<npdls; i++)  /* create as necessary */
      if (creating[i]) {
	PDL_Indx *cp = creating+nc;
	pdls[i]->datatype = dtype;
	pdl_barf_if_error(pdl_thread_create_parameter(&pdl_thr,i,cp,0));
	nc += realdims[i];
	pdl_barf_if_error(pdl_make_physical(pdls[i]));
	PDLDEBUG_f(pdl_dump(pdls[i]));
	/* And make it nonnull, now that we've created it */
	pdls[i]->state &= (~PDL_NOMYDIMS);
      }
    pdl_error error_ret = {0, NULL, 0};
    if (pdl_startthreadloop(&pdl_thr,NULL,NULL,&error_ret) < 0) croak("Error starting threadloop");
    pdl_barf_if_error(error_ret);
    for(i=0; i<npdls; i++) {
	PDL_Indx *thesedims = pdls[i]->dims, *theseincs = PDL_REPRINCS(pdls[i]);
	/* need to make sure we get the vaffine (grand)parent */
	if (PDL_VAFFOK(pdls[i]))
	   pdls[i] = pdls[i]->vafftrans->from;
	child[i]=pdl_null();
	if (!child[i]) pdl_pdl_barf("Error making null pdl");
	/*  instead of pdls[i] its vaffine parent !!!XXX */
	pdl_barf_if_error(pdl_affine_new(pdls[i],child[i],pdl_thr.offs[i],
		thesedims,realdims[i],
		theseincs,realdims[i]));
	pdl_barf_if_error(pdl_make_physical(child[i])); /* make sure we can get at
					the vafftrans          */
	csv[i] = sv_newmortal();
	pdl_SetSV_PDL(csv[i], child[i]); /* pdl* into SV* */
    }
    int thrloopval;
    do {  /* the actual threadloop */
	pdl_trans *traff;
    	dSP;
	PUSHMARK(sp);
	EXTEND(sp,npdls);
	for(i=0; i<npdls; i++) {
	   /* just twiddle the offset - quick and dirty */
	   /* we must twiddle both !! */
	   traff = child[i]->trans_parent;
	   traff->offs = pdl_thr.offs[i];
	   child[i]->vafftrans->offs = pdl_thr.offs[i];
	   child[i]->state |= PDL_PARENTDATACHANGED;
	   PUSHs(csv[i]);
	}
	for (i=0; i<nothers; i++)
	  PUSHs(others[i]);   /* pass the OtherArgs onto the stack */
    	PUTBACK;
	perl_call_sv(code,G_DISCARD);
	thrloopval = pdl_iterthreadloop(&pdl_thr,0);
	if ( thrloopval < 0 ) die("Error in iterthreadloop");
    } while( thrloopval );
    pdl_freethreadstruct(&pdl_thr);
