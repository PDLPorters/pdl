
#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

static SV *getref_pdl(pdl *it) {
	SV *newref;
	if(!it->sv) {
		SV *ref;
		HV *stash = gv_stashpv("PDL",TRUE);
		SV *psv = newSViv((IV)it);
		it->sv = psv;
		newref = newRV_noinc(it->sv);
		(void)sv_bless(newref,stash);
	} else {
		newref = newRV_inc(it->sv);
		SvAMAGIC_on(newref);
	}
	return newref;
}

void SetSV_PDL ( SV *sv, pdl *it ) {
	SV *newref = getref_pdl(it); /* YUCK!!!! */
	sv_setsv(sv,newref);
	SvREFCNT_dec(newref);
}


/* Size of data type information */

int pdl_howbig (int datatype) {
    switch (datatype) {
    case PDL_B:
      return sizeof(PDL_Byte);
    case PDL_S:
      return sizeof(PDL_Short);
    case PDL_US:
      return sizeof(PDL_Ushort);
    case PDL_L:
      return sizeof(PDL_Long);
    case PDL_F:
      return sizeof(PDL_Float);
    case PDL_D:
      return sizeof(PDL_Double);
    default:
      croak("Unknown datatype code = %d",datatype);
    }
}

/* Check minimum datatype required to represent number */

#define TESTTYPE(b,a) {a foo = nv; if(nv == foo) return b;}
int pdl_whichdatatype (double nv) {
	TESTTYPE(PDL_B,PDL_Byte)
	TESTTYPE(PDL_S,PDL_Short)
	TESTTYPE(PDL_US,PDL_Ushort)
	TESTTYPE(PDL_L,PDL_Long)
	TESTTYPE(PDL_F,PDL_Float)
	TESTTYPE(PDL_D,PDL_Double)
	croak("Something's gone wrong: %lf cannot be converted by whichdatatype",
		nv);
}

/* Check minimum, at least float, datatype required to represent number */

int pdl_whichdatatype_double (double nv) {
	TESTTYPE(PDL_F,PDL_Float)
	TESTTYPE(PDL_D,PDL_Double)
	croak("Something's gone wrong: %lf cannot be converted by whichdatatype_double",
		nv);
}
/* Make a scratch data existence for a pdl */

void pdl_makescratchhash(pdl *ret,double data, int datatype) {
    STRLEN n_a;
	HV *hash;
	SV *dat; PDL_Long fake[1];

	 /* Compress to smallest available type. This may have strange
	    results sometimes :( */
	ret->datatype = datatype;
	ret->data = pdl_malloc(pdl_howbig(ret->datatype)); /* Wasteful */

       dat = newSVpv(ret->data,pdl_howbig(ret->datatype));

       ret->data = SvPV(dat,n_a);
       ret->datasv = dat;
#ifdef FOO
 /* Refcnt should be 1 already... */
       SvREFCNT_inc(ret->datasv); /* XXX MEMLEAK */
#endif

  /* This is an important point: it makes this whole piddle mortal
   * so destruction will happen at the right time.
   * If there are dangling references, pdlapi.c knows not to actually
   * destroy the C struct. */
       sv_2mortal(getref_pdl(ret));

       pdl_setdims(ret, fake, 0); /* However, there are 0 dims in scalar */
       ret->nvals = 1;

       /* NULLs should be ok because no dimensions. */
       pdl_set(ret->data, ret->datatype, NULL, NULL, NULL, 0, 0, data);

}

/*
  "Convert" a perl SV into a pdl (alright more like a mapping as
   the data block isn't actually copied)  - scalars are automatically
   converted
*/

pdl* SvPDLV ( SV* sv ) {

   pdl* ret;
   int fake[1];
   SV *sv2;

   if ( !SvROK(sv) ) {   /* Coerce scalar */
      SV *dat;
      double data;
      int datatype;

       ret = pdl_new();  /* Scratch pdl */

/*       ret->sv = (void*) sv; !! */

/* Scratch hash for the pdl :( - slow but safest. */

       /* Figure datatype to use */

       if ( !SvIOK(sv) && SvNOK(sv) && SvNIOK(sv)  )  {/* Perl Double (e.g. 2.0) */
          data = SvNV(sv);
          datatype = pdl_whichdatatype_double(data);
	  }
       else { /* Perl Int (e.g. 2) */
          data = SvNV(sv);
          datatype = pdl_whichdatatype(data);
       }
       pdl_makescratchhash(ret,data,datatype);

       return ret;
   }

#undef FOODEB
#ifdef FOODEB
	printf("SvPDLV\n");
	printf("SV: %d\n",sv);
	printf("SvRV: %d\n",SvRV(sv));
	printf("SvTYPE: %d\n",SvTYPE(SvRV(sv)));
#endif

   if(SvTYPE(SvRV(sv)) == SVt_PVHV) {
   	HV *hash = (HV*)SvRV(sv);
	SV **svp = hv_fetch(hash,"PDL",3,0);
	if(svp == NULL) {
		croak("Hash given as a pdl - but not {PDL} key!");
	}
	if(*svp == NULL) {
		croak("Hash given as a pdl - but not {PDL} key (*svp)!");
	}

	/* This is the magic hook which checks to see if {PDL}
	is a code ref, and if so executes it. It should
	return a standard piddle. This allows
	all kinds of funky objects to be derived from PDL,
	and allow normal PDL functions to still work so long
	as the {PDL} code returns a standard piddle on
	demand - KGB */

	if (SvROK(*svp) && SvTYPE(SvRV(*svp)) == SVt_PVCV) {
	   dSP;
	   int count;
	   ENTER ;
	   SAVETMPS;
	   PUSHMARK(sp) ;
	   count = perl_call_sv(*svp, G_SCALAR|G_NOARGS);
	   SPAGAIN ;
	   if (count != 1)
              croak("Execution of PDL structure failed to return one value\n") ;

	   sv=newSVsv(POPs);
	   PUTBACK ;
	   FREETMPS ;
	   LEAVE ;
	}
	else {
   	   sv = *svp;
	}
#ifdef FOODEB
	printf("SvPDLV2\n");
	printf("SV2: %d\n",sv);
	printf("SvTYPE2: %d\n",SvTYPE(sv));
	printf("SvFLAGS2: %d\n",SvFLAGS(sv));
	printf("SvANY: %d\n",SvANY(sv));
#endif
	if(SvGMAGICAL(sv)) {
		mg_get(sv);
	}
#ifdef FOODEB
	printf("SvPDLV3\n");
	printf("SV3: %d\n",sv);
	printf("SvTYPE3: %d\n",SvTYPE(sv));
	printf("SvFLAGS3: %d\n",SvFLAGS(sv));
	printf("SvANY: %d\n",SvANY(sv));
#endif
        if ( !SvROK(sv) ) {   /* Got something from a hash but not a ref */
		croak("Hash given as pdl - but PDL key is not a ref!");
        }
#ifdef FOODEB
	printf("SvRV2: %d\n",SvRV(sv));
	printf("SvTYPE2: %d\n",SvTYPE(SvRV(sv)));
#endif
   }


   if (SvTYPE(SvRV(sv)) != SVt_PVMG)
      croak("Error - argument is not a recognised data structure");

   sv2 = (SV*) SvRV(sv);

#ifdef FOIJFSOEFJSEOFJSOEIJFOISJFEFSF
   /* Now, do magic: check if there are more than this one ref
      to this internal sv. If there are, we've been "="'ed
      (assigned) elsewhere and therefore must copy to keep
      the semantics clear. This may at the moment be slightly
      inefficient but as a future optimization, SvPDLV may be replaced
      by SvPDLV_nodup in places where it is sure that this is ok. */

   if(SvREFCNT(sv2) > 1) {
   	pdl *tmp = (pdl *)SvIV(sv2);
	pdl *pnew = pdl_hard_copy(tmp);
   	printf("More than one ref; copying\n");

	SetSV_PDL(sv,pnew);
	ret = pnew;
   } else {
#else
	   ret = (pdl *)SvIV(sv2);
#endif
#ifdef FOOOOOOOO
   }
#endif

   if(ret->magicno != PDL_MAGICNO) {
   	croak("Fatal error: argument is probably not a piddle, or\
 magic no overwritten. You're in trouble, guv: %d %d %d\n",sv2,ret,ret->magicno);
   }

   return ret;
}

/* Make a new pdl object as a copy of an old one and return - implement by
   callback to perl method "copy" or "new" (for scalar upgrade) */

SV* pdl_copy( pdl* a, char* option ) {

   SV* retval;
   char meth[20];

   dSP ;   int count ;

   retval = newSVpv("",0); /* Create the new SV */

   ENTER ;   SAVETMPS ;   PUSHMARK(sp) ;

   /* Push arguments */

#ifdef FOOBAR
   if (sv_isobject((SV*)a->hash)) {
#endif
       XPUSHs(sv_2mortal(getref_pdl(a)));
       strcpy(meth,"copy");
       XPUSHs(sv_2mortal(newSVpv(option, 0))) ;
#ifdef FOOBAR
   }
   else{
       XPUSHs(perl_get_sv("PDL::name",FALSE)); /* Default object */
       XPUSHs(sv_2mortal(getref_pdl(a)));
       strcpy(meth,"new");
   }
#endif

   PUTBACK ;

   count = perl_call_method(meth, G_SCALAR); /* Call Perl */

   SPAGAIN;

   if (count !=1)
      croak("Error calling perl function\n");

   sv_setsv( retval, POPs ); /* Save the perl returned value */

   PUTBACK ;   FREETMPS ;   LEAVE ;

   return retval;
}



/* Pack dims array - returns dims[] (pdl_malloced) and ndims */

PDL_Long* pdl_packdims ( SV* sv, int *ndims ) {

   SV*  bar;
   AV*  array;
   int i;
   PDL_Long *dims;

   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;

   array = (AV *) SvRV(sv);   /* dereference */

   *ndims = (int) av_len(array) + 1;  /* Number of dimensions */

   dims = (PDL_Long *) pdl_malloc( (*ndims) * sizeof(*dims) ); /* Array space */
   if (dims == NULL)
      croak("Out of memory");

   for(i=0; i<(*ndims); i++) {
      bar = *(av_fetch( array, i, 0 )); /* Fetch */
      dims[i] = (int) SvIV(bar);
   }
   return dims;
}

/* unpack dims array into PDL SV* */

void pdl_unpackdims ( SV* sv, PDL_Long *dims, int ndims ) {

   AV*  array;
   HV* hash;
   int i;

   hash = (HV*) SvRV( sv );
   array = newAV();
   hv_store(hash, "Dims", strlen("Dims"), newRV( (SV*) array), 0 );

   if (ndims==0 )
      return;

   for(i=0; i<ndims; i++)
         av_store( array, i, newSViv( (IV)dims[i] ) );
}

PDL_Long pdl_safe_indterm( PDL_Long dsz, PDL_Long at, char *file, int lineno)
{
  if (at >= 0 && at < dsz) return at;
  pdl_barf("access [%d] out of range [0..%d] (inclusive) at %s line %d",
          at, dsz-1, file?file:"?", lineno);
}

/*
   pdl_malloc - utility to get temporary memory space. Uses
   a mortal *SV for this so it is automatically freed when the current
   context is terminated without having to call free(). Naughty but
   nice!
*/


void* pdl_malloc ( int nbytes ) {
    STRLEN n_a;
   SV* work;

   work = sv_2mortal(newSVpv("", 0));

   SvGROW( work, nbytes);

   return (void *) SvPV(work, n_a);
}

/*********** Stuff for barfing *************/

/* Version of perl's mess_alloc - we need this copy
here because it is defined static in util.c! */

static SV *
pdl_mess_alloc()
{
    SV *sv;
    XPVMG *any;

    /* Create as PVMG now, to avoid any upgrading later */
    New(905, sv, 1, SV);
    Newz(905, any, 1, XPVMG);
    SvFLAGS(sv) = SVt_PVMG;
    SvANY(sv) = (void*)any;
    SvREFCNT(sv) = 1 << 30; /* practically infinite */
    return sv;
}

/* Version of perl's mess() constructor which doesn't
do the automatic appending of stuff when "\n" not
seen at the end of the string - for use by pdl_barf() */

/* work araound an omission in the CAPI */
#ifdef PERL_CAPI
#undef PL_mess_sv
static SV *PDL_mess_sv = NULL;
#define PL_mess_sv PDL_mess_sv
#endif

char *
pdl_mess(pat, args)
    const char *pat;
    va_list *args;
{
    SV *sv;

    if (!PL_mess_sv)
	PL_mess_sv = pdl_mess_alloc();
    sv = PL_mess_sv;
    sv_vsetpvfn(sv, pat, strlen(pat), args, Null(SV**), 0, Null(bool*));
    {
	/* call the PDL message enhancing routine */
	ENTER;
	LEAVE;
        {
	    dSP;
	    SV *msg;

	    ENTER;

	    PUSHMARK(sp);
	    XPUSHs(sv);
	    PUTBACK;
	    perl_call_pv("PDL::Core::barf_msg", G_SCALAR);
            sv = POPs;
	    LEAVE;
	}
    }
    return SvPVX(sv);
}

/*
   pdl_barf - warning routine to be called when PDL routine
   croak

   Note: we call back in to Perl as error reporting is
   not time critical, and this gives us lots of flexibility.

   code stolen from croak() in util.c in Perl distribution
*/

#ifdef I_STDARG
void
pdl_barf(const char* pat, ...)
#else
/*VARARGS0*/
void
pdl_barf(pat, va_alist)
    char *pat;
    va_dcl
#endif
{
    va_list args;
    char *message;
    HV *stash;
    GV *gv;
    CV *cv;
    dTHR;
#ifdef I_STDARG
    va_start(args, pat);
#else
    va_start(args);
#endif
    message = pdl_mess(pat, &args);
    va_end(args);

    if (PL_diehook) {
	/* sv_2cv might call croak() */
       SV *olddiehook = PL_diehook;
	ENTER;
       SAVESPTR(PL_diehook);
       PL_diehook = Nullsv;
	cv = sv_2cv(olddiehook, &stash, &gv, 0);
	LEAVE;
	if (cv && !CvDEPTH(cv) && (CvROOT(cv) || CvXSUB(cv))) {
	    dSP;
	    SV *msg;

	    ENTER;
	    msg = newSVpv(message, 0);
	    SvREADONLY_on(msg);
	    SAVEFREESV(msg);

	    PUSHMARK(sp);
	    XPUSHs(msg);
	    PUTBACK;
	    perl_call_sv((SV*)cv, G_DISCARD);

	    LEAVE;
	}
    }
    if (PL_in_eval) {
#if (PERL_VERSION > 5) || ((PERL_VERSION >= 5) && (PERL_SUBVERSION >= 57))
       PL_restartop = die_where(message,strlen(message));
#else
        PL_restartop = die_where(message);
#endif
	JMPENV_JUMP(3);
    }
    PerlIO_puts(PerlIO_stderr(),message);
    (void)PerlIO_flush(PerlIO_stderr());
    my_failure_exit();
}
