#
# Create pdlcore.c
# - needed for bad-value handling in whichdatatype
#


use strict;

use Config;
use File::Basename qw(&basename &dirname);

require 'Dev.pm'; PDL::Core::Dev->import;
use vars qw( %PDL_DATATYPES );

# check for bad value support
use vars qw( $bvalflag $usenan );
require "badsupport.p";
require 'Types.pm';
PDL::Types->import(':All');

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//
    if ($Config{'osname'} eq 'VMS' or
	$Config{'osname'} eq 'OS2');  # "case-forgiving"

if ( $bvalflag ) {
    print "Extracting $file (WITH bad value support)\n";
} else {
    print "Extracting $file (NO bad value support)\n";
}
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

print OUT <<"!WITH!SUBS!";

/* pdlcore.c - generated automatically by pdlcore.c.PL */
/*           - bad value support = $bvalflag */

!WITH!SUBS!
  ;


print OUT <<'!HEADER!'
#undef FOODEB

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

/*** Turn on definitions to print lots of fencepost information in the constructor ***/
//#define DEBUG_SETAV_TYPE  1
//#define DEBUG_KLUDGE_COPY 1

/***************
 * Paranoid check is commented out because SvPOK breaks some kinds of perl scalars.
 * (blessed scalars like '$#$foo' get set to zero in perl 5.8.x)
 */
/*   #define sv_undef(sv) (  (!(sv) || ((sv)==&PL_sv_undef) ) || !(SvNIOK(sv) || SvPOK(sv) || SvROK(sv)) )  */
#define sv_undef(sv)  ( (!(sv) || ((sv)==&PL_sv_undef)) || !(SvNIOK(sv) || (SvTYPE(sv)==SVt_PVMG) || SvPOK(sv) || SvROK(sv)))

!HEADER!
  ;

if($Config{cc} eq 'cl') {
  # _finite in CV++ 4.0
  print OUT <<'FOO';
#define finite _finite
#include <float.h>
FOO
;
}

my $finite_inc;
foreach my $inc ( qw/ math.h ieeefp.h / )
{
    if ( trylink ("finite: $inc", "#include <$inc>", 'finite(3.2);', '' ) ) {
	$finite_inc = $inc;
	last;
    }
}

if ( defined $finite_inc )
{
    print OUT "#include <$finite_inc>\n";
}
else
{
    print OUT <<'!NO!SUBS!'

#ifndef finite
#ifdef isfinite
#define finite isfinite
#else
#define finite(a) (((a) * 0) == (0))
#endif
#endif
!NO!SUBS!
}

print OUT <<'!NO!SUBS!'
static SV *getref_pdl(pdl *it) {
	SV *newref;
	if(!it->sv) {
		SV *ref;
		HV *stash = gv_stashpv("PDL",TRUE);
		SV *psv = newSViv(PTR2IV(it));
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

!NO!SUBS!
  ;
# generate the cases for the various types

for my $type (typesrtkeys()) {
   my ($sym,$ctype) = map {typefld($type,$_)} qw/sym ctype/;
   print OUT << "!WITH!SUBS!";
    case $sym:
      return sizeof($ctype);
!WITH!SUBS!
}

print OUT <<'!NO!SUBS!';

    default:
      croak("Unknown datatype code = %d",datatype);
    }
}

/* Check minimum datatype required to represent number */
/* Microsoft compilers do some unbelievable things - hence
   some #ifdef's inserted by Sisyphus */
#if defined _MSC_VER && _MSC_VER < 1400
#define TESTTYPE(b,a) {a foo = nv; a bar = foo; foo += 0; if(nv == bar) return b;}
#else
#define TESTTYPE(b,a) {a foo = nv; if(nv == foo) return b;}
#endif
#if defined _MSC_VER && _MSC_VER < 1400
#pragma optimize("", off)
#endif
int pdl_whichdatatype (double nv) {
!NO!SUBS!

# generate the cases for the various types

for my $type (typesrtkeys()) {
   my ($sym,$ctype) = map {typefld($type,$_)} qw/sym ctype/;
   print OUT << "!WITH!SUBS!";
	TESTTYPE($sym,$ctype)
!WITH!SUBS!
}

print OUT <<'!NO!SUBS!';

        if( !finite(nv) ) { return PDL_D; }

	croak("Something's gone wrong: %lf cannot be converted by whichdatatype",
		nv);
}

/* Check minimum, at least float, datatype required to represent number */

int pdl_whichdatatype_double (double nv) {
	TESTTYPE(PDL_F,PDL_Float)
	TESTTYPE(PDL_D,PDL_Double)

        if( !finite(nv) ) { return PDL_D; }

	croak("Something's gone wrong: %lf cannot be converted by whichdatatype_double",
		nv);
}

#if defined _MSC_VER && _MSC_VER < 1400
#pragma optimize("", on)
#endif

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
   the data block is not actually copied)  - scalars are automatically
   converted.
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

/* Scratch hash for the pdl :( - slow but safest. */

	/* handle undefined values */
        if( sv_undef(sv) ) {
            sv = get_sv("PDL::undefval",1);
	    if(SvIV(get_sv("PDL::debug",1))){
		fprintf(stderr,"Warning: SvPDLV converted undef to $PDL::undefval (%g).\n",SvNV(sv));
		}
	}

       /* Figure datatype to use */
       if ( !SvIOK(sv) && SvNOK(sv) && SvNIOK(sv)  )  {/* Perl Double (e.g. 2.0) */
          data = SvNV(sv);

!NO!SUBS!

# XXX HACK this may not be sensible (DJB 08/31/00)
# - only relevant if BADVAL_USENAN is true in config file
#

if ( $bvalflag and $usenan ) {
   print OUT <<'!NO!SUBS!';

          /*
           * default NaN/Infs to double
           * XXX sensible ?
           */
          if ( finite(data) == 0 ) {
             datatype = PDL_D;
          } else {
             datatype = pdl_whichdatatype_double(data);
          }

!NO!SUBS!
} else {

   print OUT "\tdatatype = pdl_whichdatatype_double(data);\n";

} # if: $bvalflag

print OUT <<'!NO!SUBS!';

	  }
       else { /* Perl Int (e.g. 2) */
          data = SvNV(sv);
          datatype = pdl_whichdatatype(data);
       }
       pdl_makescratchhash(ret,data,datatype);

       return ret;
   } /* End of scalar case */


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
      croak("Error - tried to use an unknown data structure as a PDL");
    else if( !( sv_derived_from( sv, "PDL") ) )
      croak("Error - tried to use an unknown Perl object type as a PDL");

   sv2 = (SV*) SvRV(sv);


/* The below "CRUFTY" code is, I believe, intended to make (e.g.)
 * "$a = $b" copy the underlying PDL for $b, rather than simply
 * generating a reference to the same PDL.  It is commented out because
 * dataflow is the current default (you have to ask for a copy explicitly,
 * if you want one).
 *   --CED 16-Jan-2005
 */
#ifdef OLD_CRUFTY_CODE_FOR_ASSIGNMENT_AVOIDANCE
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
	   ret = INT2PTR(pdl *,SvIV(sv2));
   }
#else
    ret = INT2PTR(pdl *, SvIV(sv2));
#endif

   if(ret->magicno != PDL_MAGICNO) {
   	croak("Fatal error: argument is probably not a piddle, or\
 magic no overwritten. You're in trouble, guv: %p %p %lu\n",sv2,ret,ret->magicno);
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


void* pdl_malloc ( STRLEN nbytes ) {
    STRLEN n_a;
   SV* work;

   work = sv_2mortal(newSVpv("", 0));

   SvGROW( work, nbytes);

   return (void *) SvPV(work, n_a);
}

/*********** Stuff for barfing *************/
/*
   This routine barfs/warns in a thread-safe manner. If we're in the main thread,
   this calls the perl-level barf/warn. If in a worker thread, we save the
   message to barf/warn in the main thread later
*/

static void pdl_barf_or_warn(const char* pat, int iswarn, va_list* args)
{
    /* If we're in a worker thread, we queue the
     * barf/warn for later, and exit the thread ...
     */
    if( pdl_pthread_barf_or_warn(pat, iswarn, args) )
        return;

    /* ... otherwise we fall through and barf by calling
     * the perl-level PDL::barf() or PDL::cluck()
     */

    { /* scope block for C89 compatibility */

        SV * sv;

        dSP;
        ENTER;
        SAVETMPS;
        PUSHMARK(SP);

        sv = sv_2mortal(newSV(0));
        sv_vsetpvfn(sv, pat, strlen(pat), args, Null(SV**), 0, Null(bool*));
        va_end(*args);

        XPUSHs(sv);

        PUTBACK;

        if(iswarn) call_pv("PDL::cluck", G_DISCARD);
        else       call_pv("PDL::barf",  G_DISCARD);

        FREETMPS;
        LEAVE;
    } /* end C89 compatibility scope block */
}

#define GEN_PDL_BARF_OR_WARN_I_STDARG(type, iswarn)     \
    void pdl_##type(const char* pat, ...)               \
    {                                                   \
        va_list args;                                   \
        va_start(args, pat);                            \
        pdl_barf_or_warn(pat, iswarn, &args);           \
    }

#define GEN_PDL_BARF_OR_WARN_LEGACY(type, iswarn)       \
    void pdl_##type(pat, va_alist)                      \
        char *pat;                                      \
        va_dcl                                          \
    {                                                   \
        va_list args;                                   \
        va_start(args);                                 \
        pdl_barf_or_warn(pat, iswarn, &args);           \
    }

#ifdef I_STDARG
GEN_PDL_BARF_OR_WARN_I_STDARG(barf, 0)
GEN_PDL_BARF_OR_WARN_I_STDARG(warn, 1)
#else
GEN_PDL_BARF_OR_WARN_LEGACY(barf, 0)
GEN_PDL_BARF_OR_WARN_LEGACY(warn, 1)
#endif


/**********************************************************************
 *
 * CONSTRUCTOR/INGESTION HELPERS
 *
 * The following routines assist with the permissive constructor,
 * which is designed to build a PDL out of basically anything thrown at it.
 *
 * They are all called by pdl_avref in Core.xs, which in turn is called by the constructors
 * in Core.pm.PL.
 *
 *

/******************************
 * av_ndcheck -
 *  traverse a Perl array ref recursively, following down any number of
 *  levels of references, and generate a minimal PDL dim list that can
 *  encompass them all according to permissive-constructor rules.
 *
 *  Scalars, array refs, and PDLs may be mixed in the incoming AV.
 *
 *  The routine works out the dimensions of a corresponding
 *  piddle (in the AV dims) in reverse notation (vs PDL conventions).
 *
 *  It does not enforce a rectangular array, the idea being that
 *  omitted values will be set to zero in the resulting piddle,
 *  i.e. we can make piddles from 'sparse' array refs.
 *
 *  Empty PDLs are treated like any other dimension -- i.e. their 
 *  0-length dimensions are thrown into the mix just like nonzero 
 *  dimensions would be.
 *
 *  The possible presence of empty PDLs forces us to pad out dimensions
 *  to unity explicitly in cases like
 *         [ Empty[2x0x2], 5 ]
 *  where simple parsing would yield a dimlist of 
 *         [ 2,0,2,2 ]
 *  which is still Empty.
 */

int av_ndcheck(AV* av, AV* dims, int level, int *datalevel)
{
  int i, len, oldlen, newdepth, depth = 0;
  int n_scalars = 0;
  SV *el, **elp;
  pdl *pdl;           /* Stores PDL argument */

  if(dims==NULL) {
    pdl_barf("av_ndcheck - got a null dim array! This is a bug in PDL.");
  }

  /* Start with a clean slate */
   if(level==0) {
    av_clear(dims);
  }

  len = av_len(av);                         /* Loop over elements of the AV */
  for (i=0; i<= len; i++) {
    
    newdepth = 0;                           /* Each element - find depth */
    elp = av_fetch(av,i,0);
    
    el = elp ? *elp : 0;                    /* Get the ith element */
    if (el && SvROK(el)) {                  /* It is a reference */
      if (SvTYPE(SvRV(el)) == SVt_PVAV) {   /* It is an array reference */
	
	/* Recurse to find depth inside the array reference */
	newdepth = 1 + av_ndcheck((AV *) SvRV(el), dims, level+1, datalevel);
	
      } else if ( pdl = SvPDLV(el) ) {
	/* It is a PDL - walk down its dimension list, exactly as if it
	 * were a bunch of nested array refs.  We pull the ndims and dims
	 * fields out to local variables so that nulls can be treated specially.
	 */
	int j;
	short pndims;
	PDL_Long *pdims;
	PDL_Long pnvals;
	
#ifdef DEBUG_SETAV_TYPE
	printf("av_ndcheck - found a PDL....\n");
#endif
	
	pdl_make_physdims(pdl);
	
	pndims = pdl->ndims;
	pdims = pdl->dims;
	pnvals = pdl->nvals;
	
#ifdef DEBUG_SETAV_TYPE
	{
	  printf("av_ndcheck: nvals is %d; ndims is %d; dims are (",pdl->nvals, pdl->ndims);
	  for(j=0;j<pdl->ndims; j++) {
	    printf("%s%d",j?", ":"",pdl->dims[j]);
	  }
	  printf("); level is %d\n",level);
	}
#endif
	
	for(j=0;j<pndims;j++) {
	  int jl = pndims-j+level;
	  
	  int siz = pdims[j];
	  
	  if(  av_len(dims) >= jl &&
	       av_fetch(dims,jl,0) != NULL &&
	       SvIOK(*(av_fetch(dims,jl,0)))) {
	    
	    /* We have already found something that specifies this dimension -- so */ 
	    /* we keep the size if possible, or enlarge if necessary.              */
	    oldlen=(int)SvIV(*(av_fetch(dims,jl,0)));
	    if(siz > oldlen) {
	      sv_setiv(*(av_fetch(dims,jl,0)),(IV)(pdims[j]));
	    }
	    
	  } else {
	    /* Breaking new dimensional ground here -- if this is the first element */
	    /* in the arg list, then we can keep zero elements -- but if it is not  */
	    /* the first element, we have to pad zero dims to unity (because the    */
	    /* prior object had implicit size of 1 in all implicit dimensions)      */
	    av_store(dims, jl, newSViv((IV)(siz?siz:(i?1:0))));
	  }
	}
	
	/* We have specified all the dims in this PDL.  Now pad out the implicit */
	/* dims of size unity, to wipe out any dims of size zero we have already */
	/* marked. */
	
	for(j=pndims+1; j <= av_len(dims); j++) {
	  SV **svp = av_fetch(dims,j,0);

	  if(!svp){
	    av_store(dims, j, newSViv((IV)1));
	  } else if( (int)SvIV(*svp) == 0 ) {
	    sv_setiv(*svp, (IV)1);
	  }
	}
	
	newdepth= pndims;
	
      } else {
	croak("av_ndcheck: non-array, non-PDL ref in structure\n\t(this is usually a problem with a pdl() call)");
      }

    } else { 
      /* got a scalar (not a ref) */
      n_scalars++;

    }

      if (newdepth > depth)
	depth = newdepth;
  }
  
  len++; // convert from funky av_len return value to real count
  
#ifdef DEBUG_SETAV_TYPE
  {
      int i,dim;
      if(dims != NULL) {
	  dim = av_len(dims) + 1;
      } else {
	  dim =0 ;
      }

      printf("av_ndcheck:  depth=%d, length is %d; derived dim list is [",depth, dim);
      fflush(stdout);
      for( i=0; i<dim; i++ ) {
	  SV **svp = av_fetch(dims, i, 0);
	  int k;

	  if(svp != NULL) {
	      k = SvIV (*svp);
	  } else {
	      k = -1;
	  }

	  printf(" %d",k);
      }
      printf(" ]\n");
  }
#endif
  
    if (av_len(dims) >= level && av_fetch(dims, level, 0) != NULL
      && SvIOK(*(av_fetch(dims, level, 0)))) {
    oldlen = (int) SvIV(*(av_fetch(dims, level, 0)));
    
    if (len > oldlen)
      sv_setiv(*(av_fetch(dims, level, 0)), (IV) len);
    }
    else
      av_store(dims,level,newSViv((IV) len));
  
  /* We found at least one element -- so pad dims to unity at levels earlier than this one */
#ifdef DEBUG_SETAV_TYPE
    printf("n_scalars=%d\n",n_scalars);
#endif

  if(n_scalars) {
    for(i=0;i<level;i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
	av_store(dims, i, newSViv((IV)1));
      } else if( (int)SvIV(*svp) == 0) {
	sv_setiv(*svp, (IV)1);
      }
    }
    
    for(i=level+1; i <= av_len(dims); i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
	av_store(dims, i, newSViv((IV)1));
      } else if( (int)SvIV(*svp) == 0) {
	sv_setiv(*svp, (IV)1);
      }
    }
  }

#ifdef DEBUG_SETAV_TYPE
  {
      int i,dim;
      if(dims != NULL) {
	  dim = av_len(dims) + 1;
      } else {
	  dim =0 ;
      }

      printf("av_ndcheck:  depth=%d, length is %d; derived dim list is [",depth, dim);
      fflush(stdout);
      for( i=0; i<dim; i++ ) {
	  SV **svp = av_fetch(dims, i, 0);
	  int k;

	  if(svp != NULL) {
	      k = SvIV (*svp);
	  } else {
	      k = -1;
	  }

	  printf(" %d",k);
      }
      printf(" ]\n");
  }
#endif

  return depth;
}

pdl* pdl_from_array(AV* av, AV* dims, int type, pdl* p)
{
  int ndims, i, level=0;
  PDL_Long *pdims;
     double undefval;

  ndims = av_len(dims)+1;
  pdims = (PDL_Long *) pdl_malloc( (ndims) * sizeof(*pdims) );
  for (i=0; i<ndims; i++) {
     pdims[i] = SvIV(*(av_fetch(dims, ndims-1-i, 0))); /* reverse order */
  }

#ifdef DEBUG_SETAV_TYPE
  {
    int ii;
    printf("pdl_from_array: dim list is: (");
    for(ii=0; ii<ndims; ii++) {
      printf("%s%d",ii?", ":"",pdims[ii]);
    }
    printf(")\n");
  }
#endif

  if (p == NULL)
     p = pdl_new();
  pdl_setdims (p, pdims, ndims);
  p->datatype = type;
  pdl_allocdata (p);
  pdl_make_physical(p);
  /* this one assigns the data */

  {
    /******
     * Copy the undefval to fill empty spots in the piddle...
     */
    SV *sv = get_sv("PDL::undefval",0);
    undefval = ((!sv) || (sv==&PL_sv_undef)) ? 0 : (double)SvNV(sv);
  }

  switch (type) {
!NO!SUBS!

##########
# Perl snippet autogenerates switch statement to distribute
# pdl_setav calls...
#
  for my $type(sort keys %PDL_DATATYPES){
    my $t2 = $PDL_DATATYPES{$type};
    $t2 =~ s/PDL_//;
    print OUT <<"!WITH!SUBS!";
  case $type:
    pdl_setav_$t2(p->data,av,pdims,ndims,level, undefval);
    break;

!WITH!SUBS!
  }
#
# Back to your regularly scheduled C code emission...
########
  print OUT <<'!NO!SUBS!';
  default:
    croak("pdl_from_array: internal error: got type %d",type);
    break;
  }
  p->state &= ~PDL_NOMYDIMS;
  return p;
}


!NO!SUBS!

######################################################################
# these are helper functions for fast assignment from array refs
# mainly used by pdl_avref in Core.xs which implements converting
# array refs to piddles


for my $in ( keys %PDL_DATATYPES ) {

  (my $type = $PDL_DATATYPES{$in}) =~ s/^PDL_//;

print OUT <<"!WITH!SUBS!";

/*
 * pdl_kludge_copy  - copy a PDL into a part of a being-formed PDL.
 * It is only used by pdl_setav, to handle the case where a PDL is part
 * of the argument list. Ideally this would use the existing threadloop
 * code but that seems too hard.
 *
 * kludge_copy recursively walks down the dim list of both the source and dest
 * pdls, copying values in as we go.  It differs from PP copy in that it operates
 * on only a portion of the output pdl.
 *
 * (If I were Lazier I would have popped up into the perl level and used threadloops to
 * assign to a slice of the output pdl -- but this is probably a little faster.)
 *
 * -CED 17-Jun-2004
 *
 * Arguments:
 * poff  is an integer indicating which element along the current direction is being treated (for padding accounting)
 * pdata is a pointer into the destination PDL's data;
 * pdims is a pointer to the destination PDL's dim list;
 * ndims is the size of the destination PDL's dimlist;
 * level is the conjugate dimension along which copying is happening (indexes pdims).
 *    "conjugate" means that it counts backward through the dimension array.
 * stride is the increment in the data array corresponding to this dimension;
 *
 * pdl is the input PDL.
 * plevel is the dim number for the input PDL, which works in the same sense as level.
 *   It is offset to account for the difference in dimensionality between the input and
 *   output PDLs. It is allowed to be negative (which is equivalent to the "permissive
 *   slicing" that treats missing dimensions as present and having size 1), but should
 *   not match or exceed pdl->ndims.
 * pptr is the current offset data pointer into pdl->data.
 *
 * Kludge-copy works backward through the dim lists, so that padding is simpler:  if undefval
 * padding is required at any particular dimension level, the padding occupies a contiguous
 * block of memory.
 */

long pdl_kludge_copy_$type(long poff,
			   PDL_$type* pdata,
                           PDL_Long* pdims,
                           PDL_Long ndims,
                           int level,
                           long stride,
                           pdl* pdl,
                           int plevel,
                           void* pptr,
			   double undefval
			   ) {
  int i;
  long undef_count = 0;

#ifdef DEBUG_KLUDGE_COPY
  printf("entering pdl_kludge_copy: level=%d, ndims=%d, plevel=%d; pdl->ndims=%d\\n",level,ndims,plevel,pdl->ndims);
#endif

  if(level > ndims ) {
    fprintf(stderr,"pdl_kludge_copy: level=%d; ndims=%d\\n",level,ndims);
    croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: Assertion failed; ndims-1-level (%d) < 0!.",ndims-1-level);
  }

  if(level >= ndims - 1) {
    /* In this case we are in as far as we can go in the destination PDL, so direct copying is in order. */
    int pdldim = pdl->ndims - 1 - plevel;
    int pdlsiz;
    int oob = (ndims-1-level < 0);

    /* Do bounds checking on the source dimension -- if we wander off the end, we
     * are doing permissive-slicing kind of stuff; if we wander off the beginning, we
     * are doing dimensional padding.  In either case, we just iterate once.
     */
    if(pdldim < 0 || pdldim >= pdl->ndims) {
      pdldim = (pdldim < 0) ? (0) : (pdl->ndims - 1);
      pdlsiz = 1;
    } else {
      pdlsiz = pdl->dims[pdldim];
    }


#ifdef DEBUG_KLUDGE_COPY
    fprintf(stderr,"     pdldim_expr=%d, pdldim=%d, pdlsiz=%d, pdims[0]=%d\\n",pdl->ndims-1-plevel,pdldim, pdlsiz,pdims[0]);
#endif

    switch(pdl->datatype) {

!WITH!SUBS!

        # perl loop to emit code for all the PDL types
	#
	foreach my $switch_type (keys %PDL::Types::typehash) {

	my $ctype = $PDL::Types::typehash{$switch_type}{ctype};

	print OUT <<"!WITH!SUBS!";


      case ${switch_type}:
#ifdef DEBUG_KLUDGE_COPY
      if(pptr && pdata) {
        fprintf(stderr,"*** kludge Assigning to %d (num - %g); pdlsiz=%d\\n",pdata, (double)( *((${ctype} *)pptr)),pdlsiz);
      } else {
	fprintf(stderr,"*** kludge Skipping assignment (null pointer in source)\\n");
      }
#endif
	   /* copy data (unless the source pointer is null) */
      i=0;
      if(pptr && pdata && pdlsiz) {
	for(; i<pdlsiz; i++)
	  pdata[i] = (PDL_$type) ((${ctype} *)pptr)[i];
      } else {
	if(pdata) 
	  pdata[i] = undefval;
      }
	/* pad out, in the innermost dimension */
#ifdef DEBUG_KLUDGE_COPY
      fprintf(stderr,"padding; "); fflush(stderr);
      fprintf(stderr," ndims-1-level=%d; ndims=%d, level=%d; plevel=%d; pdl->ndims=%d; poff=%d; oob=%d\\n", ndims-1-level,ndims, level, plevel, pdl->ndims, poff, oob);
#endif
      if( !oob ) {
	for(;  i< pdims[0]-poff; i++) {
	  undef_count++;
	  pdata[i] = undefval;
	}
#ifdef DEBUG_KLUDGE_COPY
      fprintf(stderr,"      filled in row: ");
      for(i=0;i<pdims[0] - poff; i++)
	fprintf(stderr,"%g ",pdata[i]);
      fprintf(stderr,"\\n");
#endif
      }

      break;
!WITH!SUBS!

	} # end of foreach in the perl generator code

      print OUT <<"!WITH!SUBS!";
    default:
      croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: unknown type of %d.",pdl->datatype);
      break;
    }

    return undef_count;
  }

  /* If we're here we're not at the bottom level yet... */
#ifdef DEBUG_KLUDGE_COPY
  printf("Entering PKC recursion loop.  imax is %d (plevel=%d; pdl->dims[%d-1-%d] = %d)\\n",
	 (   (plevel >= 0)   ?   (pdl->dims[ pdl->ndims - 1 - level ])   :   1    ),
	 plevel,
	 ndims,
	 plevel,
	 pdl->dims[pdl->ndims-1-plevel]
	 );
#endif
  for(i=0;
      i    <    (   (plevel >= 0 && (pdl->ndims - 1 - plevel >= 0) && (pdl->ndims - 1 - plevel < pdl->ndims))   ?   (pdl->dims[ pdl->ndims-1-plevel ])   :   1    );
      i++) {

#ifdef DEBUG_KLUDGE_COPY
    {
      char buf[10240];
      char sb[1024];
      int ii;
      *buf=0;
      for(ii=0;ii<ndims;ii++) {
	sprintf(sb,"%d",pdims[ii]);
	if(ii) strcat(buf,",");
	strcat(buf,sb);
      }
      printf("pdl_kludge_copy: pushing... level=%d, i=%d, pdata=%d, pdims=(%s), ndims=%d, stride=%d, plevel=%d, pptr=%d\\n",
	     level, i, pdata, buf, ndims, stride, plevel, pptr);

      printf("  (pdims[ndims-2-level] is %d)\\n",pdims[ndims-2-level]);
    }
#endif
    undef_count += pdl_kludge_copy_$type(0, pdata + stride * i,
			  pdims,
			  ndims,
			  level+1,
			  stride / ((pdims[ndims-2-level]) ? (pdims[ndims-2-level]) : 1),
			  pdl,
			  plevel+1,
			  ((PDL_Byte *) pptr) + pdl->dimincs[pdl->ndims-1-plevel] * i * pdl_howbig(pdl->datatype),
			  undefval
			  );
#ifdef DEBUG_KLUDGE_COPY
    printf("(level=%d, back from recursion)\\n",level);
#endif
  } /* end of kludge_copy recursion loop */

  /* pad tree to zero if there are not enough elements... */
#ifdef DEBUG_KLUDGE_COPY
  printf("   pdl_kludge_copy - finished recursion  loop at level %d... i is %d, pdims[%d]=%d...\\n",
	 level, i, ndims - 1 - level);
#endif

  if(i < pdims[ndims - 1 - level]) {
      int cursor, target;

      cursor = i * stride;
      target = pdims[ndims-1-level]*stride;
      undef_count += target - cursor;

#ifdef DEBUG_KLUDGE_COPY
	printf("i=%d, pdims[%d - 1 - %d]=%d, stride=%d, cursor=%d, target=%d....\\n",i,ndims,level,pdims[ndims - 1 - level],stride,cursor,target);
	{
	    int ii;
	    printf("   pdims is:[");
	    for(ii=0; ii<ndims;ii++) {
		printf("%s%d",(ii?", ":""),pdims[ii]);
	    }
	    printf("]\\n");
	}
#endif


      for(;
	  cursor < target;
	  cursor++) {
	  pdata[cursor] = undefval;
      }

  } /* end of padding IF statement */

  return undef_count;
}

/*
 * pdl_setav_type loads a new PDL with values from a Perl AV, another PDL, or
 * a mix of both.  Heterogeneous sizes are handled by padding the new PDLs
 * values out to size with the undefval.  It is called by pdl_setav only.
 *
 *
 * Recent changes to setav_$type:
 * - Look for PDLs and deep copy them with pdl_kludge_copy just as if they were
 *    array refs.
 * - Allow multiple depths in different elements.  The max depth should have been
 *    determined by pdl_av_ndcheck (it comes in as the ndims parameter), so other
 *    depths are descended to and extra elements are filled in with zeroes.
 *    In the Best of All Possible Worlds this would be badval compliant and
 *    the extra elements would get filled in with BAD.  
 * --CED, 17-Jun-2004
 *
 * - Check for undef values and set the PDL element to PDL::undefval for those
 *   elements.
 * - Keep track of how many undef values were encountered, for debugging
 *   (I am wary of action-at-a-distance)
 *
 *  -CED, 28-Jul-2004
 *
 *
 *   -  pdata is the data pointer from a PDL
 *   -  av is the array ref (or PDL) to use to fill the data with,
 *   -  pdims is the dimlist
 *   -  ndims is the size of the dimlist
 *   -  level is the recursion level, which is also the dimension that we are filling
 */

long pdl_setav_$type(PDL_$type* pdata, AV* av,
		     PDL_Long* pdims, PDL_Long ndims, int level, double undefval)
{
  int cursz = pdims[ndims-1-level]; /* we are starting from the highest dim
                                       inwards */
  int len = av_len(av);
  int i,stride=1;

  SV *el, **elp;
  long undef_count = 0;

  fflush(stdout);

  for (i=0;i<ndims-1-level;i++) {
    stride *= pdims[i];
  }

#ifdef DEBUG_SETAV_TYPE
  printf("entering pdl_setav_$type: pdata=%d\\n",pdata);
  {
    int i;
    printf("ndims=%d; level=%d; ndims-1-level is %d; pdims factor is (",ndims,level,ndims-1-level);
    for(i=0;i<ndims-1-level; i++) {
      printf("%s%d",(i?", ":""),pdims[i]);
    }
    printf("); initial stride is %d\\n",stride);
  }
#endif

  for (i=0;i<=len;i++,pdata += stride) { /* note len is actually the highest index in the array */

    int foo;

#ifdef DEBUG_SETAV_TYPE
	  {
	    /* Generate some debugging info */
	    char buf[10240];
	    char sb[1024];
	    int ii;
	    *buf=0;
	    for(ii=0;ii<ndims; ii++) {
	      sprintf(sb,"%d",pdims[ii]);
	      if(ii) strcat(buf,",");
	      strcat(buf,sb);
	    }
	    printf("pdl_setav_$type: level=%d, i=%d, pdata=%d, pdims=(%s), ndims=%d\\t",
		   level, i, pdata, buf, ndims);
	  }
#endif

    elp = av_fetch(av,i,0);
    el = (elp ? *elp : 0);
    foo = el ? SVavref(el) : 0;

    if (foo) {

#ifdef DEBUG_SETAV_TYPE
      printf("found an array ref -- recursing\\n");
#endif

      undef_count += pdl_setav_$type(pdata, (AV *) SvRV(el), pdims, ndims, level+1, undefval);

    } else if( el && SvROK(el) ) {
      pdl *pdl;
      if( pdl = SvPDLV(el) ) {

	pdl_make_physical(pdl);


#ifdef DEBUG_SETAV_TYPE
	printf("source pdl has %d vals...\\n",pdl->nvals);
#endif

	{ // convenience block.. recursively copy/pad each PDL.

#ifdef DEBUG_SETAV_TYPE
	  {
	    /* Generate some debugging info */
	    char buf[10240];
	    char sb[1024];
	    int ii;
	    *buf=0;
	    for(ii=0;ii<ndims; ii++) {
	      sprintf(sb,"%d",pdims[ii]);
	      if(ii) strcat(buf,",");
	      strcat(buf,sb);
	    }

	    printf("Calling pdl_kludge_copy - pdata is %d, pdims is (%s), ndims is %d, level is %d, stride is %d, pdl->data is %d\\n",
		   pdata, buf, ndims, level, stride, pdl->data
		   );
	  }
#endif
	  {
	    int pd = pdims[ ndims - 2 - level];
	    if(!pd)
	      pd = 1;
	    undef_count += pdl_kludge_copy_$type(0, pdata,pdims,ndims, level+1, stride / pd , pdl, 0, pdl->data,undefval);
	  }
	}

      } else {
	croak("Non-array, non-PDL element in list");
      }
    } else { /* SvROK(el)==0; scalar element */
      if( sv_undef(el) ) {
#ifdef DEBUG_SETAV_TYPE
	printf("undefined scalar element\\n");
#endif
	*pdata = (PDL_$type) undefval; /* undef case */
	undef_count++;

      } else {
#ifdef DEBUG_SETAV_TYPE
	printf("defined scalar element %g\\n",(double)SvNV(el));
#endif
	*pdata = (PDL_$type) SvNV(el);

      }
      /* Pad dim if we are not deep enough */
      if(level < ndims-1) {
	PDL_$type *cursor = pdata;
	PDL_$type *target = pdata + stride;

#ifdef DEBUG_SETAV_TYPE
	printf("\\tPadding: level=%d; ndims-1=%d.  pdata is %d, stride is %d, target is %d\\n",level,ndims-1,pdata,stride, target);
#endif
	for( cursor++;  cursor < target; cursor++ ) {
	  *cursor = (PDL_$type)undefval;
	  undef_count++;
	}
      }
    }

  } /* end of i loop */

  /* in case this dim is incomplete set remaining elements to the undefval */

#ifdef DEBUG_SETAV_TYPE
  printf("\\tloop is complete.  len is %d, cursz-1 is %d, stride is %d\\n",len,cursz-1, stride);
#endif

  if(len < cursz-1 ) {
    PDL_$type *target = pdata + stride * (cursz - 1 - len);
#ifdef DEBUG_SETAV_TYPE
    printf("\\tpadding %d elements with the undefval...\\n",target-pdata);
#endif

    for( ;
	 pdata < target;
	 pdata++
	 ) {

      *pdata = (PDL_$type) undefval;
      undef_count++;
    }
  }

  if(level==0 && undef_count) {
    char debug_flag;
    SV *sv;
    sv = get_sv("PDL::debug",0);
    debug_flag = (sv_undef(sv)) ? 0 : (char)SvIV(sv);

    if(debug_flag) {
      fprintf(stderr,"Warning: pdl_setav_$type converted undef to $PDL::undefval (%g) %ld time%s\\n",undefval,undef_count,undef_count==1?"":"s");
    }
  }

  return undef_count;
}


!WITH!SUBS!

  } # end type loop
