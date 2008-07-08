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
#ifdef _MSC_VER
#define TESTTYPE(b,a) {a foo = nv; a bar = foo; foo += 0; if(nv == bar) return b;}
#else
#define TESTTYPE(b,a) {a foo = nv; if(nv == foo) return b;}
#endif
#ifdef _MSC_VER
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

#ifdef _MSC_VER
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
here because it's defined static in util.c! */

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
   pdl_barf - warning routine to be called when PDL routines
   croak

   Now just a stupid croak wrapper

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
#ifdef I_STDARG
    va_start(args, pat);
#else
    va_start(args);
#endif
    message = pdl_mess(pat, &args);
    va_end(args);

    croak(message);
}

/* traverse an array ref recursively following down any number of
   levels of references and make sure that
   (1) no other references than array refs are present
   (2) at no level data and array refs are mixed

   The routine works out the dimensions of a corresponding
   piddle (in the AV dims) in reverse notation (vs PDL conventions).
  
   It does not enforce a rectangular array the idea being that
   omitted values will be set to zero in the resulting piddle,
   i.e. we can make piddles from 'sparse' array refs

*/

int av_ndcheck(AV* av, AV* dims, int level, int *datalevel)
{
  int i, len, oldlen, newdepth, depth = 0;
  int hasref = 0, hasnonref=0;
  SV *el;
  pdl *pdl;           /* Stores PDL argument */
  int num_nulls = 0;  /* keeps track of null PDLs (if any) */

  /* Start with a clean slate */
   if(level==0) {
    av_clear(dims);
  }

  len = av_len(av);                         /* Loop over elements of the AV */
  for (i=0; i<= len; i++) {
    newdepth = 0;                           /* Each element - find depth */
    el = *(av_fetch(av,i,0));               /* Get the ith element */

    if (SvROK(el)) {                        /* It is a reference */
      if (SvTYPE(SvRV(el)) == SVt_PVAV) {   /* It is an array reference */
         hasref = 1;                

         /* Recurse to find depth inside the array reference */
         newdepth = 1 + av_ndcheck((AV *) SvRV(el), dims, level+1, datalevel);
      
      } else if ( pdl = SvPDLV(el) ) {
	int j;
	
	/* It is a PDL - walk down its dimension list, exactly as if it
         * were a bunch of nested array refs. 
         */

	 pdl_make_physdims(pdl);

        if(pdl->nvals==0) {
	   num_nulls++;
        }

        for(j=0;j<pdl->ndims;j++) {
           int jl = pdl->ndims-j+level;
           if(  av_len(dims) >= jl && 
                av_fetch(dims,jl,0) != NULL &&
                SvIOK(*(av_fetch(dims,jl,0)))) {

              oldlen=(int)SvIV(*(av_fetch(dims,jl,0)));

              if(pdl->dims[j] > oldlen) {
                sv_setiv(*(av_fetch(dims,jl,0)),(IV)(pdl->dims[j]));
              }
	    } else {
              av_store(dims, jl, newSViv((IV)pdl->dims[j]));
	    }
        }
        
	newdepth= pdl->ndims;

      } else {
	croak("av_ndcheck: non-array, non-PDL ref in structure\n\t(this is usually a problem with a pdl() call)");
      }
    }
    if (newdepth > depth)
    depth = newdepth;
  }

  len++; /* Increment from last-element address to number of elements */
  len -= num_nulls; /* Null PDLs contribute no elements. */

  if (dims != NULL) {
    if (av_len(dims) >= level && av_fetch(dims, level, 0) != NULL
	&& SvIOK(*(av_fetch(dims, level, 0)))) {
      oldlen = (int) SvIV(*(av_fetch(dims, level, 0)));

      if (len > oldlen)
	sv_setiv(*(av_fetch(dims, level, 0)), (IV) len);
    }
    else 
      av_store(dims,level,newSViv((IV) len));
  }

  return depth;
}

pdl* pdl_from_array(AV* av, AV* dims, int type, pdl* p)
{
  int ndims, i, level=0;
  PDL_Long *pdims;
     double undefval; 

  ndims = av_len(dims)+1;
  pdims = (PDL_Long *) pdl_malloc( (ndims) * sizeof(*pdims) );
  for (i=0; i<ndims; i++)
     pdims[i] = SvIV(*(av_fetch(dims, ndims-1-i, 0))); /* reverse order */
  if (p == NULL)
     p = pdl_new();
  pdl_setdims (p, pdims, ndims);
  p->datatype = type;
  pdl_allocdata (p);
  pdl_make_physical(p);
  /* this one assigns the data */
  /* note that this switch statement should be generated from
   * %PDL_DATATYPES to make it compatible with future changes
   * in PDL data types
   */

  {
    /****** 
     * Copy the undefval to fill empty spots in the piddle...
     */
    SV *sv = get_sv("PDL::undefval",0);
    undefval = ((!sv) || (sv==&PL_sv_undef)) ? 0 : (double)SvNV(sv);
  }
  switch (type) {
  case PDL_D:
    pdl_setav_Double(p->data,av,pdims,ndims,level, undefval);
    break;
  case PDL_F:
    pdl_setav_Float(p->data,av,pdims,ndims,level, undefval);
    break;
  case PDL_L:
    pdl_setav_Long(p->data,av,pdims,ndims,level, undefval);
    break;
  case PDL_S:
    pdl_setav_Short(p->data,av,pdims,ndims,level, undefval);
    break;
  case PDL_U:
    pdl_setav_Ushort(p->data,av,pdims,ndims,level, undefval);
    break;
  case PDL_B:
    pdl_setav_Byte(p->data,av,pdims,ndims,level, undefval);
    break;
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
 * on only a portion of the input pdl.
 * 
 * (If I were Lazier I would have popped up into the perl level and used threadloops to
 * assign to a slice of the output pdl -- but this is probably a little faster.)
 *
 * -CED 17-Jun-2004
 */

void pdl_kludge_copy_$type(PDL_$type* pdata, 
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

  //  printf("kludge_copying: stride=%d, pdata=%d, level=%d, plevel=%d; pptr=%d; pdl->data=%d\\n",stride,pdata, level, plevel, pptr,pdl->data);


  if(plevel > pdl->ndims || level > ndims)
   croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: Assertion failed; plevel (%d) > pdl->ndims (%d)",plevel,pdl->ndims-1);
  
  if(plevel <= pdl->ndims-1 ) {
   
    if(ndims-2-level < 0) 
       croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: Assertion failed; ndims-2-level (%d) < 0!.",ndims-2-level);

    stride /= pdims[ndims-2-level];  

    for(i=0;i<pdl->dims[pdl->ndims-1-plevel];i++) {

      //      printf("kc, plev %d: i=%d  ",plevel,i);

      pdl_kludge_copy_$type(pdata + stride*i,
                            pdims,
                            ndims,
                            level+1,
                            stride,
                            pdl,
                            plevel+1,
                            ((PDL_Byte *)pptr)+pdl->dimincs[pdl->ndims-1-plevel]* i * pdl_howbig(pdl->datatype),
			    undefval
                           );
    }
    /* pad tree to zero if there are not enough elements... */
      if(i < pdims[pdl->ndims - 1 - level]) {
	//		printf("i=%d, pdims[%d - 1 - %d]=%d....\\n",i,pdl->ndims,level,pdims[pdl->ndims - 1 - level]);
	int j = (pdims[pdl->ndims - 1 - level] - i)*stride;
	PDL_$type *target = pdata + pdims[pdl->ndims - 1 - level]*stride;
	PDL_$type *cursor = pdata + i * stride;
	
	for(j=0; cursor < target; cursor++,j++) {
	  *cursor = undefval;
	  //  	  printf("*** kludge Assigning to %d (pad)\\n",cursor);
	}
      }

  } else {

      switch(pdl->datatype) {
!WITH!SUBS!

# loop through all the types
#
foreach my $switch_type (keys %PDL::Types::typehash) {

    my $ctype = $PDL::Types::typehash{$switch_type}{ctype};

print OUT <<"!WITH!SUBS!";
       case ${switch_type}:
       //              printf("*** kludge Assigning to %d (num - %g)\\n",pdata, (double)( *((${ctype} *)pptr) ));
          *pdata = (PDL_$type) (*((${ctype} *)pptr));
          break;
!WITH!SUBS!
}

print OUT <<"!WITH!SUBS!";
        default: 
          croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: unknown type of %d.",pdl->datatype);
          break;
     }

      /* Pad subtree to zero if the PDL is not deep enough */
      if(level < ndims-1) {
	PDL_$type *target = pdata + stride;
	*pdata++;
	for(; pdata<target; pdata++) {
	  *pdata = undefval;
	  //	  	  printf("*** kludge: assigning to %d (final pad)\\n",pdata);
	}
      }
  }
}


/*
 * Recent changes to setav_$type:
 * - Look for PDLs and deep copy them with pdl_kludge_copy just as if they were
 *    array refs.
 * - Allow multiple depths in different elements.  The max depth should have been
 *    determined by pdl_av_ndcheck (it comes in as the ndims parameter), so other
 *    depths are descended to and extra elements are filled in with zeroes.
 *    In the Best of All Possible Worlds this would be badval compliant and 
 *    the extra elements would get filled in with BAD.  Soon, soon.
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

  SV* el;
  long undef_count = 0;

  for (i=0;i<ndims-1-level;i++)
    stride *= pdims[i];

  for (i=0;i<=len;i++,pdata += stride) {

    el = *(av_fetch(av,i,0));
    if (SVavref(el)) {
      undef_count += pdl_setav_$type(pdata, (AV *) SvRV(el), pdims, ndims, level+1, undefval);

    } else if( SvROK(el) ) {
      pdl *pdl;
      if( pdl = SvPDLV(el) ) {
	
	pdl_make_physical(pdl);
	
	if(pdl->nvals==0) { /* Ignore null PDLs */
	  pdata -= stride; 
	    
	} else {                     /* PDLs must be recursively copied/padded */   
	  pdl_kludge_copy_$type(pdata,pdims,ndims,level,stride,pdl,0,pdl->data,undefval);
	  
        }
      } else {
	croak("Non-array, non-PDL element in list");
      }
    } else { /* SvROK(el)==0; scalar element */
      if( sv_undef(el) ) {
	//		printf("*** setav Assigning to %d (pad)\\n",pdata);
	*pdata = (PDL_$type) undefval; /* undef case */
	undef_count++;

      } else {
	//		printf("*** setav Assigning to %d (num)\\n",pdata);
	*pdata = (PDL_$type) SvNV(el);

      }
      /* Pad dim if we're not deep enough */
      if(level < ndims-1) {
	PDL_$type *cursor = pdata;
	PDL_$type *target = pdata + stride;
	for( cursor++;  cursor < target; cursor++ ) {
	  //	  	  printf("*** setav assigning to %d (long pad)\\n",cursor);
	  *cursor = (PDL_$type)undefval;
	  undef_count++;
	}
      }
    }
    
  } /* end of i loop */
  
  /* in case this dim is incomplete set remaining elements to zero */

  //  printf("len=%d; cursz=%d\\n",len,cursz);
  if(len < cursz -1 ) {
    PDL_$type *target = pdata + stride * (cursz - 1 - len);
    for( ;
	 pdata < target;
	 pdata++
	 ) {
      //            printf("*** setav assigning to %d (final pad)\\n",pdata);
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
      fprintf(stderr,"Warning: pdl_setav_$type converted undef to $PDL::undefval (%g) %d time%s\\n",undefval,undef_count,undef_count==1?"":"s");
    }
  }

  return undef_count;
}


!WITH!SUBS!

} # end type loop
