# Create pdlcore.c
# - needed for bad-value handling in whichdatatype

use strict;

use Config;
use File::Basename qw(&basename &dirname);

require 'Dev.pm'; PDL::Core::Dev->import;
use vars qw( %PDL_DATATYPES );

# check for bad value support
require './Config.pm'; # to load the PDL not the Perl one
die "No PDL::Config found" unless %PDL::Config;
my $bvalflag = $PDL::Config{WITH_BADVAL};
my $usenan = $PDL::Config{BADVAL_USENAN};
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


print OUT <<'!HEADER!';
#undef FOODEB
#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

/***************
 * So many ways to be undefined...
 */
#define sv_undef(sv)  ( (!(sv) || ((sv)==&PL_sv_undef)) || !(SvNIOK(sv) || (SvTYPE(sv)==SVt_PVMG) || SvPOK(sv) || SvROK(sv)))
!HEADER!


##############################
# Figure out the best definition for the 'finite()' call or call-like macro
# We short-circuit the process for the cl compiler; for other compilers we search 
# for 'isfinite' or 'finite', with a preference for 'isfinite' (IEEE recommends this).

if($Config{cc} eq 'cl') {
  # _finite in CV++ 4.0
  print OUT <<'FOO';
#define finite _finite
#include <float.h>
FOO
} else {
    my $finite_inc;
    my $use_isfinite = 0;
    foreach my $inc ( qw/ math.h ieeefp.h / )
    {
        if ( trylink ("finite: $inc", "#include <$inc>", 'isfinite(3.2);', '' ) ) {
            $finite_inc = $inc;
            $use_isfinite = 1;
            last;
        }
        if ( (!defined($finite_inc))  and  trylink ("finite: $inc", "#include <$inc>", 'finite(3.2);','') ) {
            $finite_inc = $inc;
        }
    }
    
    if ( defined $finite_inc ) {
        print OUT "#include <$finite_inc>\n";
        print OUT "#define finite(a) (isfinite(a))\n";
    }
    else
    {
        print OUT <<'!NO!SUBS!';
            /* Kludgy finite/isfinite because pdlcore.c.PL was unable to find one in your math library */
#ifndef finite
#ifdef isfinite
#define finite isfinite
#else
#define finite(a) (((a) * 0) == (0))
#endif
#endif
!NO!SUBS!
    }
}


##############################
# PDL handling stuff starts here
#
print OUT <<'!NO!SUBS!'
int _anyval_eq_anyval(PDL_Anyval x, PDL_Anyval y) {
    switch (x.type) {
        case PDL_B:
            switch (y.type) {
                case PDL_B:   return (x.value.B                 == y.value.B) ? 1 : 0;
                case PDL_S:   return ((PDL_Short)(x.value.B)    == y.value.S) ? 1 : 0;
                case PDL_US:  return ((PDL_Ushort)(x.value.B)   == y.value.U) ? 1 : 0;
                case PDL_L:   return ((PDL_Long)(x.value.B)     == y.value.L) ? 1 : 0;
                case PDL_IND: return ((PDL_Indx)(x.value.B)     == y.value.N) ? 1 : 0;
                case PDL_LL:  return ((PDL_LongLong)(x.value.B) == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.B)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.B)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_S:
            switch (y.type) {
                case PDL_B:   return (x.value.S                 == (PDL_Short)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.S                 == y.value.S) ? 1 : 0;
                case PDL_US:  return ((PDL_Long)(x.value.S)     == (PDL_Long)(y.value.U)) ? 1 : 0;
                case PDL_L:   return ((PDL_Long)(x.value.S)     == y.value.L) ? 1 : 0;
                case PDL_IND: return ((PDL_Indx)(x.value.S)     == y.value.N) ? 1 : 0;
                case PDL_LL:  return ((PDL_LongLong)(x.value.S) == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.S)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.S)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_US:
            switch (y.type) {
                case PDL_B:   return (x.value.U                 == (PDL_Ushort)(y.value.B)) ? 1 : 0;
                case PDL_S:   return ((PDL_Long)(x.value.U)     == (PDL_Long)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.U                 == y.value.U) ? 1 : 0;
                case PDL_L:   return ((PDL_Long)(x.value.U)     == y.value.L) ? 1 : 0;
                case PDL_IND: return ((PDL_Indx)(x.value.U)     == y.value.N) ? 1 : 0;
                case PDL_LL:  return ((PDL_LongLong)(x.value.U) == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.U)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.U)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_L:
            switch (y.type) {
                case PDL_B:   return (x.value.L                 == (PDL_Long)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.L                 == (PDL_Long)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.L                 == (PDL_Long)(y.value.U)) ? 1 : 0;
                case PDL_L:   return (x.value.L                 == y.value.L) ? 1 : 0;
                case PDL_IND: return ((PDL_Indx)(x.value.L)     == y.value.N) ? 1 : 0;
                case PDL_LL:  return ((PDL_LongLong)(x.value.L) == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.L)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.L)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_IND:
            switch (y.type) {
                case PDL_B:   return (x.value.N                 == (PDL_Indx)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.N                 == (PDL_Indx)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.N                 == (PDL_Indx)(y.value.U)) ? 1 : 0;
                case PDL_L:   return (x.value.N                 == (PDL_Indx)(y.value.L)) ? 1 : 0;
                case PDL_IND: return (x.value.N                 == y.value.N) ? 1 : 0;
                case PDL_LL:  return ((PDL_LongLong)(x.value.N) == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.N)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.N)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_LL:
            switch (y.type) {
                case PDL_B:   return (x.value.Q                 == (PDL_LongLong)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.Q                 == (PDL_LongLong)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.Q                 == (PDL_LongLong)(y.value.U)) ? 1 : 0;
                case PDL_L:   return (x.value.Q                 == (PDL_LongLong)(y.value.L)) ? 1 : 0;
                case PDL_IND: return (x.value.Q                 == (PDL_LongLong)(y.value.N)) ? 1 : 0;
                case PDL_LL:  return (x.value.Q                 == y.value.Q) ? 1 : 0;
                case PDL_F:   return ((PDL_Float)(x.value.Q)    == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.Q)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_F:
            switch (y.type) {
                case PDL_B:   return (x.value.F                 == (PDL_Float)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.F                 == (PDL_Float)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.F                 == (PDL_Float)(y.value.U)) ? 1 : 0;
                case PDL_L:   return (x.value.F                 == (PDL_Float)(y.value.L)) ? 1 : 0;
                case PDL_IND: return (x.value.F                 == (PDL_Float)(y.value.N)) ? 1 : 0;
                case PDL_LL:  return (x.value.F                 == (PDL_Float)(y.value.Q)) ? 1 : 0;
                case PDL_F:   return (x.value.F                 == y.value.F) ? 1 : 0;
                case PDL_D:   return ((PDL_Double)(x.value.F)   == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        case PDL_D:
            switch (y.type) {
                case PDL_B:   return (x.value.D                 == (PDL_Double)(y.value.B)) ? 1 : 0;
                case PDL_S:   return (x.value.D                 == (PDL_Double)(y.value.S)) ? 1 : 0;
                case PDL_US:  return (x.value.D                 == (PDL_Double)(y.value.U)) ? 1 : 0;
                case PDL_L:   return (x.value.D                 == (PDL_Double)(y.value.L)) ? 1 : 0;
                case PDL_IND: return (x.value.D                 == (PDL_Double)(y.value.N)) ? 1 : 0;
                case PDL_LL:  return (x.value.D                 == (PDL_Double)(y.value.Q)) ? 1 : 0;
                case PDL_F:   return (x.value.D                 == (PDL_Double)(y.value.F)) ? 1 : 0;
                case PDL_D:   return (x.value.D                 == y.value.D) ? 1 : 0;
                default:      return 0;
            }
        default:
            return 0;
    }
}

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
int pdl_whichdatatype (IV nv) {
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

int pdl_whichdatatype_double (NV nv) {
        TESTTYPE(PDL_F,PDL_Float)
        TESTTYPE(PDL_D,PDL_Double)

        if( !finite(nv) ) { return PDL_D; }
        croak("Something's gone wrong: %lf cannot be converted by whichdatatype_double",
                nv);
}


#if defined _MSC_VER && _MSC_VER < 1400
#pragma optimize("", on)
#endif




/* Make a scratch dataspace for a scalar pdl */

void pdl_makescratchhash(pdl *ret, PDL_Anyval data) {
  STRLEN n_a;
  HV *hash;
  SV *dat; PDL_Indx fake[1];
  
  /* Compress to smallest available type.  */
  ret->datatype = data.type;

  /* Create a string SV of apropriate size.  The string is arbitrary
   * and just has to be larger than the largest datatype.   */
  dat = newSVpvn("                                ",pdl_howbig(ret->datatype));
  
  ret->data = SvPV(dat,n_a);
  ret->datasv = dat;
  /* Refcnt should be 1 already... */
    
  /* Make the whole pdl mortal so destruction happens at the right time.
   * If there are dangling references, pdlapi.c knows not to actually
   * destroy the C struct. */
  sv_2mortal(getref_pdl(ret));
  
  pdl_setdims(ret, fake, 0); /* 0 dims in a scalar */
  ret->nvals = 1;            /* 1 val  in a scalar */
  
  /* NULLs should be ok because no dimensions. */
  pdl_set(ret->data, ret->datatype, NULL, NULL, NULL, 0, 0, data);
  
}


/*
  "Convert" a perl SV into a pdl (alright more like a mapping as
   the data block is not actually copied in the most common case
   of a single scalar) scalars are automatically converted to PDLs.
*/

pdl* SvPDLV ( SV* sv ) {

   pdl* ret;
   PDL_Indx fake[1];
   SV *sv2;

   if ( !SvROK(sv) ) {  
      /* The scalar is not a ref, so we can use direct conversion. */

      SV *dat;
      PDL_Anyval data;
      int datatype;
      NV tmp_NV;
      IV tmp_IV;

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
      if ( !SvIOK(sv) )  { /* Perl Double (e.g. 2.0) */
         tmp_NV = SvNV(sv);

!NO!SUBS!

##########
# If badvals and usenan are true, we default NaNs to type double. 
if ( $bvalflag and $usenan ) {
    print OUT q{
      /*
       * default NaN/Infs to double
       *
       */
      if ( finite(tmp_NV) == 0 ) {
        datatype = PDL_D;
      } else {
        datatype = pdl_whichdatatype_double(tmp_NV);
      }
   }
} else {
   print OUT q{
        datatype = pdl_whichdatatype_double(tmp_NV);

   }
} # if: $bvalflag

print OUT <<'!NO!SUBS!';
        ANYVAL_FROM_CTYPE(data, datatype, tmp_NV);
     } /* end of Perl Double case for data type */
     else { /* Perl Int (e.g. 2) */
        tmp_IV = SvIV(sv);
        datatype = pdl_whichdatatype(tmp_IV);
        ANYVAL_FROM_CTYPE(data, datatype, tmp_IV);
     }

     pdl_makescratchhash(ret, data);

     return ret;

   } /* End of scalar case */

   /* If execution reaches here, then sv is NOT a scalar
    * (i.e. it is a ref).
    */



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
           SAVETMPS ;
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

        if(SvGMAGICAL(sv)) {
                mg_get(sv);
        }

        if ( !SvROK(sv) ) {   /* Got something from a hash but not a ref */
                croak("Hash given as pdl - but PDL key is not a ref!");
        }
    }
      
    if(SvTYPE(SvRV(sv)) == SVt_PVAV) {
        /* This is similar to pdl_avref in Core.xs.PL -- we do the same steps here. */
        AV *dims, *av;
        int i, depth; 
        int datalevel = -1;
        pdl *p;

        av = (AV *)SvRV(sv);
        dims = (AV *)sv_2mortal((SV *)newAV());
        av_store(dims,0,newSViv( (IV) av_len(av)+1 ) );
        
        /* Pull sizes using av_ndcheck */
        depth = 1 + av_ndcheck(av,dims,0,&datalevel);

        return pdl_from_array(av, dims, -1, NULL); /* -1 means pdltype autodetection */

    } /* end of AV code */
    
    if (SvTYPE(SvRV(sv)) != SVt_PVMG)
      croak("Error - tried to use an unknown data structure as a PDL");
    else if( !( sv_derived_from( sv, "PDL") ) )
      croak("Error - tried to use an unknown Perl object type as a PDL");

    sv2 = (SV*) SvRV(sv);

    /* Return the pdl * pointer */
    ret = INT2PTR(pdl *, SvIV(sv2));

    /* Final check -- make sure it has the right magic number */
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

PDL_Indx* pdl_packdims ( SV* sv, int *ndims ) {

   SV*  bar;
   AV*  array;
   int i;
   PDL_Indx *dims;

   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;

   array = (AV *) SvRV(sv);   /* dereference */

   *ndims = (int) av_len(array) + 1;  /* Number of dimensions */

   dims = (PDL_Indx *) pdl_malloc( (*ndims) * sizeof(*dims) ); /* Array space */
   if (dims == NULL)
      croak("Out of memory");

   for(i=0; i<(*ndims); i++) {
      bar = *(av_fetch( array, i, 0 )); /* Fetch */
      dims[i] = (PDL_Indx) SvIV(bar);
   }
   return dims;
}

/* unpack dims array into PDL SV* */

void pdl_unpackdims ( SV* sv, PDL_Indx *dims, int ndims ) {

   AV*  array;
   HV* hash;
   int i;

   hash = (HV*) SvRV( sv );
   array = newAV();
   (void)hv_store(hash, "Dims", strlen("Dims"), newRV( (SV*) array), 0 );

   if (ndims==0 )
      return;

   for(i=0; i<ndims; i++)
         av_store( array, i, newSViv( (IV)dims[i] ) );
}

PDL_Indx pdl_safe_indterm( PDL_Indx dsz, PDL_Indx at, char *file, int lineno)
{
  if (at >= 0 && at < dsz) return at;
  pdl_barf("access [%d] out of range [0..%d] (inclusive) at %s line %d",
          at, dsz-1, file?file:"?", lineno);
  return at; // This can never happen - placed to avoid a compiler warning.
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
 * in Core.pm.PL.  The main entry point is pdl_from_array(), which calls 
 * av_ndcheck() to identify the necessary size of the output PDL, and then dispatches
 * the copy into pdl_setav_<type> according to the type of the output PDL.
 *
 */

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
 *  It does not enforce a rectangular array on the input, the idea being that
 *  omitted values will be set to zero or the undefval in the resulting piddle,
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

PDL_Indx av_ndcheck(AV* av, AV* dims, int level, int *datalevel)
{
  PDL_Indx i, len, oldlen;
  int newdepth, depth = 0;
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
        
      } else if ( (pdl = SvPDLV(el)) ) {
        /* It is a PDL - walk down its dimension list, exactly as if it
         * were a bunch of nested array refs.  We pull the ndims and dims
         * fields out to local variables so that nulls can be treated specially.
         */
        int j;
        short pndims;
        PDL_Indx *pdims;
        PDL_Indx pnvals;
        
        pdl_make_physdims(pdl);
        
        pndims = pdl->ndims;
        pdims = pdl->dims;
        pnvals = pdl->nvals;
        
        for(j=0;j<pndims;j++) {
          int jl = pndims-j+level;
          
          PDL_Indx siz = pdims[j];
          
          if(  av_len(dims) >= jl &&
               av_fetch(dims,jl,0) != NULL &&
               SvIOK(*(av_fetch(dims,jl,0)))) {
            
            /* We have already found something that specifies this dimension -- so */ 
            /* we keep the size if possible, or enlarge if necessary.              */
            oldlen=(PDL_Indx)SvIV(*(av_fetch(dims,jl,0)));
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
  
    if (av_len(dims) >= level && av_fetch(dims, level, 0) != NULL
      && SvIOK(*(av_fetch(dims, level, 0)))) {
    oldlen = (PDL_Indx) SvIV(*(av_fetch(dims, level, 0)));
    
    if (len > oldlen)
      sv_setiv(*(av_fetch(dims, level, 0)), (IV) len);
    }
    else
      av_store(dims,level,newSViv((IV) len));
  
  /* We found at least one element -- so pad dims to unity at levels earlier than this one */
  if(n_scalars) {
    for(i=0;i<level;i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
        av_store(dims, i, newSViv((IV)1));
      } else if( (PDL_Indx)SvIV(*svp) == 0) {
        sv_setiv(*svp, (IV)1);
      }
    }
    
    for(i=level+1; i <= av_len(dims); i++) {
      SV **svp = av_fetch(dims, i, 0);
      if(!svp) {
        av_store(dims, i, newSViv((IV)1));
      } else if( (PDL_Indx)SvIV(*svp) == 0) {
        sv_setiv(*svp, (IV)1);
      }
    }
  }

  return depth;
}

/* helper function used in pdl_from_array */
static int _detect_datatype(AV *av) {
  SV **item;
  AV *array;
  int count, i;
  if (!av) return PDL_D;
  count = av_len(av);
  for (i = 0; i < count; i++) {
    item = av_fetch(av, i, 0);
    if (*item) {
      if (SvROK(*item)) {
        array = (AV*)SvRV(*item);
        if (_detect_datatype(array) == PDL_D) {
          return PDL_D;
        }
      }
      if (SvOK(*item) && !SvIOK(*item)) {
        return PDL_D;
      }
    }
  }
#if IVSIZE == 8
  return PDL_LL;
#else
  return PDL_L;
#endif
}

/**********************************************************************
 * pdl_from_array - dispatcher gets called only by pdl_avref (defined in
 * Core.xs) - it breaks out to pdl_setav_<type>, below, based on the 
 * type of the destination PDL.
 */
pdl* pdl_from_array(AV* av, AV* dims, int type, pdl* p)
{
  int ndims, i, level=0;
  PDL_Indx *pdims;
  PDL_Anyval undefval = { -1, 0 };

  ndims = av_len(dims)+1;
  pdims = (PDL_Indx *) pdl_malloc( (ndims) * sizeof(*pdims) );
  for (i=0; i<ndims; i++) {
     pdims[i] = SvIV(*(av_fetch(dims, ndims-1-i, 0))); /* reverse order */
  }

  if (p == NULL)
     p = pdl_new();
  pdl_setdims (p, pdims, ndims);
  if (type == -1) {
    /* XXX-PDLANYVAL do we reaaly want this type of autodetection (it used to be always PDL_D) */
    type = _detect_datatype(av);
  }
  p->datatype = type;
  pdl_allocdata (p);
  pdl_make_physical(p);
  /* this one assigns the data */
  {
    /******
     * Copy the undefval to fill empty spots in the piddle...
     */
    SV *sv = get_sv("PDL::undefval",0);
    if ((!sv) || (sv==&PL_sv_undef)) {
       ANYVAL_FROM_CTYPE(undefval, type, 0);
    }
    else {
       /* Need to set undefvalue from the perl scalar */
       if (SvIOK(sv)) {
         ANYVAL_FROM_CTYPE(undefval, type, SvIV(sv));
       }
       else if (SvNOK(sv)) {
         ANYVAL_FROM_CTYPE(undefval, type, SvNV(sv));
       }
       else {
         ANYVAL_FROM_CTYPE(undefval, type, 0); /* this should not happen */
       }
    }
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
    pdl_setav_$t2(p->data,av,pdims,ndims,level, undefval.value.$PDL::Types::typehash{$type}->{ppsym});
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

/*
 * pdl_kludge_copy_<type>  - copy a PDL into a part of a being-formed PDL.
 * It is only used by pdl_setav_<type>, to handle the case where a PDL is part
 * of the argument list. 
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
!NO!SUBS!

for my $in ( keys %PDL_DATATYPES ) {

  (my $type = $PDL_DATATYPES{$in}) =~ s/^PDL_//;

print OUT <<"!WITH!SUBS!";

PDL_Indx pdl_kludge_copy_$type(PDL_Indx poff, // Offset into the dest data array
                           PDL_$type* pdata,  // Data pointer in the dest data array
                           PDL_Indx* pdims,   // Pointer to the dimlist for the dest pdl
                           PDL_Indx ndims,         // Number of dimensions in the dest pdl
                           int level,         // Recursion level
                           PDL_Indx stride,   // Stride through memory for the current dim
                           pdl* pdl,          // pointer to the source pdl
                           int plevel,        // level within the source pdl
                           void* pptr,        // Data pointer in the source pdl
                           PDL_$type undefval // undefval for the dest pdl
                           ) {
  PDL_Indx i;
  PDL_Indx undef_count = 0;

  /* Can't copy into a level deeper than the number of dims in the output PDL */
  if(level > ndims ) {
    fprintf(stderr,"pdl_kludge_copy: level=%d; ndims=%"IND_FLAG"\\n",level,ndims);
    croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: Assertion failed; ndims-1-level (%"IND_FLAG") < 0!.",ndims-1-level);
  }

  if(level >= ndims - 1) {
    /* We are in as far as we can go in the destination PDL, so direct copying is in order. */
    int pdldim = pdl->ndims - 1 - plevel;  // which dim are we working in the source PDL?
    PDL_Indx pdlsiz;
    int oob = (ndims-1-level < 0);         // out-of-bounds flag

    /* Do bounds checking on the source dimension -- if we wander off the end of the 
     * dimlist, we are doing permissive-slicing kind of stuff (not enough dims in the 
     * source to fully account for the output dimlist); if we wander off the beginning, we
     * are doing dimensional padding.  In either case, we just iterate once.
     */
    if(pdldim < 0 || pdldim >= pdl->ndims) {
      pdldim = (pdldim < 0) ? (0) : (pdl->ndims - 1);
      pdlsiz = 1;
    } else {
      pdlsiz = pdl->dims[pdldim];
    }

    /* This is the actual data-copying code. It is generated with a Perl loop, to
     * ensure that all current PDL types get treated. */

    switch(pdl->datatype) {
!WITH!SUBS!

        # perl loop to emit code for all the PDL types
        #
        foreach my $switch_type (keys %PDL::Types::typehash) {

        my $ctype = $PDL::Types::typehash{$switch_type}{ctype};

        print OUT <<"!WITH!SUBS!";

      case ${switch_type}:
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
      if( !oob ) {
        for(;  i< pdims[0]-poff; i++) {
          undef_count++;
          pdata[i] = undefval;
        }
      }

      break;
!WITH!SUBS!

        } # end of foreach in the perl generator code

      print OUT <<"!WITH!SUBS!";
    default:
      croak("Internal error - please submit a bug report at http://sourceforge.net/projects/pdl/:\\n  pdl_kludge_copy: unknown datatype of %d.",(int)(pdl->datatype));
      break;
    }

    return undef_count;
  }

  /* If we are here, we are not at the bottom level yet.  So walk
   *  across this dim and handle copying one dim deeper via recursion.
   *  The loop is placed in a convenience block so we can define the 
   *  dimensional boundscheck flag -- that avoids having to evaluate the complex 
   *  ternary expression for every loop iteration.
   */
  {
      PDL_Indx limit =  (   
          (plevel >= 0 && 
           (pdl->ndims - 1 - plevel >= 0) && 
           (pdl->ndims - 1 - plevel < pdl->ndims)
          )   
          ?   (pdl->dims[ pdl->ndims-1-plevel ])   
          :   1    
          );
      for(i=0; i < limit ; i++) {
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
      } /* end of kludge_copy recursion loop */
  } /* end of recursion convenience block */

  /* pad the rest of this dim to zero if there are not enough elements in the source PDL... */
  if(i < pdims[ndims - 1 - level]) {
      int cursor, target;

      cursor = i * stride;
      target = pdims[ndims-1-level]*stride;
      undef_count += target - cursor;

      for(;
          cursor < target;
          cursor++) {
          pdata[cursor] = undefval;
      }

  } /* end of padding IF statement */

  return undef_count;
}

/*
 * pdl_setav_<type> loads a new PDL with values from a Perl AV, another PDL, or
 * a mix of both.  Heterogeneous sizes are handled by padding the new PDLs
 * values out to size with the undefval.  It is only called by pdl_setav in Core.XS,
 * via the trampoline pdl_from_array just above. pdl_from_array dispatches execution
 * to pdl_setav_<type> according to the type of the destination PDL. 
 *
 * The code is complicated by the "bag-of-stuff" nature of AVs.  We handle 
 * Perl scalars, AVs, *and* PDLs (via pdl_kludge_copy).
 * 
 *   -  pdata is the data pointer from a PDL
 *   -  av is the array ref (or PDL) to use to fill the data with,
 *   -  pdims is the dimlist
 *   -  ndims is the size of the dimlist
 *   -  level is the recursion level, which is also the dimension that we are filling
 */

PDL_Indx pdl_setav_$type(PDL_$type* pdata, AV* av,
                     PDL_Indx* pdims, int ndims, int level, PDL_$type undefval)
{
  PDL_Indx cursz = pdims[ndims-1-level]; /* we go from the highest dim inward */
  PDL_Indx len = av_len(av);
  PDL_Indx i,stride=1;

  SV *el, **elp;
  PDL_Indx undef_count = 0;

  for (i=0;i<ndims-1-level;i++) {
    stride *= pdims[i];
  }

  for (i=0;i<=len;i++,pdata += stride) { /* note len is actually highest index, not element count */

    int foo;

    /* Fetch the next value from the AV */
    elp = av_fetch(av,i,0);
    el = (elp ? *elp : 0);
    foo = el ? SVavref(el) : 0;

    if (foo) {
      /* If the element was an AV ref, recurse to walk through that AV, one dim lower */
      undef_count += pdl_setav_$type(pdata, (AV *) SvRV(el), pdims, ndims, level+1, undefval);

    } else if( el && SvROK(el) ) {

      /* If the element was a ref but not an AV, then it should be a PDL */
      pdl *pdl;
      if( (pdl = SvPDLV(el)) ) {
          /* The element was a PDL - use pdl_kludge_copy to copy it into the destination */
          PDL_Indx pd;
          int pddex;
          
          pdl_make_physical(pdl);
          
          pddex = ndims - 2 - level;
          pd = (pddex >= 0 && pddex < ndims ? pdims[ pddex ] : 0);
          if(!pd)
              pd = 1;
          undef_count += pdl_kludge_copy_$type(0, pdata,pdims,ndims, level+1, stride / pd , pdl, 0, pdl->data,undefval);

      } else {
        /* The element is a non-PDL, non-AV ref.  Not allowed. */
        croak("Non-array, non-PDL element in list");
      }
    } else { /* el==0 || SvROK(el)==0: this is a scalar or undef element */
      if( sv_undef(el) ) {  /* undef case */
        *pdata = (PDL_$type) undefval; 
        undef_count++;

      } else {              /* scalar case */
        if (SvIOK(el)) {
          *pdata = (PDL_$type) SvIV(el);
        } else {
          *pdata = (PDL_$type) SvNV(el);
        }
      }

      /* Pad dim if we are not deep enough */
      if(level < ndims-1) {
        PDL_$type *cursor = pdata;
        PDL_$type *target = pdata + stride;

        for( cursor++;  cursor < target; cursor++ ) {
          *cursor = (PDL_$type)undefval;
          undef_count++;
        }
      }
    }

  } /* end of element loop through the supplied AV */

  /* in case this dim is incomplete set any remaining elements to the undefval */

  if(len < cursz-1 ) {
    PDL_$type *target = pdata + stride * (cursz - 1 - len);
    for( ;
         pdata < target;
         pdata++
         ) {

      *pdata = (PDL_$type) undefval;
      undef_count++;
    }
  }

  /* If the Perl scalar PDL::debug is set, announce padding */
  if(level==0 && undef_count) {
    char debug_flag;
    SV *sv;
    sv = get_sv("PDL::debug",0);
    debug_flag = (sv_undef(sv)) ? 0 : (char)SvIV(sv);

    if(debug_flag) {
      fflush(stdout);
      fprintf(stderr,"Warning: pdl_setav_$type converted undef to $PDL::undefval (%g) %ld time%s\\n",undefval,undef_count,undef_count==1?"":"s");
      fflush(stderr);
    }
  }

  return undef_count;
}


!WITH!SUBS!

  } # end type loop
