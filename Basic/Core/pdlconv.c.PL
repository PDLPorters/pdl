#
# Create pdlconv.c 
# - for many different datatypes

use strict;
 
use Config;
use File::Basename qw(&basename &dirname);
 
require 'Dev.pm'; PDL::Core::Dev->import;
use vars qw( %PDL_DATATYPES );

# This forces PL files to create target in same directory as PL file.
# This is so that make depend always knows where to find PL derivatives.
chdir(dirname($0));
my $file;
($file = basename($0)) =~ s/\.PL$//;
$file =~ s/\.pl$//                                                              
    if ($Config{'osname'} eq 'VMS' or
	$Config{'osname'} eq 'OS2');  # "case-forgiving"
 
print "Extracting $file\n";
open OUT,">$file" or die "Can't create $file: $!";
chmod 0644, $file;

# $date = `date`; chop $date;

##### HEADER ######
 
print OUT <<"!WITH!SUBS!";
 
/***************************************************************

    pdlconv.c
    automatically created by pdlconv.c.PL

****************************************************************/

!WITH!SUBS!
 
print OUT <<'!NO!SUBS!';

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

!NO!SUBS!

# these 2 routines shouldn't need to be changed to handle
# bad values, since all they do is copy data from
# one piddle to another of the same type
# (assuming no per-piddle bad values)
#

for(['readdata_vaffine', "*ap = *pp"],
    ['writebackdata_vaffine', "*pp = *ap"]) {

print OUT <<"!WITH!SUBS!";

void pdl_$_->[0](pdl *a) {
	int i,j;
	int intype = a->datatype;
	if(!PDL_VAFFOK(a)) {
		die("pdl_$_ without vaffine");
	}
	PDL_ENSURE_ALLOCATED(a);
	if(0) {

!WITH!SUBS!

##### Generate code for each data type #####

for my $in ( keys %PDL_DATATYPES ) {

    my $intype = $PDL_DATATYPES{$in};
    print OUT <<"!WITH!SUBS!";

    } else if (intype == $in)  {
	$intype *ap = ($intype *) a->data;
	$intype *pp = ($intype *) a->vafftrans->from->data;
	pp += a->vafftrans->offs;
	for(i=0; i<a->nvals; i++) {
	    $_->[1];
	    for(j=0; j<a->ndims; j++) {
		pp += a->vafftrans->incs[j];
		if((j < a->ndims - 1 &&
		    (i+1) % a->dimincs[j+1]) ||
		   j == a->ndims - 1)
		    break;
		pp -= a->vafftrans->incs[j] *
		    a->dims[j];
	    }
	    ap ++;
	}

!WITH!SUBS!

} #### End of perl loop ####

print OUT <<'!NO!SUBS!';

	}

/***	free(inds); ***/
}

!NO!SUBS!

} # End of outer perl loop

print OUT <<'!NO!SUBS!';

/* Various conversion utilities for pdl data types */

/* Swap pdls */

void pdl_swap(pdl** a, pdl** b) {
   pdl* tmp;
   tmp = *b; *b=*a; *a=tmp;
}

/* Change the type of all the data in a pdl struct, either changing the
   original perl structure or making a temporary copy  */

/* 
 * it seems this does not have to be aware of bad values
 * (at least in the current scheme)
 */

void pdl_converttype( pdl** aa, int targtype, Logical changePerl ) {
    pdl* a=*aa;  /* Point to cache */
    int intype;
    void* b;     /* Scratch data ptr */
    SV*   bar;
    HV*   hash;
    int   nbytes;
    int   diffsize;
#if (PERL_VERSION >= 5) && (PERL_SUBVERSION >= 57)
    dXSARGS;
#endif

    PDLDEBUG_f(printf("pdl_converttype %d, %d, %d, %d\n", a, a->datatype,
    	targtype, changePerl);)

    intype = a->datatype;
    if (intype == targtype)
       return;

    diffsize = pdl_howbig(targtype) != pdl_howbig(a->datatype);

    nbytes = a->nvals * pdl_howbig(targtype); /* Size of converted data */

    if (changePerl) {   /* Grow data */

      if(a->state & PDL_DONTTOUCHDATA) {
	croak("Trying to convert of magical (mmaped?) pdl");
      }

      if (diffsize) {
         b = a->data;                      /* pointer to old data */
         a->data     = pdl_malloc(nbytes); /* Space for changed data */
      }
      else{
         b = a->data; /* In place */
      }

    }else{

       die("Sorry, temporary type casting is not allowed now");

       b = a->data;          /* Ptr to old data */
       a = pdl_tmp();        /* Brand new scratch pdl */
/*       pdl_clone(*aa,  a);  */ /* Copy old pdl entries */
       a->data     = pdl_malloc(nbytes); /* Space for changed data */
       *aa = a;              /* Change passed value to new address */
    }

    /* Do the conversion */

    if (0) {
        /* Nothing */

!NO!SUBS!

##### Generate code for each pair of data types #####

for my $in ( keys %PDL_DATATYPES ) { 
    for my $targ ( keys %PDL_DATATYPES ) {

    next if $in eq $targ; # Skip duplicates

    my $intype   = $PDL_DATATYPES{$in}; 
    my $targtype = $PDL_DATATYPES{$targ};

    print OUT <<"!WITH!SUBS!";

    } else if (intype == $in && targtype == $targ)  {
       { $intype *bb = ($intype *) b;
         $targtype *aa = ($targtype *) a->data;
         int i = a->nvals;
         aa += i-1; bb += i-1;
         while (i--)
                *aa-- = ($targtype) *bb--;
       }                                                                        

!WITH!SUBS!

}} #### End of perl loop ####

#### Trailer ####

print OUT <<'!NO!SUBS!';

    } else {
      croak("Don't know how to convert datatype %d to %d", intype, targtype);
    }

    if (changePerl) {   /* Tidy up */

      /* Store new data */

      if (diffsize) {
        STRLEN n_a;
         bar = a->datasv;
         sv_setpvn( bar, (char*) a->data, nbytes );
         a->data = (void*) SvPV(bar, n_a);
      }

    }

    a->datatype = targtype;
}


/* Ensure 'a' and 'b' are the same data types of high enough precision,
   using a reasonable set of rules.
*/

void pdl_coercetypes( pdl** aa, pdl** bb, Logical changePerl ) {

     pdl* a = *aa;  /* Double ptr passed as value of ptr may be changed to */
     pdl* b = *bb;  /* point at a temporary copy of the cached pdl */
     Logical oneisscalar;
     pdl *scalar,*vector;
     int targtype;

     if (a->datatype == b->datatype) /* Nothing to be done */
        return;

     /* Detect the vector & scalar case */

     oneisscalar = (a->nvals==1 || b->nvals==1) && !(a->nvals==1 && b->nvals==1);

     /* Rules for deciding what the target data type is */

     if (oneisscalar) {  /* Vector x Scalar case */

        scalar  = a; vector = b;
        if (b->nvals==1) {
           scalar = b;
           vector = a;
        }

        if (vector->datatype >= scalar->datatype) /* Vector more complex - easy */

           targtype = vector->datatype;

        else { /* Scalar more complex than vector- special rules to avoid
                  overzealous promotion of vector  */

           if (vector->datatype == PDL_F)  /* FxD is OK as F */
              targtype = vector->datatype;

           else if (vector->datatype <= PDL_L && scalar->datatype <= PDL_L)
              targtype = vector->datatype; /* two ints is OK as input int */

           else if (vector->datatype <= PDL_F && scalar->datatype==PDL_D)
              targtype = PDL_F; /* Only promote FOOxD as far as F */

           else
              targtype = scalar->datatype;

        }


     }else{ /* Vector x Vector - easy */

        targtype = a->datatype;
        if (b->datatype > a->datatype)
           targtype = b->datatype;

     }

     /* Do the conversion */

     pdl_converttype(aa, targtype, changePerl);
     pdl_converttype(bb, targtype, changePerl);
}

/* Given PDL return an allocated **ptr to 2D data thus allowing a[j][i] syntax */

void ** pdl_twod( pdl* x ) {

   int i,nx,ny,size;
   long *p;
   char *xx;

   if (x->ndims>2)
      croak("Data must be 1 or 2-dimensional for this routine");

   xx = (char*) x->data;

   nx = *(x->dims); ny = x->ndims==2 ? *(x->dims+1) : 1;

   size=pdl_howbig(x->datatype);

   p = (long*) pdl_malloc( ny*sizeof(long) ); /* 1D array of ptrs p[i] */
   for (i=0;i<ny;i++)
       p[i] = (long) &xx[i*nx*size];

   return (void**) p;
}

!NO!SUBS!
