#
# Create pdlconv.c 
# - for many different datatypes

use strict;
 
use Config;
use File::Basename qw(&basename &dirname);
require './Dev.pm'; PDL::Core::Dev->import;
PDL::Core::Dev::loadmod_Types();

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

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

!NO!SUBS!

# these 2 routines shouldn't need to be changed to handle
# bad values, since all they do is copy data from
# one ndarray to another of the same type
# (assuming no per-ndarray bad values)
#

for(['readdata_vaffine', "*ap = *pp"],
    ['writebackdata_vaffine', "*pp = *ap"]) {

    my $name = $_->[0];
    my $code = $_->[1];

print OUT <<"!WITH!SUBS!";

void pdl_${name}(pdl *a) {
	PDL_Indx i;
	int j;
	int intype = a->datatype;
	if(!PDL_VAFFOK(a)) {
		die("pdl_$name without vaffine");
	}
	PDL_ENSURE_ALLOCATED(a);
	switch ( intype ) {
!WITH!SUBS!

##### Generate code for each data type #####

for my $t ( PDL::Types::types() ) {
    my ($in, $intype) = map $t->$_, qw(sym ctype);
    print OUT <<"!WITH!SUBS!";
    case ${in}:
      {
	$intype *ap = ($intype *) a->data;
	$intype *pp = ($intype *) a->vafftrans->from->data;
	pp += a->vafftrans->offs;
	for(i=0; i<a->nvals; i++) {
	    ${code};
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
      }
      break;

!WITH!SUBS!

} #### End of perl loop ####

#    default:
#      die("pdl_$name does not recognise the datatype");

print OUT <<'!NO!SUBS!';

    } /* switch( intype ) */

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
    STRLEN   nbytes;
    int   diffsize;
    PDL_Indx   i;
    PDLDEBUG_f(printf("pdl_converttype %p, %d, %d, %d\n", (void*)a, a->datatype,
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
         a->data     = pdl_smalloc(nbytes); /* Space for changed data */
      }
      else{
         b = a->data; /* In place */
      }

    }else{
       die("Sorry, temporary type casting is not allowed now");
    }

    /* Do the conversion as nested switch statements */
#define X_OUTER(datatype_out, generic_out, generic_ppsym_out, shortctype_out) \
    generic_out *bb = (generic_out *) b; \
    i = a->nvals; \
    PDL_GENERICSWITCH2(targtype, X_INNER);
#define X_INNER(datatype_in, generic_in, generic_ppsym_in, shortctype_in) \
    generic_in *aa = (generic_in *) a->data; \
    aa += i-1; bb += i-1; \
    while (i--) \
      *aa-- = (generic_in) *bb--;
    PDL_GENERICSWITCH(intype, X_OUTER);
#undef X_INNER
#undef X_OUTER

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
!NO!SUBS!
