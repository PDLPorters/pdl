#!/usr/local/bin/perl

# Perl utility to generate pdlconv.c automatically
# for many different datatypes

require 'Dev.pm'; PDL::Core::Dev->import;

# $date = `date`; chop $date;

##### HEADER ######

print <<EOD;


/***************************************************************

   pdlconv.c

****************************************************************/

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

EOD

for([readdata_vaffine, "*ap = *pp"],
    [writebackdata_vaffine, "*pp = *ap"]) {

print <<EOD;

void pdl_$_->[0](pdl *a) {
	PDL_Long *inds ;
	int i,j;
	int intype = a->datatype;
	if(!PDL_VAFFOK(a)) {
		die("Pdl_make_phys_from_vaffine without vaffine");
	}
	inds = malloc(sizeof(PDL_Long) * a->ndims);
	for(i=0; i<a->ndims; i++) {
		inds[i] = 0;
	}
	PDL_ENSURE_ALLOCATED(a);
	if(0) {

EOD

##### Generate code for each data type #####

for $in ( keys %PDL_DATATYPES ) {

    $intype = $PDL_DATATYPES{$in};
    print <<EOD;

    } else if (intype == $in)  {
       { $intype *ap = ($intype *) a->data;
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
       }

EOD

} #### End of perl loop ####

print <<'EOD';

	}

	free(inds);
}

EOD

} # End of outer perl loop

print <<'EOD';



/* Various conversion utilities for pdl data types */


/* Swap pdls */

void pdl_swap(pdl** a, pdl** b) {
   pdl* tmp;
   tmp = *b; *b=*a; *a=tmp;
}

/* Change the type of all the data in a pdl struct, either changing the
   original perl structure or making a temporary copy  */

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

       b = a->data;          /* Ptr to old data */
       a = pdl_tmp();        /* Brand new scratch pdl */
/*       pdl_clone(*aa,  a);  */ /* Copy old pdl entries */
       a->data     = pdl_malloc(nbytes); /* Space for changed data */
       *aa = a;              /* Change passed value to new address */

       die("Sorry, temporary type casting is not allowed now");
    }

    /* Do the conversion */

    if (0) {
        /* Nothing */

EOD

##### Generate code for each pair of data types #####

for $in ( keys %PDL_DATATYPES ) { for $targ ( keys %PDL_DATATYPES ) {

    next if $in eq $targ; # Skip duplicates

    $intype = $PDL_DATATYPES{$in}; $targtype = $PDL_DATATYPES{$targ};
    print <<EOD;

    } else if (intype == $in && targtype == $targ)  {
       { $intype *bb = ($intype *) b;
         $targtype *aa = ($targtype *) a->data;
         int i = a->nvals;
         aa += i-1; bb += i-1;
         while (i--)
                *aa-- = ($targtype) *bb--;
       }

EOD

}} #### End of perl loop ####

#### Trailer ####

print <<'EOD';

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


EOD
