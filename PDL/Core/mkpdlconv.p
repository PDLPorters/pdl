#!/usr/local/bin/perl

# Perl utility to generate pdlconv.c automatically
# for many different datatypes

use lib "../..";
use PDL::Core::Dev;

$date = `date`; chop $date;

##### HEADER ######

print <<EOD;


/*************************************************************** 

   pdlconv.c  

****************************************************************/

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

EOD

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
    STRLEN   len;
    int   diffsize;

    intype = a->datatype;

    if (intype == targtype) 
       return;

    diffsize = pdl_howbig(targtype) != pdl_howbig(a->datatype);

    nbytes = a->nvals * pdl_howbig(targtype); /* Size of converted data */

    if (changePerl) {   /* Grow data */

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
       pdl_clone(*aa,  a);   /* Copy old pdl entries */
       a->data     = pdl_malloc(nbytes); /* Space for changed data */
       *aa = a;              /* Change passed value to new address */
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

      hash = (HV*) SvRV( (SV*) a->sv );  /* Orig hash ref */

      /* Store new data */

      if (diffsize) {
         bar = pdl_getKey(hash, "Data");
         sv_setpvn( bar, (char*) a->data, nbytes );
         a->data = (void*) SvPV(bar, len);
      }

      /* Store new datatype */

      bar = pdl_getKey(hash, "Datatype");
      sv_setiv(bar, (IV) targtype);

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


/*  Utility to change the size of the data compt of a pdl */

void pdl_grow (pdl* a, int newsize) {

   SV* foo;
   HV* hash;
   int nbytes;
   int ncurr;
   STRLEN len;

   nbytes = newsize * pdl_howbig(a->datatype);
   ncurr  = SvCUR( (SV*)a->sv );
   if (ncurr == nbytes) 
      return;    /* Nothing to be done */

   hash = (HV*) SvRV( (SV*) a->sv ); 
   foo = pdl_getKey(hash, "Data");

   if (ncurr>nbytes)  /* Nuke back to zero */
      sv_setpvn(foo,"",0);
      
   SvGROW ( foo, nbytes );   SvCUR_set( foo, nbytes );
   a->data = SvPV( foo, len ); a->nvals = newsize;
}

/*  Utility to change the value of the data type field of a pdl  */

void pdl_retype (pdl* a, int newtype) {

   SV* foo;
   HV* hash; 

   if (a->datatype == newtype) 
      return;  /* Nothing to be done */

   hash = (HV*) SvRV( (SV*) a->sv ); 
   foo = pdl_getKey(hash, "Datatype");
   sv_setiv(foo, (IV) newtype);
   a->datatype = newtype;
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
