
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */


/* Size of data type information */

int pdl_howbig (int datatype) {
    switch (datatype) {
    case PDL_B:
      return 1;
    case PDL_S:
      return 2;
    case PDL_US:
      return 2;
    case PDL_L:
      return 4;
    case PDL_F:
      return 4;
    case PDL_D:
      return 8;
    default:
      croak("Unknown datatype code = %d",datatype);
    }
}

/* 
  "Convert" a perl SV into a pdl (alright more like a mapping as
   the data block isn't actually copied)  - scalars are automatically
   converted
*/

pdl* SvPDLV ( SV* sv ) {

   pdl* ret;
   int fake[1];
   HV* hash;

   if ( !SvROK(sv) ) {   /* Coerce scalar */

       ret = pdl_tmp();  /* Scratch pdl */

       ret->sv = (void*) sv;
       if ( ((SvIOK(sv) && !SvNOK(sv))) || !SvNIOK(sv) ) { /* Int */
          ret->datatype = PDL_L;
          ret->data     = pdl_malloc(pdl_howbig(ret->datatype));
          *((int*)ret->data) = (int) SvIV(sv);
       }
       else {
          ret->datatype = PDL_D;
          ret->data     = pdl_malloc(pdl_howbig(ret->datatype));
          *((double*)ret->data) = SvNV(sv);
       }
       *fake = 1;  /* Number of dims of scalar */
       pdl_setdims(ret, fake, 1, NULL);
       ret->nthreaddims=0;
       ret->nvals = 1;
       return ret;
   }
       
   if (SvTYPE(SvRV(sv)) != SVt_PVHV)
      croak("Error - argument is not a recognised data structure"); 

   hash = (HV*) SvRV(sv); 

   /* Check for existence of PDL cache - i.e. $$x{PDL} exists and !=0  */

   ret = pdl_getcache( hash );

   if (ret != NULL ) { /* Does exist so return cached value */

      ret->sv = (void*) sv; /* This value can never be cached! */
      return ret ; 
   }
   
   ret = pdl_fillcache( hash ); /* Cache value and return */ 

   ret->sv = (void*) sv; /* This value can never be cached! */
  
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

   if (sv_isobject((SV*)a->sv)) {
       XPUSHs((SV*)a->sv); 
       strcpy(meth,"copy");    
       XPUSHs(sv_2mortal(newSVpv(option, 0))) ;    
   }
   else{
       XPUSHs(perl_get_sv("PDL::name",FALSE)); /* Default object */
       XPUSHs((SV*)a->sv);   /* Value */
       strcpy(meth,"new");    
   }

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

int* pdl_packdims ( SV* sv, int *ndims ) {

   SV*  bar;
   AV*  array;
   int i;
   int *dims;

   if (!(SvROK(sv) && SvTYPE(SvRV(sv))==SVt_PVAV))  /* Test */
       return NULL;

   array = (AV *) SvRV(sv);   /* dereference */
  
   *ndims = (int) av_len(array) + 1;  /* Number of dimensions */

   if ( (*ndims)==0 )
      return NULL;

   dims = (int*) pdl_malloc( (*ndims) * sizeof(int) ); /* Array space */
   if (dims == NULL)
      croak("Out of memory");

   bar = sv_newmortal(); /* Scratch variable */

   for(i=0; i<(*ndims); i++) {
      bar = *(av_fetch( array, i, 0 )); /* Fetch */
      dims[i] = (int) SvIV(bar); 
   }
   return dims;
} 

/* unpack dims array into PDL SV* */

void pdl_unpackdims ( SV* sv, int *dims, int ndims ) {

   AV*  array;
   SV** foo;
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

/*
   pdl_malloc - utility to get temporary memory space. Uses
   a mortal *SV for this so it is automatically freed when the current
   context is terminated without having to call free(). Naughty but
   nice!
*/


void* pdl_malloc ( int nbytes ) {
   
   SV* work;
   
   work = sv_2mortal(newSVpv("", 0));
   
   SvGROW( work, nbytes);
   
   return (void *) SvPV(work, na);
}



