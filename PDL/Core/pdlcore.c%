
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

pdl SvPDLV ( SV* sv ) {

   HV*  hash;
   SV** foo;
   pdl  ret;
   STRLEN len;

   ret.sv = (void*) sv;

   if ( !SvROK(sv) ) {   /* Coerce scalar */
       if ( ((SvIOK(sv) && !SvNOK(sv))) || !SvNIOK(sv) ) { /* Int */
          ret.datatype = PDL_L;
          ret.data     = pdl_malloc(pdl_howbig(ret.datatype));
          *((int*)ret.data) = (int) SvIV(sv);
       }
       else {
          ret.datatype = PDL_D;
          ret.data     = pdl_malloc(pdl_howbig(ret.datatype));
          *((double*)ret.data) = SvNV(sv);
       }
       ret.dims  = (int*) pdl_malloc(sizeof(int));
       *(ret.dims) = 1; 
       ret.ndims = 1;
       ret.nvals = 1;
       return ret;
   }
       
   if (SvTYPE(SvRV(sv)) != SVt_PVHV)
      croak("Error - argument is not a recognised data structure"); 

   hash = (HV*) SvRV(sv); 

   /* Transfer data items to return value */

   foo = hv_fetch( hash, "Data", strlen("Data"), 0);

   if (foo == NULL)
      croak("Error accessing Object 'Data' component");

   ret.data = SvPV( *foo, len ) ;

   foo = hv_fetch( hash, "Datatype", strlen("Datatype"), 0);
   if (foo == NULL)
      croak("Error accessing Object 'Datatype' component");

   ret.datatype = (int) SvNV( *foo ) ;

   ret.nvals = len / pdl_howbig(ret.datatype); 

   /* Copy dimensions info */

   foo = hv_fetch( hash, "Dims", strlen("Dims"), 0);
   if (foo == NULL)
      croak("Error accessing Object 'Dims' component");

   ret.dims  = pdl_packdims( *foo, &(ret.ndims) ); /* Pack into PDL */
   if (ret.ndims > 0 && ret.dims == NULL)
      croak("Error reading 'Dims' component");
              
   return ret;

}


/* Make a new pdl object as a copy of an old one and return - implement by    
   callback to perl method "copy" or "new" (for scalar upgrade) */

SV* pdl_copy( pdl a, char* option ) {

   SV* retval;
   char meth[20];

   dSP ;   int count ;

   retval = newSVpv("",0); /* Create the new SV */

   ENTER ;   SAVETMPS ;   PUSHMARK(sp) ;

   /* Push arguments */

   if (sv_isobject((SV*)a.sv)) {
       XPUSHs((SV*)a.sv); 
       strcpy(meth,"copy");    
       XPUSHs(sv_2mortal(newSVpv(option, 0))) ;    
   }
   else{
       XPUSHs(perl_get_sv("PDL::name",FALSE)); /* Default object */
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




