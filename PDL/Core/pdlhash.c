
/* pdlhash.c - functions for manipulating pdl hashes */


#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */


/* Retrieve cached pdl value from $$x{PDL} */

pdl* pdl_getcache( HV* hash ) {
   long address=0; /* previously int */ 
   SV** foo;

   if (hv_exists(hash, "PDL", strlen("PDL"))) {
      foo = hv_fetch( hash, "PDL", strlen("PDL"), 0);
      if (foo == NULL)
         croak("Unexpected error accessing Object 'PDL' component");
      address = SvIV(*foo);
   }
   return ( address==0? (pdl*) NULL : (pdl*) address );
}

/* Fills the cache associated with a PDL sv with new values from
   the PDL perl data strcuture */

pdl* pdl_fillcache( HV* hash ) {

   SV**   foo;
   SV *   bar;
   STRLEN len;
   int*   dims;
   int*   incs;
   int    ndims, nincs;
   pdl* thepdl = pdl_getcache( hash );

   if (thepdl == NULL ) { /* Doesn't exist so create out of the void */
      thepdl = pdl_new();

/*    hv_store(hash, "PDL", strlen("PDL"), newSViv((I32) thepdl),0);
      */
			hv_store(hash, "PDL", strlen("PDL"), newSViv((IV)thepdl),0); 
   }

   /* Transfer data items to return value */

   thepdl->data = SvPV(pdl_getKey( hash, "Data" ), len);

   thepdl->datatype = (int) SvNV( pdl_getKey( hash, "Datatype" ) ) ;

   thepdl->nvals = len / pdl_howbig(thepdl->datatype); 

   /* Copy dimensions info */

   foo = hv_fetch( hash, "Dims", strlen("Dims"), 0);
   if (foo == NULL)
      croak("Error accessing Object 'Dims' component");

   dims  = pdl_packdims( *foo, &ndims ); /* Pack into PDL */
   if (ndims> 0 && dims == NULL)
      croak("Error reading 'Dims' component");

   /* Fetch offset and increments, *if available* */

   foo = hv_fetch ( hash, "Incs", strlen("Incs"), 0);

   if(foo == NULL) {
      pdl_setdims(thepdl, dims, ndims, NULL);
   } else {
      incs = pdl_packdims( *foo, &nincs ); /* Pack */
      if(nincs != thepdl->ndims) 
         croak("NDIMS AND NINCS UNEQUAL!\n");

      pdl_setdims(thepdl, dims, ndims, incs);
   }

   foo = hv_fetch (hash, "Offs", strlen("Offs"), 0);
   if(foo == NULL) {
   	thepdl->offs = 0;
   } else {
   	thepdl->offs = SvIV( *foo );
   }

   /* Fetch ThreadDims and ThreadIncs *if available* */

   foo = hv_fetch ( hash, "ThreadDims", strlen("ThreadDims"), 0);
   if(foo == NULL)
   	thepdl->nthreaddims = 0;
   else
        dims = pdl_packdims(*foo, &ndims);
   

   if(thepdl->nthreaddims) {
      foo = hv_fetch ( hash, "ThreadIncs", strlen("ThreadIncs"), 0);
      if(foo == NULL)
         die("Threaddims but not ThreadIncs given!\n");
	
      incs = pdl_packdims (*foo, &nincs );
      if(nincs != thepdl->nthreaddims)  
         die("NThreaddims != NThreadIncs!\n");

      pdl_setthreaddims( thepdl, dims, ndims, incs);
    }
   return thepdl;
}

/* 
   Get $$x{Data} etc. allowing for dereferencing - note we don't need
   to provide pdl_setKey etc. because once we have the SV* if we change
   it then it is changed in the original hash.
*/

SV* pdl_getKey( HV* hash, char* key ) {

   SV**   foo;
   SV*    bar;
   
   foo = hv_fetch( hash, key, strlen(key), 0);

   if (foo == NULL)
      croak("Error accessing Object %s component", key);

   /* Now, if key is a reference, we need to dereference it */

   bar = *foo;

   while(SvROK(bar)) {
   	bar = SvRV(bar);
   }
   return bar;
}

