/* pdlhash.c - functions for manipulating pdl hashes */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

/*  Utility to change the size of the data compt of a pdl */
void pdl_grow (pdl* a, PDL_Indx newsize) {
   SV* foo;
   HV* hash;
   STRLEN nbytes;
   STRLEN ncurr;
   STRLEN len;
   if(a->state & PDL_DONTTOUCHDATA) {
   	die("Trying to touch data of an untouchable (mmapped?) pdl");
   }
   if(a->datasv == NULL)
   	a->datasv = newSVpv("",0);
   foo = a->datasv;
   nbytes = ((STRLEN) newsize) * pdl_howbig(a->datatype);
   ncurr  = SvCUR( foo );
   if (ncurr == nbytes)
      return;    /* Nothing to be done */
   if(nbytes > (1024*1024*1024)) {
     SV *sv = get_sv("PDL::BIGPDL",0);
     if(sv == NULL || !(SvTRUE(sv)))
   	die("Probably false alloc of over 1Gb PDL! (set $PDL::BIGPDL = 1 to enable)");
     fflush(stdout);
   }
   (void)SvGROW ( foo, nbytes );
   SvCUR_set( foo, nbytes );
   a->data = (void *) SvPV( foo, len ); a->nvals = newsize;
}

/* unpack dims array into Hash */
void pdl_unpackarray ( HV* hash, char *key, PDL_Indx *dims, PDL_Indx ndims ) {
   AV*  array;
   PDL_Indx i;
   array = newAV();
   (void)hv_store(hash, key, strlen(key), newRV( (SV*) array), 0 );
   if (ndims==0 )
      return;
   for(i=0; i<ndims; i++)
         av_store( array, i, newSViv( (IV)dims[i] ) );
}
