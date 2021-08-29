

/***************************************************************

   pdlsections.c

****************************************************************/

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */



/*
   Code for subsection handling - extraction/insertion. Works
   for arbitrary dimensionality of data.
*/


/* Compute offset of (x,y,z,...) position in row-major list */

PDL_Indx pdl_get_offset(PDL_Indx* pos, PDL_Indx* dims, PDL_Indx *incs, PDL_Indx offset, PDL_Indx ndims) {
   PDL_Indx i;
   PDL_Indx result;
   for(i=0; i<ndims; i++) { /* Check */
      if(pos[i]<-dims[i] || pos[i]>=dims[i])
         croak("Position out of range");
   }
   result = offset;
   for (i=0; i<ndims; i++) {
       result = result + (pos[i]+((pos[i]<0)?dims[i]:0))*incs[i];
   }
   return result;
}

/* wrapper for pdl_at where only want first item, cf sclr_c */
PDL_Anyval pdl_at0( pdl* it ) {
    PDL_Indx nullp = 0;
    PDL_Indx dummyd = 1;
    PDL_Indx dummyi = 1;
    pdl_make_physvaffine( it );
    if (it->nvals < 1)
       croak("ndarray must have at least one element");
    return pdl_at(PDL_REPRP(it), it->datatype, &nullp, &dummyd,
            &dummyi, PDL_REPROFFS(it),1);
}

/* Return value at position (x,y,z...) */

PDL_Anyval pdl_at( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, 
	PDL_Indx* incs, PDL_Indx offset, PDL_Indx ndims) {
   PDL_Anyval result = { -1, 0 };
   PDL_Indx ioff = pdl_get_offset(pos, dims, incs, offset, ndims);
   GENERICLOOP (datatype)
      generic *xx = (generic *) x;
      result.type = datatype;
      result.value.generic_ppsym = xx[ioff];
   ENDGENERICLOOP
   return result;
}

/* Set value at position (x,y,z...) */

void pdl_set( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, PDL_Indx* incs, PDL_Indx offs, PDL_Indx ndims, PDL_Anyval value){
   PDL_Indx ioff = pdl_get_offset(pos, dims, incs, offs, ndims);
   GENERICLOOP (datatype)
      generic *xx = (generic *) x;
      ANYVAL_TO_CTYPE(xx[ioff], generic, value);
   ENDGENERICLOOP
}
