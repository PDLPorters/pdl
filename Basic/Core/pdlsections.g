

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

PDL_Indx pdl_get_offset(PDL_Indx* pos, PDL_Indx* dims, PDL_Indx *incs, PDL_Indx offset, int ndims) {
   int i;
   PDL_Indx result;
   result = offset;
   for (i=0; i<ndims; i++) {
       result = result + (pos[i]+((pos[i]<0)?dims[i]:0))*incs[i];
   }
   return result;
}

/* Check validity of section - return number of elements in it */

PDL_Indx pdl_validate_section( PDL_Indx* sec, PDL_Indx* dims, int ndims ){

   PDL_Indx i,start,end,count;

   count=1;

   for(i=0;i<ndims;i++){

       if (dims[i]<=0 || ndims==0)    /* Never happens :-) */
           croak("PDL object has a dimension <=0 !");

       start = sec[2*i];
       end   = sec[2*i+1];

       if (start<0 || end<0 || start>end || end>=dims[i])
            croak("Invalid subsection specified");

       count = count * (end-start+1);
   }
   return count;
}

/* Increrement a position pointer array by one row */

void pdl_row_plusplus ( PDL_Indx* pos, PDL_Indx* dims, int ndims ) {

    int i, noescape;

    i=1; noescape=1;

    while(noescape) {

       (pos[i])++;

       if (pos[i]==dims[i]) { /* Carry */
          if (i>=(ndims)-1)  {
             noescape = 0; /* Exit */
          }else{
             pos[i]=0;
             i++;
          }
       }else{
          noescape = 0;    /* Exit */
       }
    }
}

/* Return value at position (x,y,z...) */

PDL_Anyval pdl_at( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, 
	PDL_Indx* incs, PDL_Indx offset, int ndims) {

    int i;
    PDL_Indx ioff;
    PDL_Anyval result = { -1, 0 };

    for(i=0; i<ndims; i++) { /* Check */

       /* leave pdl_get_offset to handle -ve offsets (i.e. from end of
          dimension), so elements of pos[] won't be changed */

       if(pos[i]<-dims[i] || pos[i]>=dims[i])
          croak("Position out of range");
    }

   ioff = pdl_get_offset(pos, dims, incs, offset, ndims);

   GENERICLOOP (datatype)

      generic *xx = (generic *) x;
      result.type = datatype;
      result.value.generic_ppsym = xx[ioff];

   ENDGENERICLOOP

   return result;
}

/* Set value at position (x,y,z...) */

void pdl_set( void* x, int datatype, PDL_Indx* pos, PDL_Indx* dims, PDL_Indx* incs, PDL_Indx offs, int ndims, PDL_Anyval value){

    int i;
    PDL_Indx ioff;

    for(i=0; i<ndims; i++) { /* Check */

       if(pos[i]<-dims[i] || pos[i]>=dims[i])
          croak("Position out of range");
    }

   ioff = pdl_get_offset(pos, dims, incs, offs, ndims);

   GENERICLOOP (datatype)

      generic *xx = (generic *) x;
      ANYVAL_TO_CTYPE(xx[ioff], generic, value);

   ENDGENERICLOOP
}



