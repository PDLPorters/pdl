

/***************************************************************

   pdlsections.c

****************************************************************/

#define PDL_CORE      /* For certain ifdefs */
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */



/*
   Code for subsection handling - extraction/insertion. Works
   for arbitrary dimensionality of data.
*/


/* Compute offset of (x,y,z,...) position in row-major list */

int pdl_get_offset(PDL_Long* pos, PDL_Long* dims, PDL_Long *incs, PDL_Long offset, int ndims) {
   int i;
   int result;
   result = offset;
   for (i=0; i<ndims; i++) {
       result = result + (pos[i]+((pos[i]<0)?dims[i]:0))*incs[i];
   }
   return result;
}

/* Check validity of section - return number of elements in it */

int pdl_validate_section( int* sec, int* dims, int ndims ){

   int i,start,end,count;

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

void pdl_row_plusplus ( int* pos, int* dims, int ndims ) {

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

/* Take the N-dimensional subsection of an N-dimensional array */

#ifdef FOOBAR

void pdl_subsection( char *y, char*x, int datatype, int* sec,
                     int* dims, int *incs, int offs, int* ndims) {


   /* Note dims, ndims are altered and returned to reflect the new section */

   int *start,*end;
   int i,n1,n2,nrow,count,dsize;

   /* Seperate section into start and end arrays - KISS! */

   start = (int *) pdl_malloc( (*ndims)*sizeof(int) );
   end   = (int *) pdl_malloc( (*ndims)*sizeof(int) );

   if (start == NULL || end == NULL)
       croak("Out of memory");

   for(i=0;i<*ndims;i++){
       start[i] = sec[2*i];
       end[i]   = sec[2*i+1];
   }

   n1 = pdl_get_offset(start, dims, incs, offs, *ndims); /* Start pos */
   n2 = pdl_get_offset(end,   dims, incs, offs, *ndims); /* End   pos */

   dsize = pdl_howbig(datatype); /* Size of item */

   nrow = end[0]-start[0]+1; /* Size of a row chunk */
   count = 0;                /* Transfer count */

   while(n1<=n2) {

       memcpy( y+count*dsize, x+n1*dsize, nrow*dsize ); /* Copy row */
       count += nrow;
       if (*ndims<2)
           break;
       pdl_row_plusplus( start, dims, *ndims ); /* Incr start[] one row */
       n1 = pdl_get_offset(start, dims, incs, offs, *ndims); /* New pos */

    }

    /* Calculate new dimensions */

    for(i=0;i<*ndims;i++)
       dims[i] = sec[2*i+1]-sec[2*i]+1;

   /* Remove trailing degenerate unary dimensions */

    while( (*ndims)>1 && dims[(*ndims)-1] == 1 )
         (*ndims)--;

   /* Remove leading degenerate unary dimensions */

    while( (*ndims)>1 && *dims == 1 ) {
         for(i=0;i<(*ndims)-1;i++)
            dims[i]=dims[i+1];    /* Shuffle down */
         (*ndims)--;
    }
}

/* Insert one N-dimensional array in another */

void pdl_insertin( char*y, int* ydims, int nydims,
                   char*x, int* xdims, int nxdims,
                   int datatype, int* pos) {

   /* Note inserts x[] in y[] */

   int i,nyvals,nxvals,n1,n2,nrow,ntran,dsize;

   nyvals = 1; nxvals = 1;

   for(i=0; i<nydims; i++) { /* Check position */

     nyvals *= ydims[i]; /* Compute total elements */

     if(pos[i]<0 || pos[i]>=ydims[i])
        croak("Position out of range");
   }

   nxvals = 1;
   for(i=0; i<nxdims; i++) /* Compute total elements */
     nxvals *= xdims[i];


   n1 = pdl_get_offset(pos, ydims, yincs, offset, nydims); /* Start pos in Y */
   n2 = 0;                             /* Start pos in X */

   nrow = xdims[0];               /* Size of a row chunk */
   ntran = nrow;                  /* Amount to transfer per row */
   if (pos[0]+nrow-1 > ydims[0])  /* Edge overflow */
       ntran = ydims[0]-pos[0];

   dsize = pdl_howbig(datatype);

   while(n2<nxvals) { /* Copy those bytes... */

       memcpy( y+n1*dsize, x+n2*dsize, ntran*dsize );

       if (nydims<2)
          break;
       pdl_row_plusplus( pos, ydims, nydims); /* Incr pos[] one row */

       n1 = pdl_get_offset( pos, ydims, incs, nydims); /* New pos in Z */

       if (n1>=nyvals)  /* Off Y image */
          break;

       n2 += nrow;  /* New pos in X */

   }

}

#endif


/* Return value at position (x,y,z...) */

double pdl_at( void* x, int datatype, PDL_Long* pos, PDL_Long* dims, 
	PDL_Long *incs, PDL_Long offset, int ndims) {

    int i;
    double result;

    for(i=0; i<ndims; i++) { /* Check */

       /* leave pdl_get_offset to handle -ve offsets (i.e. from end of
          dimension), so elements of pos[] won't be changed */

       if(pos[i]<-dims[i] || pos[i]>=dims[i])
          croak("Position out of range");
    }

   i = pdl_get_offset(pos, dims, incs, offset, ndims);

   GENERICLOOP (datatype)

      generic *xx = (generic *) x;
      result = (double)xx[i];

   ENDGENERICLOOP

   return result;
}

/* Set value at position (x,y,z...) */

void pdl_set( void* x, int datatype, PDL_Long* pos, PDL_Long* dims, PDL_Long *incs, PDL_Long offs, int ndims, double value){

    int i;

    for(i=0; i<ndims; i++) { /* Check */

       if(pos[i]<-dims[i] || pos[i]>=dims[i])
          croak("Position out of range");
    }

   i = pdl_get_offset(pos, dims, incs, offs, ndims);

   GENERICLOOP (datatype)

      generic *xx = (generic *) x;
      xx[i] = value;

   ENDGENERICLOOP
}



