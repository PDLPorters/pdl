

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

int pdl_get_offset(int* pos, int* dims, int ndims) {
   int i;
   int result,size;
   size = 1;
   result = 0;
   for (i=0; i<ndims; i++) {
       if (i>0)
          size = size*dims[i-1];
       result = result + pos[i]*size;
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


void pdl_subsection( char *y, char*x, int datatype, int* sec, 
                     int* dims, int* ndims) {


   /* Note dims, ndims are altered and returned to reflect the new section */

   int *start,*end;
   int i,n1,n2,nrow,noescape,count,dsize;

   /* Seperate section into start and end arrays - KISS! */

   start = (int *) pdl_malloc( (*ndims)*sizeof(int) );
   end   = (int *) pdl_malloc( (*ndims)*sizeof(int) );

   if (start == NULL || end == NULL)
       croak("Out of memory");

   for(i=0;i<*ndims;i++){
       start[i] = sec[2*i];
       end[i]   = sec[2*i+1];
   }

   n1 = pdl_get_offset(start, dims, *ndims); /* Start pos */
   n2 = pdl_get_offset(end,   dims, *ndims); /* End   pos */

   dsize = pdl_howbig(datatype); /* Size of item */

   nrow = end[0]-start[0]+1; /* Size of a row chunk */
   count = 0;                /* Transfer count */

   while(n1<=n2) {

       memcpy( y+count*dsize, x+n1*dsize, nrow*dsize ); /* Copy row */
       count += nrow;
       if (*ndims<2)
           break;
       pdl_row_plusplus( start, dims, *ndims ); /* Incr start[] one row */
       n1 = pdl_get_offset(start, dims, *ndims); /* New pos */

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


   n1 = pdl_get_offset(pos, ydims, nydims); /* Start pos in Y */
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

       n1 = pdl_get_offset( pos, ydims, nydims); /* New pos in Z */

       if (n1>=nyvals)  /* Off Y image */
          break;         

       n2 += nrow;  /* New pos in X */

   }

}


/* Return value at position (x,y,z...) */

double pdl_at( void* x, int datatype, int* pos, int* dims, int ndims) {

    int i;
    double result;

    for(i=0; i<ndims; i++) { /* Check */

       if(pos[i]<0 || pos[i]>=dims[i])
          croak("Position out of range");
    }
   
   i = pdl_get_offset(pos, dims, ndims);

   GENERICLOOP (datatype) 

      generic *xx = (generic *) x;
      result = (double)xx[i];

   ENDGENERICLOOP

   return result;
}

/* Set value at position (x,y,z...) */

void pdl_set( void* x, int datatype, int* pos, int* dims, int ndims, double value){

    int i;

    for(i=0; i<ndims; i++) { /* Check */

       if(pos[i]<0 || pos[i]>=dims[i])
          croak("Position out of range");
    }
   
   i = pdl_get_offset(pos, dims, ndims);

   GENERICLOOP (datatype)

      generic *xx = (generic *) x;
      xx[i] = value;

   ENDGENERICLOOP
}



/* Fill array with the corresponding coordinate (1=X, 2=Y, etc..) */

void pdl_axisvals( pdl* a, int axis ) {
   
   int i,j;
   int *dims, *data, *pos, nrow;
   
   dims = (int*) a->dims;

   GENERICLOOP (a->datatype)

      generic *data = (generic*) a->data;
   
      pos = (int*) pdl_malloc( a->ndims * sizeof(int) );
      for (i=0; i<a->ndims; i++) /* Init */
           pos[i]=0;
      
      /* For each pixel */
      
      i=0; nrow = dims[0];
      while(i<a->nvals) {
         for (j=0; j<nrow; j++) { /* Fill row */
              pos[0]=j;
              data[i] =  pos[axis];
              i++;
         }
         pos[0]=0;
         if (a->ndims<2) 
            break;
         pdl_row_plusplus( pos, dims, a->ndims );
      }
   
  ENDGENERICLOOP
}




