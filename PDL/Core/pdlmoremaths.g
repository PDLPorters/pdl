
/*************************************************************** 

   pdlmoremaths.c                                        

****************************************************************/


#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */


/* Convolve image with kernel (real space so only speedy for small kernel) */

void pdl_convolve (pdl* c, pdl* a, pdl* b) {
   
   int *dimsa = a->dims; 
   int *dimsb = b->dims;
   int *pos,*off;
   
   int i,j,k,n,offcen,cen,ncen,nrow;
   
   if (a->ndims != b->ndims) 
      croak("Arguments do not have the same dimensionality");
   for(i=0; i<a->ndims; i++)
         if (dimsb[i]>dimsa[i]) 
             croak("Second argument must be smaller in all dimensions that first");
 
   pos = (int*) pdl_malloc( a->ndims * sizeof(int) ); /* Init pos[] */
   for (i=0; i<a->ndims; i++) /* Zero */
       pos[i]=0;

   /* Find middle pixel in b */
   i=0; nrow = dimsb[0];
   while(i<b->nvals) {
      for (j=0; j<nrow; j++) { /* For each row */
           pos[0]=j;

           for(k=0;k<b->ndims;k++) {       /* Is centre? */
               if (pos[k] != dimsb[k]/2)
                   goto getout;
           }
           ncen = i;
getout:    i++;
      }
      pos[0]=0;
      pdl_row_plusplus( pos, dimsb, b->ndims );
   }

   for (i=0; i<a->ndims; i++) /* Zero */
       pos[i]=0;
   
   /* Initialise offset array to handle the relative coords efficiently */

   off = (int*) pdl_malloc(b->nvals*sizeof(int)); /* Offset array */

   i=0; 
   while(i<b->nvals) {
      n = pdl_get_offset(pos, dimsa, a->ndims); /* Start of row in A */
      for (j=0; j<nrow; j++) { /* Fill row */
           off[i] = n+j;
           if (i==ncen) 
              offcen = off[i]; /* Offset to middle */
           i++;
      }
      pdl_row_plusplus( pos, dimsa, a->ndims );
   }

   for(i=0;i<b->nvals;i++)    /* Subtract center offset */
       off[i]=offcen-off[i]; 

   /* Now convolve the data */

   GENERICLOOP (a->datatype)
   
     generic *aa = (generic*) a->data;
     generic *bb = (generic*) b->data;
     generic *cc = (generic*) c->data;
   
     for(i=0; i<a->nvals; i++) {
        cc[i] = 0;
        for(j=0; j<b->nvals; j++) 
            cc[i] += aa[ (i+off[j]+a->nvals) % a->nvals ] * bb[j] ;
     }

   ENDGENERICLOOP
}

/* 
   Calculate histogram of data - note values outside the range
   are put in the first/last bins
*/

void pdl_hist (pdl* c, pdl* a, double min, double step) {
   
   int i,j;
   int size;

   GENERICLOOP (a->datatype)

        generic *cc = (generic *)(c->data); generic *aa = (generic*)(a->data);         
 
        j=c->nvals; cc+=j-1; /* Point to last element */
        while(j--)    
           *cc-- = 0; 

        cc=(generic *)(c->data); /* Point back to start */
        i = a->nvals;
        aa += i-1;

        while(i--) {
           j = (int) ( ( (double)(*aa) - min )/step );
           if (j<0) 
              j=0;
           if (j>=c->nvals)
              j=c->nvals-1;
           (cc[j])++;
           aa--;
        } /* End while */

   ENDGENERICLOOP
}


/* Matrix multiplication */

void pdl_matrixmult( pdl *c, pdl* a, pdl* b) {

   int i,j,k,m1,m2,m3,n1,n2,n3;

   if (a->ndims>2 || b->ndims>2) 
      croak("Matrix multiplication only valid for 1-2 dimensional data");

   m1 = *(a->dims); n1 = a->ndims==2 ? *(a->dims+1) : 1;
   m2 = *(b->dims); n2 = b->ndims==2 ? *(b->dims+1) : 1;

   if (m1!=n2) 
      croak("Matrix dimensions do not correspond appropriately for matrix multiplication");

   m3 = m2; n3 = n1;

   pdl_grow(c, m3*n3);
   *(c->dims) = m3; *(c->dims+1) = n3;
   c->ndims = n3==1 ? 1 : 2;
   
   GENERICLOOP (c->datatype)

      generic **cc = (generic **) pdl_twod(c);
      generic **aa = (generic **) pdl_twod(a);
      generic **bb = (generic **) pdl_twod(b);

      for(j=0; j<n3; j++) {
         for(i=0; i<m3; i++) {
             cc[j][i] = 0.0;
             for (k=0; k<m1; k++) {
                cc[j][i] += aa[j][k] * bb[k][i];
             }
          }
       }

   ENDGENERICLOOP
}



/* Matrix transpose */

void pdl_transpose(pdl*y, pdl* x) {

   int i,j,m1,n1,m2,n2;

   if (x->ndims>2) 
      croak("Matrix transpose only valid for 1-2 dimensional data");
   m1 = *x->dims; n1 = x->ndims==2 ? *(x->dims+1) : 1;

   m2 = n1; n2 = m1;
   y->ndims = 2;
   *(y->dims) = m2; *(y->dims+1) = n2;

   GENERICLOOP (y->datatype)

      generic **xx = (generic **) pdl_twod(x);
      generic **yy = (generic **) pdl_twod(y);

      for(j=0; j<n1; j++) {
         for(i=0; i<m1; i++) {
            yy[i][j] = xx[j][i];
         }
       }
             
    ENDGENERICLOOP
}
     

