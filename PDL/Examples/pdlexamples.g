
#include <math.h>
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#include "pdlexamples.h"

/* 1D example = fill 1D array with the Fibonacci series */

void pdl_fibonacci( void*x, int pdl_datatype, int n )  {

   int i;
   
   GENERICLOOP(pdl_datatype)
   
      generic *xx = x;

      if (n>=1)   /* Intialisers */
         xx[0]=1;
      if (n>=2)
         xx[1]=1;
   
      for(i=2; i<n; i++) 
         xx[i] = xx[i-1] + xx[i-2];
   
   ENDGENERICLOOP

}

/*
   2D example -  pdl_cc8compt = Do connected components analysis based
   upon 8-connectivity, loosely based upon ideas in subroutine by Dave
   Eberly (sasdhe@unx.sas.com)
*/

void pdl_cc8compt( void **aa, int pdl_datatype, int nx, int ny ) {

   int i,j,k;
   int newlabel;
   int neighbour[4];
   int nfound;
   int pass,count,next,this;
   int *equiv;

   GENERICLOOP(pdl_datatype)
      generic **a = (generic**) aa;

      /* 1st pass counts max possible compts, 2nd records equivalences */
      
      for (pass = 0; pass<2; pass++) {
   
      if (pass==1) {
         equiv = (int*) malloc((newlabel+1)*sizeof(int));
         if (equiv==(int*)0) 
            croak("Out of memory");
         for(i=0;i<=newlabel;i++)
             equiv[i]=i;
      }
   
      newlabel = 1; /* Running label */
   
      for(j=0; j<ny; j++) { for(i=0; i<nx; i++) { /* Loop over image pixels */
   
             /* Note reversal of args - a[j][i] is pixel i,j in image */
   
            nfound = 0; /* Number of neighbour >0 */
   
            if (a[j][i] > 0) { /* Check 4 neighbour already seen */
   
               if (i>0 && a[j][i-1]>0)
                   neighbour[nfound++] = a[j][i-1]; /* Store label of it */
               if (j>0 && a[j-1][i]>0)
                   neighbour[nfound++] = a[j-1][i];
               if (j>0 && i>0  && a[j-1][i-1]>0)
                   neighbour[nfound++] = a[j-1][i-1];
               if (j>0 && i<nx && a[j-1][i+1]>0)
                   neighbour[nfound++] = a[j-1][i+1];
   
               if (nfound==0)  { /* Assign new label */
                  a[j][i] = newlabel++;
               }
               else {
                  a[j][i] =  neighbour[0]; 
                  if (nfound>1 && pass == 1) {  /* Assign equivalents */
                      for(k=1; k<nfound; k++) 
                         AddEquiv( equiv, a[j][i], neighbour[k] ); 
                  }
               }
            }
   
            else {  /* No label */
   
                a[j][i]=0; 
            }
   
      }} /* End of image loop */
   
      } /* Passes */
   
      /* Replace each cycle by single label */
   
       count = 0;
       for (i = 1; i <= newlabel; i++)
         if ( i <= equiv[i] ) {
             count++;
             this = i;
             while ( equiv[this] != i ) {
   	       next = equiv[this];
   	       equiv[this] = count;
   	       this = next;
             }
   	  equiv[this] = count;
         }
   
   
      /* Now remove equivalences */
   
      for(j=0; j<ny; j++) { for(i=0; i<nx; i++) { /* Loop over image pixels */
            a[j][i]  = equiv[ (int) a[j][i] ] ;
      }}
   
      free(equiv); /* Tidy */

   ENDGENERICLOOP

}



/* Add an equivalence to a list (used by pdl_cc8compt) */

void AddEquiv ( int* equiv, int i, int j) {

   int k, tmp;

   if (i==j) 
      return;

    k = j;
    do {
      k = equiv[k];
    } while ( k != j && k != i );

    if ( k == j ) {
       tmp = equiv[i];
       equiv[i] = equiv[j];
       equiv[j] = tmp;
    }
}
