
/*************************************************************** 

   pdlstats.c                                         

****************************************************************/

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

double pdl_min(void*x, int n, int datatype) {

    double val;

    GENERICLOOP(datatype)

       generic *xx = (generic *)x;
       int i = n; xx += n-1;
       val = (double) *xx;
       while (i--) {
          if ((double) *xx < val)
             val=(double) *xx;
          xx--;
       }

    ENDGENERICLOOP
    return val;
}

double pdl_max(void*x, int n, int datatype) {

    double val;

    GENERICLOOP(datatype)

       generic *xx = (generic *)x;
       int i = n; xx += n-1;
       val = (double) *xx;
       while (i--) {
          if ((double) *xx > val)
             val=(double) *xx;
          xx--;
        }

    ENDGENERICLOOP
    return val;
}

/* Sum a PDL array */

double pdl_sum(void*x, int n, int datatype) {

    double sum;

    sum = 0.0;

    GENERICLOOP(datatype)

       generic *xx = (generic *)x;
       int i = n; xx += n-1;
       while (i--) {            
          sum += (double) *xx;
          xx--;
       }
 
    ENDGENERICLOOP
    return sum;
}


