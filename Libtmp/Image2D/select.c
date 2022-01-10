#include "EXTERN.h"
#include "perl.h"
#include "pdl.h"

/*
 * this routine is based on code referenced from
 * http://www.eso.org/~ndevilla/median/
 * the original algorithm is described in Numerical Recipes
*/

#define ELEM_SWAP(ctype,a,b) { register ctype t=(a);(a)=(b);(b)=t; }

/* for use with PDL_TYPELIST_REAL */
#define X(symbol, ctype, ppsym, ...) \
  ctype quick_select_ ## ppsym(ctype arr[], int n) \
  { \
    int low, high ; \
    int median; \
    int middle, ll, hh; \
    low = 0 ; high = n-1 ; median = (low + high) / 2; \
    for (;;) { \
        if (high <= low) /* One element only */ \
            return arr[median] ; \
        if (high == low + 1) {  /* Two elements only */ \
            if (arr[low] > arr[high]) \
                ELEM_SWAP(ctype, arr[low], arr[high]) ; \
            return arr[median] ; \
        } \
    /* Find median of low, middle and high items; swap into position low */ \
    middle = (low + high) / 2; \
    if (arr[middle] > arr[high])    ELEM_SWAP(ctype, arr[middle], arr[high]) ; \
    if (arr[low] > arr[high])       ELEM_SWAP(ctype, arr[low], arr[high]) ; \
    if (arr[middle] > arr[low])     ELEM_SWAP(ctype, arr[middle], arr[low]) ; \
    /* Swap low item (now in position middle) into position (low+1) */ \
    ELEM_SWAP(ctype, arr[middle], arr[low+1]) ; \
    /* Nibble from each end towards middle, swapping items when stuck */ \
    ll = low + 1; \
    hh = high; \
    for (;;) { \
        do ll++; while (arr[low] > arr[ll]) ; \
        do hh--; while (arr[hh]  > arr[low]) ; \
        if (hh < ll) \
        break; \
        ELEM_SWAP(ctype, arr[ll], arr[hh]) ; \
    } \
    /* Swap middle item (in position low) back into correct position */ \
    ELEM_SWAP(ctype, arr[low], arr[hh]) ; \
    /* Re-set active partition */ \
    if (hh <= median) \
        low = ll; \
        if (hh >= median) \
        high = hh - 1; \
    } \
  }
PDL_TYPELIST_REAL(X)
#undef ELEM_SWAP
#undef X
