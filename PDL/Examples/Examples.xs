
/* 
   Examples.xs - simple examples module

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#include "pdlexamples.h"  /* Local decs */

/* Return a integer or numeric scalar as approroate */

#define SET_RETVAL_NV x.datatype<PDL_F ? (RETVAL=newSViv( (IV)result )) : (RETVAL=newSVnv( result ))

static Core* PDL; /* Structure hold core C functions */
SV* CoreSV;       /* Get's pointer to perl var holding core structure */

MODULE = PDL::Examples     PACKAGE = PDL::Examples

###########################################################################

# EXAMPLES SECTION. 

# fibonacci() is just an example of the mechanics of calling C and 
# writing generic code (see pdlexamples.g) - it just fills a 1D array
# with the Fibonacci series

SV *
fibonacci(x)
   pdl*	x
   CODE:
    pdl* y;
    if (x->ndims != 1) 
       croak("Array is not 1D");

    RETVAL = PDL->copy(x,"");   /* Init y value to return as copy of x */
    y = PDL->SvPDLV(RETVAL);    /* Map */

    pdl_fibonacci( y->data, y->datatype, y->nvals );     /* Do it */
   
    OUTPUT:
     RETVAL

# Connected 8-component labelling - amore complex 2D real world example

SV *
cc8compt(x)
   pdl*	x
   CODE:
    pdl* y;
    int* dims    = x->dims;
    int nx, ny;

    RETVAL = PDL->copy(x,""); /* Init y value to return as copy of x */
    y = PDL->SvPDLV(RETVAL);         /* Map */

    nx = *(y->dims); ny = y->ndims==2 ? *(y->dims+1) : 1; 

    /* Note: PDL->twod(pdl) returns void** ptr to 1D or 2D array */

    pdl_cc8compt( PDL->twod(y), y->datatype, nx, ny); /* do it */

   OUTPUT:
     RETVAL


###########################################################################

BOOT:

   /* Get pointer to structure of core shared C routines */

   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
   if (CoreSV==NULL)
      croak("This module requires use of PDL::Core first");

   PDL = (Core*) (void*) SvIV( CoreSV );  /* Core* value */

