
/* 
   Io.xs   Under construction

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */
 
#include "pdlio.h"     /* Local decs */

/* Return a integer or numeric scalar as approroate */

#define SET_RETVAL_NV x.datatype<PDL_F ? (RETVAL=newSViv( (IV)result )) : (RETVAL=newSVnv( result ))

static Core* PDL; /* Structure hold core C functions */
SV* CoreSV;       /* Get's pointer to perl var holding core structure */

MODULE = PDL::Io     PACKAGE = PDL::Io

###########################################################################

#### IMPORTANT - typemap of 'pdl' is different in this module! ####

# Determine endianness of machine

int
isbigendian()
   CODE:
     unsigned short i;
     unsigned char *b;

     i = 42; b = (unsigned char*) (void*) &i;
  
     if (*b == 42) 
        RETVAL = 0;
     else if (*(b+1) == 42) 
        RETVAL = 1;
     else
         croak("Impossible - machine is neither big nor middle endian!!\n");
     OUTPUT:
       RETVAL

# byte swap every 2 bytes

void
bswap2(x)
   pdl*	x;
   CODE:
      int i;
      short *aa = (short*) x->data; short bb;
      unsigned char *a,*b;
      int n = x->nvals * PDL->howbig(x->datatype) / sizeof(short);
      for(i=0;i<n; i++) {
         bb = aa[i]; a = (unsigned char*) (void*) (aa+i);  
         b = (unsigned char*) &bb;
         *a = *(b+1);  *(a+1) = *b; 
     }

# byte swap every 4 bytes

void
bswap4(x)
   pdl*	x;
   CODE:
      int i;
      PDL_Long *aa = (PDL_Long*) x->data; PDL_Long bb;
      unsigned char *a,*b;
      int n = x->nvals * PDL->howbig(x->datatype) / sizeof(PDL_Long);
      for(i=0;i<n; i++) {
         bb = aa[i]; a = (unsigned char*) (void*) (aa+i);  
         b = (unsigned char*) &bb;
         *a = *(b+3);  *(a+1) = *(b+2);  *(a+2) = *(b+1); *(a+3) = *b; 
     }
  


# byte swap every 8 bytes

void
bswap8(x)
   pdl*	x;
   CODE:
      int i;
      double *aa = (double*) x->data; double bb;
      unsigned char *a,*b;
      int n = x->nvals * PDL->howbig(x->datatype) / sizeof(double);
      for(i=0;i<n; i++) {
         bb = aa[i]; a = (unsigned char*) (void*) (aa+i);  
         b = (unsigned char*) &bb;
         *a     = *(b+7);  *(a+1) = *(b+6);  *(a+2) = *(b+5); *(a+3) = *(b+4); 
         *(a+4) = *(b+3);  *(a+5) = *(b+2);  *(a+6) = *(b+1); *(a+7) = *b; 
     }
  
###########################################################################

BOOT:

   /* Get pointer to structure of core shared C routines */

   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
   if (CoreSV==NULL)
      croak("This module requires use of PDL::Core first");

   PDL = (Core*) (void*) SvIV( CoreSV );  /* Core* value */

