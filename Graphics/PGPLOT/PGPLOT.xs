
/* 
   PGPLOT.xs  

   A few routines in C to speed up PDL access to PGPLOT primitives.

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

struct PGPLOT_function_handle {
   I32 binversion;
   void (*cpgmove) (float x, float y);
   void (*cpgdraw) (float x, float y);
};


typedef struct PGPLOT_function_handle PGPLOT_function_handle;

static I32 PGPLOT_structure_version = 20000302;  /* The date the PGPLOT structure changed */
static PGPLOT_function_handle  *myhandle;
SV *ptr;

MODULE = PDL::Graphics::PGPLOT     PACKAGE = PDL::Graphics::PGPLOT 

void
pggapline(n,msgval,xpts,ypts)
  int	n
  float msgval
  float *	xpts
  float *	ypts
  CODE:
    { int i;
      int start = 0;
      while (xpts[start] == msgval) start++;  /* make sure we have a good starting point */
      myhandle->cpgmove (xpts[start], ypts[start]);
      for (i=start+1;i<n;i++) {
        if (ypts[i] == msgval) {
           /* check we are not at end of array and we don't move to a missing value */
           if (i != n-1 && ypts[i+1] != msgval) { 
             myhandle->cpgmove (xpts[i+1], ypts[i+1]);
           }
        }
        else { 
          myhandle->cpgdraw (xpts[i], ypts[i]);	
        }
      }
    }
    
BOOT:

	/* Get pointer to structure of core shared C routines */
	ptr = perl_get_sv("PGPLOT::HANDLE",FALSE);  /* SV* value */
	if (ptr==NULL)
	  Perl_croak("This module requires use of PGPLOT first");
	myhandle = (PGPLOT_function_handle*) SvIV( ptr );  

	/* If the structure read from the PGPLOT module is too old */
	if (myhandle->binversion < PGPLOT_structure_version)
          Perl_croak("This module requires PGPLOT with a newer structure version");


