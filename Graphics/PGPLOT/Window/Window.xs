
/* 
   PGPLOT.xs  

   A few routines in C to speed up PDL access to PGPLOT primitives.

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#ifndef PERL_PATCHLEVEL
#      ifndef __PATCHLEVEL_H_INCLUDED__
#              define PERL_PATCHLEVEL_H_IMPLICIT
#              include "patchlevel.h"
#      endif
#endif
#ifndef PERL_VERSION
/* Replace: 1 */
#      define PERL_VERSION PATCHLEVEL
/* Replace: 0 */
#endif
#ifndef PERL_SUBVERSION
/* Replace: 1 */
#      define PERL_SUBVERSION SUBVERSION
/* Replace: 0 */
#endif

#if (PERL_VERSION < 4) || ((PERL_VERSION == 4) && (PERL_SUBVERSION <= 5))
/* Replace: 1 */
#      define PL_sv_undef      sv_undef
#      define PL_sv_yes        sv_yes
#      define PL_sv_no         sv_no
#      define PL_na            na
#      define PL_stdingv       stdingv
#      define PL_hints         hints
#      define PL_curcop        curcop
#      define PL_curstash      curstash
#      define PL_copline       copline
#      define PL_Sv            Sv
/* Replace: 0 */
#endif

struct PGPLOT_function_handle {
   I32 binversion;
   void (*cpgmove) (float x, float y);
   void (*cpgdraw) (float x, float y);
};

typedef struct PGPLOT_function_handle PGPLOT_function_handle;

static I32 PGPLOT_structure_version = 20000302;  /* The date the PGPLOT structure changed */
static PGPLOT_function_handle  *myhandle;
SV *ptr;

MODULE = PDL::Graphics::PGPLOT::Window     PACKAGE = PDL::Graphics::PGPLOT::Window 

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
#ifndef aTHX_
#define aTHX_
#endif
	if (ptr==NULL)
	  Perl_croak(aTHX_ "This module requires PGPLOT version 2.16 or later.\nPlease install/upgrade PGPLOTLOT (see the PDL/DEPENDENCIES file).");
	myhandle = (PGPLOT_function_handle*) SvIV( ptr );  

	/* If the structure read from the PGPLOT module is too old */
	if (myhandle->binversion < PGPLOT_structure_version) {
	  char msg[128];
          sprintf (msg, "This module requires PGPLOT with a structure version at least %d", 
                         PGPLOT_structure_version);
          Perl_croak(aTHX_ msg);
        }


