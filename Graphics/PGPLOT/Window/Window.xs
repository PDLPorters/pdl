
/* 
   PGPLOT.xs  

   A few routines in C to speed up PDL access to PGPLOT primitives.

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#include "ppport.h"   /* for backwards comaptibility */

struct PGPLOT_function_handle {
   I32 binversion;
   void (*cpgmove) (float x, float y);
   void (*cpgdraw) (float x, float y);
   void (*cpgqcir) (int *icilo, int *icihi);
   void (*cpgsci)  (int ci);
   void (*cpgpt1)  (float x, float y, int sym);                                 
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



void
pgcolorpnts(n,x,y,z,sym)
  int	n
  float *	x
  float *	y
  float *	z
  int   sym
  CODE:
    { 
      /* find range of color pallette */
      int icilo, icihi, i, cirange, ci;
      float minz, maxz, zrange;

      /* If the structure read from the PGPLOT module is too old */
      if (myhandle->binversion < PGPLOT_structure_version) {
	char msg[128];
        sprintf (msg, "This function requires PGPLOT with a structure version at least %d.\nPlease upgrade your PGPLOT package.", 
                       PGPLOT_structure_version);
		       
        Perl_croak(aTHX_ msg);
      }

      myhandle->cpgqcir(&icilo, &icihi);
      
      /* find min and max values of zpts variable */
      minz =  9.99e30;
      maxz = -9.99e30;
      for (i=0;i<n;i++) {
	if (z[i] < minz) minz = z[i];
	if (z[i] > maxz) maxz = z[i];
      }

      /* determine range of available z indices and range of input 'z' values */
      cirange = icihi - icilo;
      zrange  = maxz  - minz;

      /* printf ("cilo = %d, cihi = %d\n", icilo, icihi); */

      /* for each input point, compute a scaled color index and plot the point */
      for (i=0;i<n;i++) {
	ci = (int)(icilo + (z[i] - minz) * (float)(cirange/zrange)); 
	/* printf ("x = %f, y = %f, ci = %d\n", x[i], y[i], ci); */
	myhandle->cpgsci(ci);
	myhandle->cpgpt1(x[i], y[i], sym);
      }
    }


    
BOOT:

	/* Get pointer to structure of core shared C routines */
	ptr = perl_get_sv("PGPLOT::HANDLE",FALSE);  /* SV* value */
#ifndef aTHX_
#define aTHX_
#endif
	if (ptr==NULL)
	  Perl_croak(aTHX_ "This module requires PGPLOT version 2.16 or later.\nPlease install/upgrade PGPLOT (see the PDL/DEPENDENCIES file).");
	myhandle = INT2PTR(PGPLOT_function_handle*,SvIV( ptr ));  



