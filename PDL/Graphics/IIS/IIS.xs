
/* 
   IIS.xs  - provide interface to KGB's libiis.c for
             IIS graphics (SAOimage/Ximtool etc...)
*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

#include "libiis.h"   /* Local decs */

static Core* PDL; /* Structure hold core C functions */
SV* CoreSV;       /* Get's pointer to perl var holding core structure */

MODULE = PDL::Graphics::IIS     PACKAGE = PDL::Graphics::IIS


###########################################################################

# Display image on an IIS device

void
iis_c(image,lo,hi)
   pdl	image
   float	lo
   float	hi
   CODE:
    int*  dims        = image.dims;
    int   frame       = (int)SvIV( perl_get_sv("iisframe", FALSE) );

    if (image.ndims != 2) 
       croak("Array is not 2D");
    if (frame<1 || frame>4)
       croak("$iisframe must be in range 1--4");

    iis_open(SvPV(perl_get_sv("fifi",FALSE),na),SvPV(perl_get_sv("fifo",FALSE),na), 
       (int)SvIV( perl_get_sv("fbconfig", FALSE) ),
       (int)SvIV( perl_get_sv("fb_x", FALSE) ), 
       (int)SvIV( perl_get_sv("fb_y", FALSE) ) );
    iis_display( image.data, image.datatype, dims[0],dims[1],lo,hi,frame );
    iis_close();

void
iiscur_c()
   PPCODE:
    float x,y;
    char ch;
    int   frame       = (int)SvIV( perl_get_sv("iisframe", FALSE) );

    iis_open(SvPV(perl_get_sv("fifi",FALSE),na),SvPV(perl_get_sv("fifo",FALSE),na),
       (int)SvIV( perl_get_sv("fbconfig", FALSE) ),
       (int)SvIV( perl_get_sv("fb_x", FALSE) ), 
       (int)SvIV( perl_get_sv("fb_y", FALSE) ) );
    iis_cur(&x,&y,&ch);
    iis_close();

    EXTEND(sp,3);
    PUSHs(sv_2mortal(newSVnv((float)x)));
    PUSHs(sv_2mortal(newSVnv((float)y)));
    PUSHs(sv_2mortal(newSVpv(&ch,1)));

void
iiscirc_c(x,y,r,colour)
   pdl	x
   pdl	y
   pdl	r
   pdl colour
   CODE:
     int frame  = (int)SvIV( perl_get_sv("iisframe", FALSE) );
     int n = x.nvals;
     int i, j=0, k=0;
     float *xx,*yy,*rr;
     long  *cc;

     if (x.ndims!=1 || y.ndims !=1 || r.ndims !=1 || colour.ndims!=1) 
         croak("Arguments must be 1D");
     if (y.nvals !=n || (r.nvals!=n && r.nvals!=1) || (colour.nvals!=n && colour.nvals!=1))
         croak("Dimensions must match");

     PDL->converttype( &x, PDL_F, TMP); xx = (float*) x.data;
     PDL->converttype( &y, PDL_F, TMP); yy = (float*) y.data;
     PDL->converttype( &r, PDL_F, TMP); rr = (float*) r.data;
     PDL->converttype( &colour, PDL_L, TMP); cc = (long*) colour.data;

     iis_open(SvPV(perl_get_sv("fifi",FALSE),na),SvPV(perl_get_sv("fifo",FALSE),na),
        (int)SvIV( perl_get_sv("fbconfig", FALSE) ),
        (int)SvIV( perl_get_sv("fb_x", FALSE) ), 
        (int)SvIV( perl_get_sv("fb_y", FALSE) ) );
     for (i=0; i<n; i++) {
         iis_drawcirc( xx[i], yy[i], rr[j], (int)cc[k], frame);
         if (r.nvals!=1) 
            j++;
         if (colour.nvals!=1) 
            k++;
     }
    iis_close();


###########################################################################

BOOT:

   /* Get pointer to structure of core shared C routines */

   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
   if (CoreSV==NULL)
      croak("This module requires use of PDL::Core first");

   PDL = (Core*) (void*) SvIV( CoreSV );  /* Core* value */

