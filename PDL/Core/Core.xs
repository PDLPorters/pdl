
/* 
   Core.xs   Under construction

*/

#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */

#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */

/* Return a integer or numeric scalar as approroate */

#define SET_RETVAL_NV x.datatype<PDL_F ? (RETVAL=newSViv( (IV)result )) : (RETVAL=newSVnv( result ))

static Core PDL; /* Struct holding pointers to shared C routines */

MODULE = PDL::Core     PACKAGE = PDL::Core


# C = A op B function

SV *
biop(a,b,reverse,op)
   pdl	a
   pdl	b
   Logical	reverse
   char *	op
   CODE:
    pdl c; 
    RETVAL = pdl_copy(a,""); /* Init value to return */
    c = SvPDLV(RETVAL);         /* Map */

    pdl_coercetypes(&a, &b, TMP);  /* Ensure data types equal */
    pdl_retype(&c, a.datatype); pdl_grow(&c, BIGGESTOF(a,b)); 

    if (reverse)
       pdl_swap(&a,&b);

    pdl_biop(op, c.data, a.data, b.data, a.nvals, b.nvals, a.datatype); 
    
    OUTPUT:
     RETVAL

# A = A op B function (i.e. in place so save on memory)

SV *
biop2(a,b,reverse,op)
   pdl	a
   pdl	b
   Logical	reverse
   char *	op
   CODE:
    pdl_converttype(&b, a.datatype, TMP);   /* Ensure data types equal */
    pdl_biop(op, a.data, a.data, b.data, a.nvals, b.nvals, a.datatype); 
    /* Note OUTPUT is automatically OK as return value is ST(0) (immortal) */
    
# Unary functions Y=F(X) (e.g. sqrt() log() etc.)

SV *
ufunc(x,func)
   pdl	x
   char *	func
   CODE:
     pdl y;
     RETVAL = pdl_copy(x,""); /* Init value to return */
     y = SvPDLV(RETVAL);      /* Map */

     pdl_ufunc( func, y.data, y.nvals, y.datatype );

    OUTPUT:
     RETVAL

# Binary functions C=F(A,B) (e.g. atan2 etc.)

SV *
bifunc(a,b,reverse,func)
   pdl	a
   pdl	b
   Logical	reverse
   char *	func
   CODE:
    pdl c;
    RETVAL = pdl_copy(a,""); /* Init value to return */
    c = SvPDLV(RETVAL);               /* Map */

    pdl_coercetypes(&a, &b, TMP);  /* Ensure data types equal */
    pdl_retype(&c, a.datatype); pdl_grow(&c, BIGGESTOF(a,b));

    if (reverse)
       pdl_swap(&a,&b);

    pdl_bifunc(func, c.data, a.data, b.data, a.nvals, b.nvals, a.datatype); 

    OUTPUT:
     RETVAL

# Convert PDL to new datatype (called by float(), int() etc.)

SV *
convert(a,datatype)
   pdl	a
   int	datatype
   CODE:
    pdl b;
    RETVAL = pdl_copy(a,""); /* Init value to return */
    b = SvPDLV(RETVAL);      /* Map */

    pdl_converttype( &b, datatype, PERM );

    OUTPUT:
     RETVAL

# Call my howbig function

int
howbig(datatype)
   int	datatype
   CODE:
     RETVAL = pdl_howbig(datatype);
   OUTPUT:
     RETVAL

SV *
min(x)
   pdl	x
   CODE:
     double result;
     result = pdl_min( x.data, x.nvals, x.datatype );
     SET_RETVAL_NV ;    
   OUTPUT:
     RETVAL

SV *
max(x)
   pdl	x
   CODE:
     double result;
     result = pdl_max( x.data, x.nvals, x.datatype );
     SET_RETVAL_NV ;    
   OUTPUT:
     RETVAL

SV *
sum(x)
   pdl	x
   CODE:
     double result;
     result = pdl_sum( x.data, x.nvals, x.datatype );
     SET_RETVAL_NV ;    
   OUTPUT:
     RETVAL

# Subsection functipn

SV *
sec_c(x,section)
   pdl	x
   int *	section = NO_INIT 
   CODE:
     pdl y;
     int nsecs;
     int size;

     RETVAL = pdl_copy(x,"NoData"); /* Init value to return */ 
     y = SvPDLV(RETVAL);      /* Map */

     section = pdl_packdims( ST(1), &nsecs);
     if (section == NULL || nsecs != 2*(x.ndims))
        croak("Invalid subsection specified");

     size = pdl_validate_section( section, x.dims, x.ndims );
     pdl_grow ( &y, size );   /* To new size */


     /* Note - cast to char ptr to make byte ptr */

     pdl_subsection( (char*) y.data, (char*) x.data, 
                        x.datatype, section, x.dims,  &(x.ndims) );
 
     pdl_unpackdims( RETVAL, x.dims, x.ndims ); /* Update Dims */

    OUTPUT:
     RETVAL


# Insert x in y function - note y is modified in place which
# will be useful when we get tie overloading working for lvalues

void
insertin_c(y,x,postion)
   pdl	y
   pdl	x
   int *	pos = NO_INIT 
   CODE:
     int npos;

     if (y.ndims < x.ndims)
        croak("Cannot insert higher into lower dimension");    

     pos = pdl_packdims( ST(2), &npos);
     if (pos == NULL || npos != y.ndims) 
        croak("Invalid insertion position specified");

     pdl_converttype(&x, y.datatype, TMP); /* Ensure same type */

     /* Note - cast to char ptr to make byte ptr */

     pdl_insertin( (char*) y.data, y.dims, y.ndims, 
                   (char*) x.data, x.dims, x.ndims,  
                   x.datatype, pos );
 

SV *
at_c(x,position)
   pdl	x
   int *	pos = NO_INIT
   CODE:
    int npos;
    double result;

    pos = pdl_packdims( ST(1), &npos);
    if (pos == NULL || npos != x.ndims) 
       croak("Invalid position");

    result = pdl_at( x.data, x.datatype, pos, x.dims, x.ndims);

    SET_RETVAL_NV ;    

    OUTPUT:
     RETVAL

SV *
set_c(x,position,value)
   pdl	x
   int *	pos = NO_INIT
   double	value
   CODE:
    int npos;
    double result;

    pos = pdl_packdims( ST(1), &npos);
    if (pos == NULL || npos != x.ndims) 
       croak("Invalid position");

    pdl_set( x.data, x.datatype, pos, x.dims, x.ndims, value);

# Fill array with the corresponding coordinate (1=X, 2=Y, etc..)

SV *
axisvals(x,axis)
   pdl	x
   int	axis
   CODE:
     pdl y;
     RETVAL = pdl_copy(x,""); /* Init value to return */ 
     y = SvPDLV(RETVAL);      /* Map */
     if (axis>=y.ndims) 
        croak("Data has not enough dimensions for axis=%d",axis);
     pdl_axisvals(y, axis);
    OUTPUT:
     RETVAL

# Convolve array a with b
SV *
convolve(a,b)
   pdl	a
   pdl	b
   CODE:
     pdl c;
     RETVAL = pdl_copy(a,""); /* Init value to return */ 
     c = SvPDLV(RETVAL);      /* Map */

     pdl_coercetypes(&a, &b, TMP);  /* Ensure data types equal */
     pdl_retype(&c, a.datatype); pdl_grow(&c, a.nvals); 

     pdl_convolve( c, a, b );
     OUTPUT:
      RETVAL
    
# Return histogram of a
SV *
hist_c(a,min,max,step)
   pdl	a
   double	min
   double	max
   double	step
   CODE:
     int nbins;
     pdl c;
     RETVAL = pdl_copy(a,"NoData"); /* Init value to return */ 
     c = SvPDLV(RETVAL);      /* Map */

     nbins = (max-min)/step;
     if (nbins<=0)
        croak("Error max<=min");
     pdl_grow(&c, nbins);               /* New size */ 
     pdl_unpackdims( (SV*) c.sv, &nbins, 1 ); /* Change dimensions */

     pdl_hist( c, a, min, step );
     OUTPUT:
      RETVAL

# Matrix multiplication

SV *
matrix_mult(a,b,reverse)
   pdl	a
   pdl	b
   Logical	reverse
   CODE:
     pdl c;
     RETVAL = pdl_copy(a,"NoData"); /* Init value to return */ 
     c = SvPDLV(RETVAL);            /* Map */

     pdl_coercetypes(&a, &b, TMP);  /* Ensure data types equal */
     pdl_retype(&c, a.datatype);
     if (reverse)
        pdl_swap(&a,&b);

     pdl_matrixmult(&c,a,b);  /* This fills in the dims of c */

     pdl_unpackdims( (SV*) c.sv, c.dims, c.ndims ); /* Change SV* dims */
     OUTPUT:
      RETVAL

SV * 
transpose(x,...)
   pdl	x
   CODE:
     pdl y;
     RETVAL = pdl_copy(x,""); /* Init value to return */
     y = SvPDLV(RETVAL);      /* Map */
     pdl_transpose(&y, x);
     pdl_unpackdims( (SV*) y.sv, y.dims, y.ndims ); /* Change SV* dims */
     OUTPUT:
      RETVAL
    

# Call an external C routine loaded dynamically - pass PDL args list

void
callext_c(...)
     PPCODE:
        int (*symref)(int nargs, pdl *x);
        int nargs = items-1;
        pdl *x;
        int i;

        symref = (int(*)(int, pdl*)) SvIV(ST(0));

        x = pdl_malloc( nargs * sizeof(pdl) );
        for(i=0; i<nargs; i++) 
           x[i] = SvPDLV(ST(i+1));

        i = (*symref)(nargs, x);
        if (i==0)
           croak("Error calling external routine");

BOOT:

   /* Initialise structure of pointers to core C routines */

   PDL.SvPDLV      = SvPDLV;
   PDL.copy        = pdl_copy;
   PDL.converttype = pdl_converttype;
   PDL.twod        = pdl_twod;
   PDL.malloc      = pdl_malloc;
   PDL.howbig      = pdl_howbig;

   /* 
      "Publish" pointer to this structure in perl variable for use
       by other modules
   */

   sv_setiv(perl_get_sv("PDL::SHARE",TRUE), (IV) (void*) &PDL);

# version of eval() which propogates errors encountered in
# any internal eval(). Must be passed a code reference - could
# be use perl_eval_sv() but that is still buggy. This subroutine is 
# primarily for the perlDL shell to use.
#
# Thanks to Sarathy (gsar@engin.umich.edu) for suggesting this, though
# it needs to be wrapped up in the stack stuff to avoid certain SEGVs!

void
myeval(code)
  SV *	code;
  PROTOTYPE: $
  CODE:
   PUSHMARK(sp) ;
   perl_call_sv(code, G_EVAL|G_KEEPERR|GIMME);
