
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */

#define TMP  0        /* Flags */
#define PERM 1

#define BIGGESTOF(a,b) ( a.nvals>b.nvals ? a.nvals : b.nvals )

typedef int Logical;

/*************** Function prototypes *********************/


/* pdlcore.c */

int     pdl_howbig (int datatype);           /* Size of data type (bytes) */
pdl     SvPDLV ( SV* sv );                   /* Map SV* to pdl struct */
SV*     pdl_copy( pdl a, char* option );     /* call copy method */
int*    pdl_packdims ( SV* sv, int *ndims ); /* Pack dims[] into SV aref */
void    pdl_unpackdims ( SV* sv, int *dims,  /* Unpack */
                         int ndims );        
void*   pdl_malloc ( int nbytes );           /* malloc memory - auto free()*/


/* pdlbasicops.c */

void pdl_biop ( char* op, void* c, void* a, void* b, int n1, int n2, 
           int datatype);
void pdl_bifunc ( char* func, void* c, void* a, void* b, int n1, int n2, 
             int datatype);
void pdl_ufunc ( char* func, void* x, int n, int datatype );


/* pdlconv.c */

void   pdl_swap(pdl* a, pdl* b);              /* Swap two pdls */
void   pdl_converttype( pdl* a, int targtype, /* Change type of a pdl */
                        Logical changePerl ); 
void   pdl_coercetypes( pdl *a, pdl *b, Logical changePerl ); /* Two types to same */
void   pdl_grow  ( pdl* a, int newsize);      /* Change pdl 'Data' size */
void   pdl_retype( pdl* a, int newtype);      /* Change pdl 'Datatype' value */
void** pdl_twod( pdl x );                       /* Return 2D pointer to data array */

/* pdlsections.c */

int  pdl_get_offset(int* pos, int* dims, int ndims);      /* Offset of pixel x,y,z... */
int  pdl_validate_section( int* sec, int* dims,           /* Check section */
                           int ndims );
void pdl_row_plusplus ( int* pos, int* dims,              /* Move down one row */
                        int ndims ); 
void pdl_subsection( char *y, char*x, int datatype,      /* Take subsection */
                 int* sec, int* dims, int* ndims);
void pdl_insertin( char*y, int* ydims, int nydims,        /* Insert pdl in pdl */
                   char*x, int* xdims, int nxdims, 
                   int datatype, int* pos);
double pdl_at( void* x, int datatype, int* pos, int* dims, /* Value at x,y,z,... */
             int ndims);
void  pdl_set( void* x, int datatype, int* pos, int* dims, /* Set value at x,y,z... */
                int ndims, double value);
void pdl_axisvals( pdl a, int axis );               /* Fill with axis values */

/* pdlstats.c */

double pdl_min(void*x, int n, int datatype);
double pdl_max(void*x, int n, int datatype);
double pdl_sum(void*x, int n, int datatype);

/* pdlmoremaths.c */

void pdl_convolve (pdl c, pdl a, pdl b); /* Real space convolution */
void pdl_hist (pdl c, pdl a, double min, double step) ; /* Histogram of data */
void pdl_matrixmult( pdl *c, pdl a, pdl b);  /* Matrix multiplication */

/* Structure to hold pointers core PDL routines so as to be used by many modules */

struct Core { 
    pdl    (*SvPDLV)      ( SV*  );
    SV*    (*copy)        ( pdl, char* ); 
    void   (*converttype) ( pdl*, int, Logical ); 
    void** (*twod)        ( pdl ); 
    void*  (*malloc)      ( int );
    int    (*howbig)      ( int );
};

typedef struct Core Core;

