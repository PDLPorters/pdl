
/* 
   Define the pdl C data structure which maps onto the original PDL
   perl data structure. 

   Note: in above pdl.sv is defined as a void pointer to avoid
   having to include perl.h in C code which just needs the pdl
   data.       
*/

#define PDL_NDIMS 8  /* Number of dims[] to preallocate */

struct pdl {
   void*    sv;      /* Pointer back to original sv */
   int    datatype;  /* whether byte/int/float etc. */
   void  *data;      /* Generic pointer to the data block (usually in the sv) */
   int    nvals;     /* Number of data values */
   int   *dims;      /* Array of data dimensions */
   int    ndims;     /* Number of data dimensions */
   int 	 *incs;	     	/* Array of data increments */
   int   *threaddims;   /* Array of thread dimensions */
   int    nthreaddims;  /* Number of thread dimensions */
   int 	 *threadincs;	/* Array of thread increments */
   int	 offs;          /* Data offset */
   int   def_dims[PDL_NDIMS];   /* Preallocated space for efficiency */
   int   def_incs[PDL_NDIMS]; 
   int   def_threaddims[PDL_NDIMS]; 
   int   def_threadincs[PDL_NDIMS]; 
};

typedef struct pdl pdl;

/* Data types/sizes [must be in order of complexity] */

enum pdl_datatypes { PDL_B, PDL_S, PDL_US, PDL_L, PDL_F, PDL_D };

