
/* 
   Define the pdl C data structure which maps onto the original PDL
   perl data structure. 

   Note: in above pdl.sv is defined as a void pointer to avoid
   having to include perl.h to get the SV* definition.         
*/

struct pdl {
   void*    sv;      /* Pointer back to original sv */
   int    datatype;  /* Ignored for now, assumed double - complex later */
   void  *data;      /* Generic pointer to the data block (usually in the sv) */
   int    nvals;     /* Number of data values */
   int   *dims;      /* Array of data dimensions */
   int    ndims;     /* Number of data dimensions */
};


/* Data types/sizes [must be in order of complexity] */

enum pdl_datatypes { PDL_B, PDL_S, PDL_US, PDL_L, PDL_F, PDL_D };

typedef struct pdl pdl;

