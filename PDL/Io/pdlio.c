
#include "pdl.h"      /* Data structure declarations */
#include "pdlcore.h"  /* Core declarations */
 

/*
   Map a scalar variable holding character binary data to a byte pdl -
   this allows the bswap routines to be used on an ordinary perl
   character variables.
*/

pdl pdl_frombuff( SV* sv ) {

    pdl ret;
    STRLEN len;
  
    ret.data     = (void*) SvPV(sv, len);
    ret.nvals    = len;
    ret.datatype = PDL_B;

    /* Not used in PDL::IO */

    ret.dims   = NULL;  
    ret.ndims  = 1;
    ret.sv     = NULL;

    return ret;
}

