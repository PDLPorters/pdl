#include "mconf.h"
/* Patch NaN function where no system NaN is available */
double quiet_nan(NANARG_SIGNATURE)
{
#ifdef NaN
  double a;
  return NaN(a);
#else
  double a=0;
  return 0./a; /* Expect bad value error */
#endif
}
