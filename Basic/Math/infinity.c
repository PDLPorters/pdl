#include "mconf.h"
double infinity(void)
{
#ifdef DBL_INFINITY
  return DBL_INFINITY;
#else
  return 1./0.; /* Expect divide by zero error */
#endif
}
