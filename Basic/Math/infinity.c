#include "mconf.h"
double infinity(void)
{
#ifdef DBL_INFINITY
  return DBL_INFINITY;
#else
  double a=0;
  return 1./a; /* Expect divide by zero error */
#endif
}
