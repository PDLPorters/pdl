#include "mconf.h"
double quiet_nan(void)
{
#ifdef NaN
  double a;
  return NaN(a);
#else
  double a=0;
  return 0./a; /* Expect bad value error */
#endif
}
