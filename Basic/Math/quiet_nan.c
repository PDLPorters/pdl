#include "mconf.h"
double quiet_nan(void)
{
#ifdef NaN
  double a;
  return NaN(a);
#else
  return 0./0.; /* Expect bad value error */
#endif
}
