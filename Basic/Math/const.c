/* Some constant values --  */

#include "mconf.h"
/* Many of these values should ideally come from <float.h> or
   <values.h>, which should be included in mconf.h if required */

#ifdef DBL_EPSILON
double MACHEP =  DBL_EPSILON;
#else
double MACHEP =  1.11022302462515654042E-16;   /* 2**-53 */
#endif

#if defined DBL_MIN
double UFLOWTHRESH = DBL_MIN;
#elif defined MINDOUBLE
double UFLOWTHRESH = MINDOUBLE;
#else
double UFLOWTHRESH =  2.22507385850720138309E-308; /* 2**-1022 */
#endif

#ifdef DBL_MAX_10_EXP
double MAXLOG = DBL_MAX_10_EXP;
#else
double MAXLOG =  7.08396418532264106224E2;     /* log 2**1022 */
#endif

#ifdef DBL_MIN_10_EXP
double MINLOG = DBL_MIN_10_EXP;
#else
double MINLOG = -7.08396418532264106224E2;     /* log 2**-1022 */
#endif

#if defined MAXDOUBLE
double MAXNUM =  MAXDOUBLE;
#elif defined DBL_MAX
double MAXNUM =  DBL_MAX;
#else
double MAXNUM =  1.79769313486231570815E308;    /* 2**1024*(1-MACHEP) */
#endif

#ifdef M_PI
#ifndef PI
double PI     =  M_PI;
#endif
double PIO2   =  M_PI/2;
double PIO4   =  M_PI/4;
double THPIO4 =  0.75*M_PI;
double TWOOPI =  2/M_PI;
#else
#ifndef PI
double PI     =  3.14159265358979323846;       /* pi */
#endif
double PIO2   =  1.57079632679489661923;       /* pi/2 */
double PIO4   =  7.85398163397448309616E-1;    /* pi/4 */
double THPIO4 =  2.35619449019234492885;       /* 3*pi/4 */
double TWOOPI =  6.36619772367581343075535E-1; /* 2/pi */
#endif

#ifdef M_SQRT2
double SQRT2  =  M_SQRT2;       /* sqrt(2) */
double SQRTH  =  M_SQRT2/2;    /* sqrt(2)/2 */
#else
double SQRT2  =  1.41421356237309504880;       /* sqrt(2) */
double SQRTH  =  7.07106781186547524401E-1;    /* sqrt(2)/2 */
#endif

double SQ2OPI =  7.9788456080286535587989E-1;  /* sqrt( 2/pi ) */

#ifdef M_LN2
double LOGE2  =  M_LN2;
double LOGSQ2 =  M_LN2/2;    /* log(2)/2 */
double LOG2E  =  1/M_LN2;     /* 1/log(2) */
#else
double LOGE2  =  6.93147180559945309417E-1;    /* log(2) */
double LOGSQ2 =  3.46573590279972654709E-1;    /* log(2)/2 */
double LOG2E  =  1.4426950408889634073599;     /* 1/log(2) */
#endif



