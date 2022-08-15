/*
 * resample.h - based on code from version 3.6-0 of the Eclipse library (ESO)
 *   by Nicolas Devillard
 *
 * see http://www.eso.org/eclipse for further details
 */

#ifndef _PDL_RESAMPLE_H_
#define _PDL_RESAMPLE_H_

#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef NULL
#define NULL (0L)
#endif

/* Number of tabulations in kernel  */
#define TABSPERPIX      (1000)
#define KERNEL_WIDTH    (2.0)
#define KERNEL_SAMPLES  (1+(int)(TABSPERPIX * KERNEL_WIDTH))

#define TANH_STEEPNESS	(5.0)

#ifndef PI_NUMB
#define PI_NUMB     (3.1415926535897932384626433832795)
#endif

#ifndef M_PI
#define M_PI PI_NUMB
#endif

/* declare functions */

long double
poly2d_compute( int ncoeff, long double *c, long double u, long double *vpow );

long double   *
generate_interpolation_kernel(char * kernel_type);

void
kernel_free(void *p);

#endif
