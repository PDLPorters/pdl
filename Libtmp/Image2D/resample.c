/*
 * resample.c
 * - a hacked version of 
 *      ipow.c, poly2d.c, and resampling.c 
 *   from version 3.6-0 of the Eclipse library (ESO)
 *   by Nicolas Devillard
 *
 * see http://www.eso.org/eclipse for further details
 */

#include "resample.h"

/*-------------------------------------------------------------------------*/
/**
  @name		ipow
  @memo		Same as pow(x,y) but for integer values of y only (faster).
  @param	x	A double number.
  @param	p	An integer power.
  @return	x to the power p.
  @doc

  This is much faster than the math function due to the integer. Some
  compilers make this optimization already, some do not.

  p can be positive, negative or null.
 */
/*--------------------------------------------------------------------------*/

double ipow(double x, int p)
{
	double r, recip ;

	/* Get rid of trivial cases */
	switch (p) {
		case 0:
		return 1.00 ;

		case 1:
		return x ;

		case 2:
		return x*x ;

		case 3:
		return x*x*x ;

		case -1:
		return 1.00 / x ;

		case -2:
		return (1.00 / x) * (1.00 / x) ;
	}
	if (p>0) {
		r = x ;
		while (--p) r *= x ;
	} else {
		r = recip = 1.00 / x ;
		while (++p) r *= recip ;
	}
	return r;
}


/*
 * compute the value of a 2D polynomial at a point
 * - it assumes that ncoeff is a small number, so there's
 *   little point in pre-calculating ipow(u,i)
 */

double
poly2d_compute( int ncoeff, double *c, double u, double *vpow ) {
  double out;
  int    i, j, k;

  out = 0.00;
  k = 0;
  for( j = 0; j < ncoeff; j++ ) {
    for( i = 0; i < ncoeff; i++ ) {
      out += c[k] * ipow( u, i ) * vpow[j];
      k++;
    }
  }
  return out;
}

/*-------------------------------------------------------------------------*/
/**
  @name		sinc
  @memo		Cardinal sine.
  @param	x	double value.
  @return	1 double.
  @doc

  Compute the value of the function sinc(x)=sin(pi*x)/(pi*x) at the
  requested x.
 */
/*--------------------------------------------------------------------------*/

double
sinc(double x)
{
    if (fabs(x)<1e-4)
        return (double)1.00 ;
    else
        return ((sin(x * (double)PI_NUMB)) / (x * (double)PI_NUMB)) ;
} /* sinc() */

/**
  @name		reverse_tanh_kernel
  @memo		Bring a hyperbolic tangent kernel from Fourier to normal space.
  @param	data	Kernel samples in Fourier space.
  @param	nn		Number of samples in the input kernel.
  @return	void
  @doc

  Bring back a hyperbolic tangent kernel from Fourier to normal space. Do
  not try to understand the implementation and DO NOT MODIFY THIS FUNCTION.
 */
/*--------------------------------------------------------------------------*/

#define KERNEL_SW(a,b) tempr=(a);(a)=(b);(b)=tempr
static void reverse_tanh_kernel(double * data, int nn)
{
    unsigned long   n,
					mmax,
					m,
					i, j,
					istep ;
    double  wtemp,
            wr,
            wpr,
            wpi,
            wi,
            theta;
    double  tempr,
            tempi;

    n = (unsigned long)nn << 1;
    j = 1;
    for (i=1 ; i<n ; i+=2) {
        if (j > i) {
            KERNEL_SW(data[j-1],data[i-1]);
            KERNEL_SW(data[j],data[i]);
        }
        m = n >> 1;
        while (m>=2 && j>m) {
            j -= m;
            m >>= 1;
        }
        j += m;
    }
    mmax = 2;
    while (n > mmax) {
        istep = mmax << 1;
        theta = 2 * M_PI / mmax;
        wtemp = sin(0.5 * theta);
        wpr = -2.0 * wtemp * wtemp;
        wpi = sin(theta);
        wr  = 1.0;
        wi  = 0.0;
        for (m=1 ; m<mmax ; m+=2) {
            for (i=m ; i<=n ; i+=istep) {
                j = i + mmax;
                tempr = wr * data[j-1] - wi * data[j];
                tempi = wr * data[j]   + wi * data[j-1];
                data[j-1] = data[i-1] - tempr;
                data[j]   = data[i]   - tempi;
                data[i-1] += tempr;
                data[i]   += tempi;
            }
            wr = (wtemp = wr) * wpr - wi * wpi + wr;
            wi = wi * wpr + wtemp * wpi + wi;
        }
        mmax = istep;
    }
} /* reverse_tanh_kernel() */
#undef KERNEL_SW

/*-------------------------------------------------------------------------*/
/**
  @name		generate_tanh_kernel
  @memo		Generate a hyperbolic tangent kernel.
  @param	steep	Steepness of the hyperbolic tangent parts.
  @return	1 pointer to a newly allocated array of doubles.
  @doc

  The following function builds up a good approximation of a box filter. It
  is built from a product of hyperbolic tangents. It has the following
  properties:

  \begin{itemize}
  \item It converges very quickly towards +/- 1.
  \item The converging transition is very sharp.
  \item It is infinitely differentiable everywhere (i.e. smooth).
  \item The transition sharpness is scalable.
  \end{itemize}

  The returned array must be deallocated using free().
 */
/*--------------------------------------------------------------------------*/

#define hk_gen(x,s) (((tanh(s*(x+0.5))+1)/2)*((tanh(s*(-x+0.5))+1)/2))

double * generate_tanh_kernel(double steep)
{
    double  *   kernel ;
    double  *   x ;
    double      width ;
    double      inv_np ;
    double      ind ;
    int         i ;
    int         np ;
    int         samples ;

    width   = (double)TABSPERPIX / 2.0 ; 
    samples = KERNEL_SAMPLES ;
    np      = 32768 ; /* Hardcoded: should never be changed */
    inv_np  = 1.00 / (double)np ;

    /*
     * Generate the kernel expression in Fourier space
     * with a correct frequency ordering to allow standard FT
     */
    x = (double *) malloc((2*np+1)*sizeof(double)) ;
    for (i=0 ; i<np/2 ; i++) {
        ind      = (double)i * 2.0 * width * inv_np ;
        x[2*i]   = hk_gen(ind, steep) ;
        x[2*i+1] = 0.00 ;
    }
    for (i=np/2 ; i<np ; i++) {
        ind      = (double)(i-np) * 2.0 * width * inv_np ;
        x[2*i]   = hk_gen(ind, steep) ;
        x[2*i+1] = 0.00 ;
    }

    /* 
     * Reverse Fourier to come back to image space
     */
    reverse_tanh_kernel(x, np) ;

    /*
     * Allocate and fill in returned array
     */
    kernel = (double *) malloc(samples * sizeof(double)) ;
    for (i=0 ; i<samples ; i++) {
        kernel[i] = 2.0 * width * x[2*i] * inv_np ;
    }
    free(x) ;
    return kernel ;

} /* generate_tanh_kernel() */


/*-------------------------------------------------------------------------*/
/**
  @name		generate_interpolation_kernel
  @memo		Generate an interpolation kernel to use in this module.
  @param	kernel_type		Type of interpolation kernel.
  @return	1 newly allocated array of doubles.
  @doc

  Provide the name of the kernel you want to generate. Supported kernel
  types are:

  \begin{tabular}{ll}
  NULL			&	default kernel, currently "tanh" \\
  "default"		&	default kernel, currently "tanh" \\
  "tanh"		&	Hyperbolic tangent \\
  "sinc2"		&	Square sinc \\
  "lanczos"		&	Lanczos2 kernel \\
  "hamming"		&	Hamming kernel \\
  "hann"		&	Hann kernel
  \end{tabular}

  The returned array of doubles is ready of use in the various re-sampling
  functions in this module. It must be deallocated using free().
 */
/*--------------------------------------------------------------------------*/

double   *
generate_interpolation_kernel(char * kernel_type)
{
    double  *	tab ;
    int     	i ;
    double  	x ;
	double		alpha ;
	double		inv_norm ;
    int     	samples = KERNEL_SAMPLES ;

	if (kernel_type==NULL) {
		tab = generate_interpolation_kernel("tanh") ;
	} else if (!strcmp(kernel_type, "default")) {
		tab = generate_interpolation_kernel("tanh") ;
	} else if (!strcmp(kernel_type, "sinc")) {
		tab = (double *) malloc(samples * sizeof(double)) ;
		tab[0] = 1.0 ;
		tab[samples-1] = 0.0 ;
		for (i=1 ; i<samples ; i++) {
			x = (double)KERNEL_WIDTH * (double)i/(double)(samples-1) ;
			tab[i] = sinc(x) ;
		}
	} else if (!strcmp(kernel_type, "sinc2")) {
		tab = (double *) malloc(samples * sizeof(double)) ;
		tab[0] = 1.0 ;
		tab[samples-1] = 0.0 ;
		for (i=1 ; i<samples ; i++) {
			x = 2.0 * (double)i/(double)(samples-1) ;
			tab[i] = sinc(x) ;
			tab[i] *= tab[i] ;
		}
	} else if (!strcmp(kernel_type, "lanczos")) {
		tab = (double *) malloc(samples * sizeof(double)) ;
		for (i=0 ; i<samples ; i++) {
			x = (double)KERNEL_WIDTH * (double)i/(double)(samples-1) ;
			if (fabs(x)<2) {
				tab[i] = sinc(x) * sinc(x/2) ;
			} else {
				tab[i] = 0.00 ;
			}
		}
	} else if (!strcmp(kernel_type, "hamming")) {
		tab = (double *) malloc(samples * sizeof(double)) ;
		alpha = 0.54 ;
		inv_norm  = 1.00 / (double)(samples - 1) ;
		for (i=0 ; i<samples ; i++) {
			x = (double)i ;
			if (i<(samples-1)/2) {
				tab[i] = alpha + (1-alpha) * cos(2.0*PI_NUMB*x*inv_norm) ;
			} else {
				tab[i] = 0.0 ;
			}
		}
	} else if (!strcmp(kernel_type, "hann")) {
		tab = (double *) malloc(samples * sizeof(double)) ;
		alpha = 0.50 ;
		inv_norm  = 1.00 / (double)(samples - 1) ;
		for (i=0 ; i<samples ; i++) {
			x = (double)i ;
			if (i<(samples-1)/2) {
				tab[i] = alpha + (1-alpha) * cos(2.0*PI_NUMB*x*inv_norm) ;
			} else {
				tab[i] = 0.0 ;
			}
		}
	} else if (!strcmp(kernel_type, "tanh")) {
		tab = generate_tanh_kernel(TANH_STEEPNESS) ;
	} else {
	  /**
	   ** trapped at perl level, so should never reach here
		e_error("unrecognized kernel type [%s]: aborting generation",
				kernel_type) ;
	  **/
		return NULL ;
	}

    return tab ;

} /* generate_interpolation_kernel() */

/***********
 *** END ***
 ***********/
