#ifdef DEBUGMAIN
void prtc(int n, double p[], double q[]);
void prtz(int n,double zr[], double zi[]);
#endif
char *cpoly(double opr[], double opi[], int degree,
	   double zeror[], double zeroi[]);
complex double polyev(int nn, complex double sc, complex double pc[],
	   complex double qc[]);

#if !defined(FALSE)
#define FALSE (0)
#endif
#if !defined(TRUE)
#define TRUE (1)
#endif

