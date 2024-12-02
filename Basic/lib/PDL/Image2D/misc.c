#include "pdl.h"

/* Add an equivalence to a list - used by pdl_ccNcompt */
void AddEquiv ( PDL_Long* equiv, PDL_Long i, PDL_Long j) {
   PDL_Long k, tmp;
   if (i==j)
      return;
    k = j;
    do {
      k = equiv[k];
    } while ( k != j && k != i );
    if ( k == j ) {
       tmp = equiv[i];
       equiv[i] = equiv[j];
       equiv[j] = tmp;
    }
}

#define MAXSEC 32
#define line(x1, x2, y) for (k=x1;k<=x2;k++) \
	{ /* printf("line from %d to %d\n",x1,x2); */ \
	image[k+wx*y] = col; }
#define PX(n) ps[2*n]
#define PY(n) ps[2*n+1]

void polyfill(PDL_Long *image, int wx, int wy, float *ps, int n,
	PDL_Long col, int *ierr)
{
   int ymin, ymax, xmin, xmax, fwrd = 1, i, j, k, nsect;
   int x[MAXSEC], temp, l;
   float s1, s2, t1, t2;

   ymin = PY(0); ymax = PY(0);
   xmin = PX(0); xmax = PX(0);
   *ierr = 0;

   for (i=1; i<n; i++) {
     ymin = PDLMIN(ymin, PY(i));
     ymax = PDLMAX(ymax, PY(i));
     xmin = PDLMIN(xmin, PX(i));
     xmax = PDLMAX(xmax, PX(i));
   }
   if (xmin < 0 || xmax >= wx || ymin < 0 || ymax >= wy) {
   	*ierr = 1; /* clipping */
	return;
   }
   s1 = PX(n-1);
   t1 = PY(n-1);
   for (l=ymin; l<= ymax; l++) {
	nsect = 0;
	fwrd = 1;
	for (i=0; i<n; i++) {
	  s2 = PX(i);
	  t2 = PY(i);
	  if ((t1 < l &&  l <= t2) || (t1 >= l && l > t2)) {
		if (nsect > MAXSEC) {
			*ierr = 2; /* too complex */
			return;
		}
		x[nsect] = (s1+(s2-s1)*((l-t1)/(t2-t1)));
	  	nsect += 1;
	  }
	  s1 = s2;
	  t1 = t2;
 	}
	/* sort the intersections */
	for (i=1; i<nsect; i++)
		for (j=0; j<i; j++)
			if (x[j] > x[i]) {
				temp = x[j];
				x[j] = x[i];
				x[i] = temp;
			}
	if (fwrd) {
		for (i=0; i<nsect-1; i += 2)
			line(x[i],x[i+1],l);
		fwrd = 0;
	} else {
		for (i=nsect-1; i>0; i -= 2)
			line(x[i-1],x[i],l);
		fwrd = 1;
	}
   }
}
