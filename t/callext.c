
/*

  Example C routine for how to use callext() in PDL
  - return log x to base y vector.

  E.g. on Solaris this would be compiled:

  cc -o callext.so -G -Kpic callext.c

  to generate dynamically loadable code. For other systems
  see the man pages on your C compiler or the Perl config
  information.

*/

#include <stdio.h>
#include <math.h>

/* For WindowsNT you have to make sure that the relevant routine
   is exported by the dll otherwise it will not be loadable even if it
   can be linked. In this example we use the __declspec declaration.
   The same effect can be achieved with .def files although currently
   CallExt.pm does not know to use them */
#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#include "pdlsimple.h"

/* This is the action routine */

void loglog_doit( float *x, float *y, int nvals) {

   int i;

   for (i=0; i<nvals; i++)
      x[i] = log((float)x[i])/log((float)y[i]);
}

/*
   This is the hook routine - nargs is the number of
   arguments and *args is an array of pdl structures
*/

DLLEXPORT int loglog_ext(int nargs, pdlsimple **args) {

   pdlsimple* x;
   pdlsimple* y;

   /* Check args */

   printf("\nExecuting the C external routine\n\n");

   if (nargs != 2) {
      fprintf(stderr, "Error in number of arguments\n");
      return (0); /* Failure */
   }

   x = args[0]; y = args[1];

   if (x->datatype != PDL_F || y->datatype != PDL_F) {
      fprintf(stderr, "Error in data type of arguments %d %d\n",
              x->datatype, y->datatype);
      return (0); /* Failure */
   }

   if (x->nvals != y->nvals) {
      fprintf(stderr, "Number of data values unequal in arguments\n");
      return(0); /* Failure */
   }

   /* Now do the buisness! */

   loglog_doit( (float*) x->data, (float*) y->data, x->nvals);

   return(1);  /* Success! */
}

