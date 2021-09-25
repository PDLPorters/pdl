#include "EXTERN.h"
#include "perl.h"
#include <gsl/gsl_math.h>

SV* ext_funname;

void set_funname(SV *fn) {
  ext_funname = fn;
}

double FUNC(double x,void * p){

  double res;
  int count;

  dSP;
  SV* funname = ext_funname; /* get function name on the perl side */

  ENTER;
  SAVETMPS;

  PUSHMARK(SP);

  XPUSHs(sv_2mortal(newSVnv(x)));

  PUTBACK;

  count=call_sv(funname,G_SCALAR);

  SPAGAIN; 

  if (count!=1)
    croak("error calling perl function\n");

  /* recover output value */
  res = POPn;

  PUTBACK;
  FREETMPS;
  LEAVE;
  
  return res;
}
