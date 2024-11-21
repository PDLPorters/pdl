#include "EXTERN.h"
#include "perl.h"
#include "pdlcore.h"
#include <gsl/gsl_math.h>

#define PDL PDL_GSL_INTEG
extern Core *PDL;

#define max_nested_integrals  20
 
static SV* ext_funname[max_nested_integrals];
static int current_fun = -1;

void set_funname(SV *fn) {
  current_fun++;
  if (current_fun > (max_nested_integrals)) barf("Too many nested integrals, sorry!\n");
  ext_funname[current_fun] = fn;
}
void dec_func() {
  current_fun--;
}

void my_handler (const char * reason,
                 const char * file,
                 int line,
                 int gsl_errno){
  printf("Warning: %s at line %d of GSL file %s\n",reason,line,file);
}

double FUNC(double x,void * p){

  SV* funname;

  int count;

  I32 ax ; 

  double res;
  double* resp;

  dSP;

  resp = &res;


  ENTER;
  SAVETMPS;

  /* get function name on the perl side */
  funname = ext_funname[current_fun];

  PUSHMARK(SP);

  XPUSHs(sv_2mortal(newSVnv(x)));

  PUTBACK;

  count=call_sv(funname,G_SCALAR);

  SPAGAIN; 
  SP -= count ;
  ax = (SP - PL_stack_base) + 1 ;

  if (count!=1)
    croak("error calling perl function\n");

  /* recover output value */
  /*res = POPn;*/
  *resp = SvNV(ST(0));

  PUTBACK;
  FREETMPS;
  LEAVE;
  
  return res;
}
