#include "EXTERN.h"
#include "perl.h"

void PDL_FORTRAN(slatecbarf)() {
   croak("slatec called halt");
}
