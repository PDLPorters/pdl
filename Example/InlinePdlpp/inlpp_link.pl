use blib; # when using inside the dist tree
use PDL;  # this must be called before (!) 'use Inline Pdlpp' calls

# for this example you need the numerical recipes library
# edit the INC and LIBS info below to point
# Inline towards the location of include and library files

use Inline Pdlpp => Config =>
  INC => "-I$ENV{HOME}/include",
  LIBS => "-L$ENV{HOME}/lib -lnr -lm",
  AUTO_INCLUDE => <<'EOINC',
#include <math.h>
#include "nr.h"    /* for poidev */
#include "nrutil.h"  /* for err_handler */

static void nr_barf(char *err_txt)
{
  fprintf(stderr,"Now calling croak...\n");
  croak("NR runtime error: %s",err_txt);
}
EOINC
  BOOT => 'set_nr_err_handler(nr_barf);'; # catch errors at the perl level

use Inline Pdlpp; # the actual code is in the __Pdlpp__ block below

$a = zeroes(10) + 30;;
print $a->poidev(-3),"\n";

__DATA__

__Pdlpp__

# poisson deviates
pp_def('poidev',
	Pars => 'xm(); [o] pd()',
	GenericTypes => [L,F,D],
	OtherPars => 'long idum',
	Code => '$pd() = poidev((float) $xm(), &$COMP(idum));',
);
