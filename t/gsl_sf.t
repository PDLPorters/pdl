

# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL::LiteF;
use Test;

BEGIN {
        eval " use PDL::GSLSF::BESSEL; ";
  unless ($@) {
    plan tests => 1;
  } else {
    plan tests => 1;
    print "ok 1 # Skipped: PDL::GSLSF not installed\n";
    exit;
  }
}


$arg = 5.0;
$expected = -0.17759677131433830434739701;

($y,$err) = gsl_sf_bessel_Jn $arg, 0;

print "got $y +- $err\n";

ok abs($y-$expected) < 1e-6;
