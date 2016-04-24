

# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked.
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL::LiteF;
use Test::More;
use strict;

BEGIN
{
   use PDL::Config;
   if ( $PDL::Config{WITH_GSL} ) {
       my $v = `gsl-config --version`;
       eval "
	  use PDL::GSLSF::AIRY;
	  use PDL::GSLSF::BESSEL;
	  use PDL::GSLSF::CLAUSEN;
	  use PDL::GSLSF::COULOMB;
	  use PDL::GSLSF::COUPLING;
	  use PDL::GSLSF::DAWSON;
	  use PDL::GSLSF::DEBYE;
	  use PDL::GSLSF::DILOG;
	  use PDL::GSLSF::ELEMENTARY;
	  use PDL::GSLSF::ELLINT;
	  use PDL::GSLSF::ELLJAC;
	  use PDL::GSLSF::ERF;
	  use PDL::GSLSF::EXP;
	  use PDL::GSLSF::EXPINT;
	  use PDL::GSLSF::FERMI_DIRAC;
	  use PDL::GSLSF::GAMMA;
	  use PDL::GSLSF::GEGENBAUER;
	  use PDL::GSLSF::HYPERG;
	  use PDL::GSLSF::LAGUERRE;
	  use PDL::GSLSF::LEGENDRE;
	  use PDL::GSLSF::LOG;
	  use PDL::GSLSF::POLY;
	  use PDL::GSLSF::POW_INT;
	  use PDL::GSLSF::PSI;
	  use PDL::GSLSF::SYNCHROTRON;
	  use PDL::GSLSF::TRANSPORT;
	  use PDL::GSLSF::TRIG;
	  use PDL::GSLSF::ZETA;
      ";
      unless ($@) {
	  plan tests => 3 if $v>=2.0;
	  plan tests => 1 if $v<2.0;
      } else {
	  warn "Warning: $@\n\n";
         plan skip_all => "PDL::GSLSF modules not installed.";
      }
   } else {
      plan skip_all => "PDL::GSLSF modules not compiled.";
   }
}

my $version = `gsl-config --version`;
my $arg = 5.0;
my $expected = -0.17759677131433830434739701;

my ($y,$err) = gsl_sf_bessel_Jn($arg, 0);

#print "got $y +- $err\n";

ok(abs($y-$expected) < 1e-6,"GSL SF Bessel function");

if ($version >= 2.0){
    my $Ylm = gsl_sf_legendre_array(xvals(21)/10-1,'Y',4,-1);
    ok($Ylm->slice("(0)")->uniq->nelem == 1, "Legendre Y00 is constant");
    ok(approx($Ylm->slice("(0),(0)"),0.5/sqrt(3.141592654),1E-6), "Y00 value is corect");
}
