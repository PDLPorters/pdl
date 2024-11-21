# Test Script for the PDL interface to the GSL library
#  This tests only that the interface is working, i.e. that the
#   functions can be called. The actual return values are not
#   checked.
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::GSL::SF;

my ($y,$err) = gsl_sf_bessel_Jn(5.0, 0);
is_pdl $y, pdl(-0.17759677131433830434739701), "GSL SF Bessel function";

chomp(my $version = `gsl-config --version`);
if ((split /\./, $version)[0] >= 2) {
  my $Ylm = gsl_sf_legendre_array(xvals(21)/10-1,'Y',4,-1);
  ok($Ylm->slice("(0)")->uniq->nelem == 1, "Legendre Y00 is constant");
  is_pdl $Ylm->slice("(0),(0)"), pdl(0.5/sqrt(3.141592654)), "Y00 value is correct";
}

{
#  Check that the PDL error handler gets called instead of aborting
#  Failure is an abort. 
my @warning;
local $SIG{__WARN__} = sub { push @warning, @_ };
my $err_test = eval {gsl_sf_lngamma(pdl(0))};
isnt $@, '', "Got an error for invalid input";
ok !@warning, 'no warnings' or diag explain \@warning;
}

($y, my $e) = gsl_sf_airy_Ai(sequence(4));
is_pdl $y, pdl([0.35502805, 0.13529242, 0.03492413, 0.0065911394]);
is_pdl $e, pdl([8.3366727e-17, 5.6151774e-17, 3.9261626e-17, 1.0852712e-17]);

done_testing;
