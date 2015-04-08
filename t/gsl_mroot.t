
# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;
use Test::More;

BEGIN
{
   use PDL::Config;
   if ( $PDL::Config{WITH_GSL} ) {
      eval " use PDL::GSL::MROOT; ";
      unless ($@) {
         ## plan tests => 2;
         plan skip_all => "PDL::GSL::MROOT doesn't work with PDL_Index, yet";
      } else {
         plan skip_all => "PDL::GSL::MROOT not installed";
      }
   } else {
      plan skip_all => "PDL::GSL::MROOT not compiled.";
   }
}

my $init = pdl (-10.00, -5.0);
my $epsabs = 1e-7;

$res = gslmroot_fsolver($init, \&rosenbrock,{Method => 0, EpsAbs => $epsabs});

my @res = list ($res);

ok(abs($res[0]- 1) < 1e-6 );
ok(abs($res[1]- 1) < 1e-6 );


sub rosenbrock{
  my ($x) = @_;
  my $a = 1;
  my $b = 10;
  my $y = zeroes($x);
  my $tmp; # work around perl -d "feature"
  ($tmp = $y->slice(0)) .=  $a * (1 - $x->slice(0));
  ($tmp = $y->slice(1)) .=  $b * ($x->slice(1) - $x->slice(0)**2);
  return $y;
}
