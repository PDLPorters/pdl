# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL::LiteF;
use PDL::GSL::MROOT;
use Test::More;

my $init = pdl (-10.00, -5.0);
my $epsabs = 1e-7;

$res = gslmroot_fsolver($init, \&rosenbrock,{Method => 0, EpsAbs => $epsabs});

my @res = list ($res);

ok(abs($res[0]- 1) < 1e-6 );
ok(abs($res[1]- 1) < 1e-6 );

done_testing;

sub rosenbrock{
  my ($x) = @_;
  my $a1 = 1;
  my $b1 = 10;
  my $y = zeroes($x);
  my $tmp; # work around perl -d "feature"
  ($tmp = $y->slice(0)) .=  $a1 * (1 - $x->slice(0));
  ($tmp = $y->slice(1)) .=  $b1 * ($x->slice(1) - $x->slice(0)**2);
  return $y;
}
