
# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;
use PDL::Config;
use Test::More;
	
BEGIN{
  eval " use PDL::GSL::INTERP; ";
  if ($@) {
    plan skip_all => "PDL::GSL::INTERP not installed";
  } else {
    plan tests => 11;
  }
}

my $x = sequence(10);
my $y = exp($x);
my $spl = PDL::GSL::INTERP->init('cspline',$x,$y);

ok(defined $spl);

my $spl2 = PDL::GSL::INTERP->init('cspline',$x,$y,{Sort => 0});

ok(defined $spl2);

ok(abs($spl->eval(5.5)-237.641810667697) < 1e-6 );
ok(abs($spl->deriv(5.5)-237.669424604497) < 1e-6 );
ok(abs($spl->deriv2(5.5)-306.23332503967) < 1e-6 );
ok(abs($spl->integ(3.2,8.5)-4925.23555581654) < 1e-6 );   
ok(abs($spl->eval(5.5,{Extrapolate => 1})-237.641810667697) < 1e-6 );
ok(abs($spl->deriv(5.5,{Extrapolate => 1})-237.669424604497) < 1e-6 );
ok(abs($spl->deriv2(5.5,{Extrapolate => 1})-306.23332503967) < 1e-6 );
ok(abs($spl->integ(3.2,8.5,{Extrapolate => 1})-4925.23555581654) < 1e-6 );   

# Bad value test added 5/31/2005 D. Hunt

SKIP: {
    skip "Test not valid without bad value support", 1 unless $PDL::Config{WITH_BADVAL};
    ok ($spl->eval(pdl(0)->setbadat(0))->isbad);
}
