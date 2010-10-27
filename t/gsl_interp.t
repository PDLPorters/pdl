
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
    plan tests => 12;
  }
}

my $x = sequence(10);
my $y = exp($x);
my $spl = PDL::GSL::INTERP->init('cspline',$x,$y);

ok(defined $spl, 'create cspline');

my $spl2 = PDL::GSL::INTERP->init('cspline',$x,$y,{Sort => 0});

ok(defined $spl2, 'create cspline no sort');

ok(abs($spl->eval(5.5)-237.641810667697) < 1e-6,  'eval 5.5'   );
ok(abs($spl->deriv(5.5)-237.669424604497) < 1e-6, 'deriv 5.5'  );
ok(abs($spl->deriv2(5.5)-306.23332503967) < 1e-6, 'deriv2 5.5' );
ok(abs($spl->integ(3.2,8.5)-4925.23555581654) < 1e-6, 'integ 3.2 to 8.5' );   
ok(abs($spl->eval(5.5,{Extrapolate => 1})-237.641810667697) < 1e-6, 'eval 5.5 w Extrapolate' );
ok(abs($spl->deriv(5.5,{Extrapolate => 1})-237.669424604497) < 1e-6, 'deriv 5.5 w Extrapolate' );
ok(abs($spl->deriv2(5.5,{Extrapolate => 1})-306.23332503967) < 1e-6, 'deriv2 5.5 w Extrapolate' );
ok(abs($spl->integ(3.2,8.5,{Extrapolate => 1})-4925.23555581654) < 1e-6, 'integ 3.2 to 8.5 w Extrapolate' );   

# Bad value test added 5/31/2005 D. Hunt

SKIP: {
    skip "Test not valid without bad value support", 1 unless $PDL::Bad::Status;
    ok ($spl->eval(pdl(0)->setbadat(0))->isbad, 'cspline eval w badvalue');
}

# Exception handling test added 10/18/2010 Jason Lin

my $nx = ($x)*($x<=3) + ($x-1)*($x>3); # x value not monotonically increasing
my $i; eval { $i = PDL::GSL::INTERP->init('cspline',$nx, $y) };

like($@,qr/invalid argument supplied by user/,"module exception handling");
