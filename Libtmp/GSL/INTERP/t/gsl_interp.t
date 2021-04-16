
# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;
use PDL::GSL::INTERP;
use Test::More;

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
eval { $spl->eval(10)     }; like($@,qr/input domain error/, 'eval 10 (extrapolation not allowed)') ;
eval { $spl->deriv(10)    }; like($@,qr/input domain error/, 'deriv 10 (extrapolation not allowed)' );
eval { $spl->deriv2(10)   }; like($@,qr/input domain error/, 'deriv2 10 (extrapolation not allowed)');
eval { $spl->integ(8.5,10)}; like($@,qr/input domain error/, 'integ 8.5 to 10 (extrapolation not allowed)');

# Bad value test added 5/31/2005 D. Hunt

ok ($spl->eval(pdl(0)->setbadat(0))->isbad, 'cspline eval w badvalue');

# Exception handling test added 10/18/2010 Jason Lin

my $nx = ($x)*($x<=3) + ($x-1)*($x>3); # x value not monotonically increasing
my $i; eval { $i = PDL::GSL::INTERP->init('cspline',$nx, $y) };

like($@,qr/invalid argument supplied by user/,"module exception handling");

done_testing;
