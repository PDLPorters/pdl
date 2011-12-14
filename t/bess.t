# -*-perl-*-

use Test;

BEGIN { plan tests => 6; }

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

ok( tapprox(bessj0(0.5),0.9384) && tapprox(bessj0(0),1) );
ok( tapprox(bessj1(0.1),0.0499) && tapprox(bessj1(0),0) );
ok( tapprox(bessjn(0.8,3),0.010) && tapprox(bessyn(0.2,2),-32.15714) );

# test inplace
$a = pdl(0.5,0.0);
$a->inplace->bessj0;
ok( tapprox($a,pdl(0.9384,1)) );

$a = pdl(0.2);
$a->inplace->bessyn(2);
ok( tapprox( $a, -32.15714 ) );   # 5

ok( tapprox( pow(2,3),8)); # test for the pow bug
