# -*-perl-*-

use Test;

BEGIN { plan tests => 5; }

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub approx {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

ok( approx(bessj0(0.5),0.9384) && approx(bessj0(0),1) );
ok( approx(bessj1(0.1),0.0499) && approx(bessj1(0),0) );
ok( approx(bessjn(0.8,3),0.010) && approx(bessyn(0.2,2),-32.15714) );

# test inplace
$a = pdl(0.5,0.0);
$a->inplace->bessj0;
ok( approx($a,pdl(0.9384,1)) );

$a = pdl(0.2);
$a->inplace->bessyn(2);
ok( approx( $a, -32.15714 ) );   # 5
