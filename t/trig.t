# -*-perl-*-

use Test;

BEGIN { plan tests => 4; }

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
    my($a,$b) = @_;
    $c = abs($a-$b);
    $d = max($c);
    $d < 0.01;
}

ok( tapprox(sinh(0.3),0.3045) && tapprox(acosh(42.1),4.43305) );
ok( tapprox(acos(0.3),1.2661) && tapprox(tanh(0.4),0.3799) );
ok( tapprox(cosh(2.0),3.7621) && tapprox(atan(0.6),0.54041) );

# inplace
$a = pdl(0.3);
$a->inplace->sinh;
ok( tapprox($a, pdl(0.3045)) );
