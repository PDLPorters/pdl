# -*-perl-*-

use Test;

BEGIN { plan tests => 2; }

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

$a = pdl( 0.01, 0.0 );
ok( tapprox( erfi($a), pdl(0.00886,0.0) ) );

# inplace
$a->inplace->erfi;
ok( tapprox( $a, pdl(0.00886,0.0) ) );
