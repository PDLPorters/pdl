# -*-perl-*-

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Test;
plan tests => 2;

sub approx {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

$a = pdl( 0.01, 0.0 );
ok( approx( erfi($a), pdl(0.00886,0.0) ) );

# inplace
$a->inplace->erfi;
ok( approx( $a, pdl(0.00886,0.0) ) );
