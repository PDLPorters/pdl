# -*-perl-*-

use Test;

BEGIN { plan tests => 5; }

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
    my($a,$b) = @_;
    $c = abs($a-$b);
    $d = max($c);
    $d < 0.01;
}

ok( tapprox(erf(0.),0.) && tapprox(erf(30.),1.) );
ok( tapprox(erf(0.5),1.-erfc(0.5)) );
ok( tapprox(erf(erfi(0.5)),0.5) && tapprox(erfi(erf(0.5)),0.5) );

# now test inplace
$a = pdl(0.0,30.0);
$a->inplace->erf;
ok( tapprox( $a, pdl(0.0,1.0) ) );

$a = pdl(0.5);
$a->inplace->erfc; 
ok( tapprox( 1.0-$a, erf(0.5) ) );
