# -*-perl-*-

use Test;

BEGIN {plan tests=>1;}

use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $a = sequence(41) - 20;
$a /= 4;
#do test on quarter-integers, to make sure we're not crazy.

my $ans_rint = pdl(-5,-5,-4,-4,-4,-4,-4,-3,-3,-3,-2,-2,-2,-2,-2,
-1,-1,-1,0,0,0,0,0,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,4,5,5);

ok(all(rint($a)==$ans_rint));
