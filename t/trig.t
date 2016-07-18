# -*-perl-*-
use strict;
use warnings;
use Test::More tests => 4;

use PDL::LiteF;
use PDL::Math;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
    my($a,$b) = @_;
    my $c = abs($a-$b);
    my $d = max($c);
    $d < 0.01;
}

ok( tapprox(sinh(0.3),0.3045) && tapprox(acosh(42.1),4.43305), "sinh, acosh");
ok( tapprox(acos(0.3),1.2661) && tapprox(tanh(0.4),0.3799), "acos, tanh");
ok( tapprox(cosh(2.0),3.7621) && tapprox(atan(0.6),0.54041), "cosh, atan");

# inplace
$a = pdl(0.3);
$a->inplace->sinh;
ok( tapprox($a, pdl(0.3045)), "sinh inplace");
