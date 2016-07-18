# -*-perl-*-
use strict;
use warnings;
use Test::More tests => 5;
use PDL::LiteF;
use PDL::Math;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
    my($a,$b) = @_;
    my $c = abs($a-$b);
    my $d = max($c);
    $d < 0.01;
}

ok( tapprox(erf(0.),0.) && tapprox(erf(30.),1.),"erf(0), erf(30)");
ok( tapprox(erf(0.5),1.-erfc(0.5)), "erf and erfc");
ok( tapprox(erf(erfi(0.5)),0.5) && tapprox(erfi(erf(0.5)),0.5), "erfi (both ways)");

# now test inplace
$a = pdl(0.0,30.0);
$a->inplace->erf;
ok( tapprox( $a, pdl(0.0,1.0) ), "erf inplace");

$a = pdl(0.5);
$a->inplace->erfc;
ok( tapprox( 1.0-$a, erf(0.5) ), "erfc inplace");
