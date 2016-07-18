# -*-perl-*-
use strict;
use warnings;
use Test::More tests => 2;
use PDL::LiteF;
use PDL::Math;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.01;
}

my $a = pdl( 0.01, 0.0 );
ok( tapprox( erfi($a), pdl(0.00886,0.0) ), "erfi");

# inplace
$a->inplace->erfi;
ok( tapprox( $a, pdl(0.00886,0.0) ), "erfi inplace");
