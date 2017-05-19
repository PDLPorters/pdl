# -*-perl-*-
#
# Test indexed-combine operations:
#
#   add       -- see indadd in PDL::Primitive
#   mul       -- multiply
#   copy      -- assignment  (last value wins)
#   band      -- bitwise and (for integer data types)
#   bor       -- bitwise or  (for integer data types)
#   bxor      -- bitwise xor (for integer data types)
#   min       -- minimum
#   max       -- maximum
#   minloc    -- minimum value with its location
#   maxloc    -- maximum value with its location
#   minmax    -- minimum and maximum
#   minmaxloc -- minimum and maximum values and their locations
#
# For example, the add operation corresponds to indadd
# and minmax corresponds to indminmax...

# Testine utility function
sub tapprox {
    my($a,$b) = @_;
    my $c = abs($a-$b);
    my $d = ref($c) ? max($c) : $c ;  # no max if we are dealing with a scalar
    $d < 0.01;
}

use Test::More;
use PDL::LiteF;

use strict;

# indadd (from primitive2.t)
my $src = pdl( 1,2,3 );
my $ind = pdl( 1,4,6 );
my $dest = zeroes(10);
indadd($src, $ind, $dest);
ok( tapprox($dest->sum, $src->sum), "indadd maintains src values" );

$dest .= 0;
indadd($src, pdl(2), $dest);
ok( tapprox($dest->at(2), $src->sum), "indadd threaded to 1-bin" );

$src = pdl( 1, 2, 5, 0, 3, 8, 9, 4 );
$ind = indx( 1, 2, 1, 2, 1, 2, 1, 2);
my $ans = pdl q[0 18 14 0 0 0 0 0 0 0] ;
$dest .= 0;
indadd($src, $ind, $dest);
ok( tapprox($dest, $ans), "indadd threaded to 2-bins" );

done_testing;
