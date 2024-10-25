use strict;
use warnings;
use Test::More;
use PDL::MyInlineMod;
use PDL::LiteF;
my $x = zeroes 10;
my $y = $x->myinc;

ok all $y == 1;

my $c = $x->plus2;
ok all $c == 2;

done_testing;
