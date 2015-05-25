use Test::More tests => 1;

# Test vaffine optimisation

use strict;
use warnings;

use PDL::LiteF;

my $x = zeroes(100,100);

my $y = $x->slice('10:90,10:90');

$y++;

ok( (not $y->allocated) ) ;
