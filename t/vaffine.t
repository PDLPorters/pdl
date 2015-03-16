use Test::More tests => 1;

# Test vaffine optimisation

use PDL::LiteF;

$x = zeroes(100,100);

$y = $x->slice('10:90,10:90');

$y++;

ok( (not $y->allocated) ) ;
