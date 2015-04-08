# Test other such primitives also
use Test::More tests => 4;

use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

$b = double ones(2,3);

$ind=1;

ok(($b->dims)[0] == 2);
ok(($b->dims)[1] == 3);
note $b;
ok(($b->at(1,1)) == 1);
ok(($b->at(1,2)) == 1);
