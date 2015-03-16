use Test::More tests => 1;
use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# This is something that would cause an exception on 1.91_00:
# when the original was undef'd, xchghashes would barf.

$a = xvals zeroes(5,5);

$b = $a->slice(':,2:3');

$a = 1;  # Undefine orig. a

$b += 1;

ok 1;
