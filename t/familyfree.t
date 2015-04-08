use PDL::LiteF;

use strict;
use warnings;

use Test::More tests => 1;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# This is something that would cause an exception on 1.91_00:
# when the original was undef'd, xchghashes would barf.

my $pa = xvals zeroes(5,5);
my $pb = $pa->slice(':,2:3');

$pa = 1;  # Undefine orig. a
$pb += 1;

ok(1);
