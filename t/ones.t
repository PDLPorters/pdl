# Test other such primitives also
use Test::More tests => 4;

use strict;
use warnings;

use PDL::LiteF;
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $pb = double ones(2,3);

my $ind = 1;

is(($pb->dims)[0], 2);
is(($pb->dims)[1], 3);
note $pb;
is($pb->at(1,1), 1);
is($pb->at(1,2), 1);
