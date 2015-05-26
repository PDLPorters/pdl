use strict;
use warnings;
use Test::More tests => 4;

use PDL::LiteF;
#If we've gotten this far and these tests fail, we're really in trouble!
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $a0 = zeroes(3,2);

my $a1 = xvals $a0;

is($a1->at(0,0), 0, "xvals 0,0 == 0");
is($a1->at(1,0), 1, "xvals 1,0 == 1");
is($a1->at(2,0), 2, "xvals 2,0 == 2");
is($a1->at(1,1), 1, "xvals 1,1 == 1");
