# Tests for a segfault bug in PDL through 2.4.2
# (Thanks, Alexey!)
no warnings qw(misc);

print "1..1\n";

use PDL;
use strict;
my $x = pdl(1,2);
my $y = bless \my $z,"ASFG";
eval '$x != $y';
print "ok 1\n" if($@ =~/Error - tried to use an unknown/);
