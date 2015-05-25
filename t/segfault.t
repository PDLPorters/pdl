# Tests for a segfault bug in PDL through 2.4.2
# (Thanks, Alexey!)

use Test::More tests => 1;
use Test::Exception;

use PDL;
use strict;
use warnings;

my $x = pdl(1,2);
my $y = bless \my $z,"ASFG";
throws_ok {
	$x != $y
} qr/Error - tried to use an unknown/;
