# -*-perl-*-

# Test PDL::AutoLoader

use strict;
use Test;

BEGIN {
#    plan tests => 1, todo => [1];
    plan tests => 1;
}

use PDL::LiteF;
use PDL::AutoLoader;

use vars qw( @PDLLIB );

@PDLLIB = ("t/"); # this means you have to run the test from ../t

my $x = long(2 + ones(2,2));

my $y = func($x);

ok( sum($y), 4*29 );

