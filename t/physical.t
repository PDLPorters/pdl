# Test ->*physical*(). This is not yet good enough: we need
# nasty test cases,

use Test::More skip_all => 'Disabled';
use PDL::LiteF;

use strict;
use warnings;

plan tests => 6;

my $pa = zeroes(4,4);
ok($pa->isphysical());

my $pb = xvals $pa + 0.1 * yvals $pa;
my $pc = $pb->slice("1:3:2,:");
ok(! $pc->isphysical());

my $pd = $pb->physical();
ok( $pd == $pb);

my $pe = $pc->physical();
ok( $pe != $pc);
ok( $pe->isphysical());

ok( all approx($pc,$pe, 0.01));
