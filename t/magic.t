# Test the dataflow magic & binding stuff
# XXX DISABLED!
use Test::More skip_all => 'Disabled';

use PDL::LiteF;

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

plan tests => 6;

my $pa = pdl 2,3,4;
$pa->doflow();

my $pb = $pa + 1;

my $pc = $pb * 2;

my @pcl = (-1,-1,-1);

$pc->bind(sub{ @pcl = $pc->list() });

is( (join ',',@pcl), "-1,-1,-1");

$pa->set(0,5);

is( (join ',',@pcl), "-1,-1,-1");

$pa->set(1,6);

is( (join ',',@pcl), "-1,-1,-1");

PDL::dowhenidle();

is( (join ',',@pcl), "12,14,10");

$pa->set(2,7);

is( (join ',',@pcl), "12,14,10");

PDL::dowhenidle();

is( (join ',',@pcl), "12,14,16");
