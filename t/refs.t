# This test case points out a problem in the freeing
# of used memory in 1.90_01
use Test::More tests => 2;
use PDL::LiteF;
use Test::Exception;
# PDL::Core::set_debugging(1);

use strict;
use warnings;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

lives_ok {
my $pa = pdl (1,2);
my $pb = pdl [[1,2],[1,2],[1,2]];
my $pc = $pa->slice(',*3');
$pc->make_physical;
$pc = $pb->clump(2);

$pb->make_physical;
$pc->make_physical;
};

lives_ok {
my $pa =  zeroes 4,5;

my $pb = $pa->slice('1:3:2,2:4:2');

$pb .=  ones(2,2);

note $pa;
};
