# Test the dataflow magic & binding stuff
# XXX DISABLED!
use Test::More skip_all => 'Disabled';

use PDL::LiteF;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

plan tests => 6;

$a = pdl 2,3,4;
$a->doflow();

$b = $a + 1;

$c = $b * 2;

@cl = (-1,-1,-1);

$c->bind(sub{ @cl = $c->list() });

ok( ((join ',',@cl) eq "-1,-1,-1"));

$a->set(0,5);

ok( ((join ',',@cl) eq "-1,-1,-1"));

$a->set(1,6);

ok( ((join ',',@cl) eq "-1,-1,-1"));

PDL::dowhenidle();

ok( ((join ',',@cl) eq "12,14,10"));

$a->set(2,7);

ok( ((join ',',@cl) eq "12,14,10"));

PDL::dowhenidle();

ok( ((join ',',@cl) eq "12,14,16"));
