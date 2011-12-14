# Test the dataflow magic & binding stuff
# XXX DISABLED!

use PDL::LiteF;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

print "1..1\n";

ok(1,1);

if (0) {

print "1..6\n";

$ind=1;

$a = pdl 2,3,4;
$a->doflow();

$b = $a + 1;

$c = $b * 2;

@cl = (-1,-1,-1);

$c->bind(sub{ @cl = $c->list() });

ok($ind++, ((join ',',@cl) eq "-1,-1,-1"));

$a->set(0,5);

ok($ind++, ((join ',',@cl) eq "-1,-1,-1"));

$a->set(1,6);

ok($ind++, ((join ',',@cl) eq "-1,-1,-1"));

PDL::dowhenidle();

ok($ind++, ((join ',',@cl) eq "12,14,10"));

$a->set(2,7);

ok($ind++, ((join ',',@cl) eq "12,14,10"));

PDL::dowhenidle();

ok($ind++, ((join ',',@cl) eq "12,14,16"));

}
