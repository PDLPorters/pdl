# Test other such primitives also

use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..4\n";

$b = double ones(2,3);

$ind=1;


ok($ind++,($b->dims)[0] == 2);
ok($ind++,($b->dims)[1] == 3);
print $b;
ok($ind++,($b->at(1,1)) == 1);
ok($ind++,($b->at(1,2)) == 1);
