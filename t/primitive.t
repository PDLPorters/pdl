# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b) = @_;
	print "APPROX: $a $b\n";
	if((join ',',$a->dims) ne (join ',',$b->dims)) {
		print "UNEQDIM\n";
		return 0;
	}
	my $c = abs($a-$b);
	my $d = max($c);
	if($d >= 0.01) {
		print "# APPROXFAIL: $a $b\n";
	}
	$d < 0.01;
}

print "1..6\n";

$a = PDL->pdl([[5,4,3],[2,3,1.5]]);

ok(1,approx($a->average(), PDL->pdl([4, 2.16666])));
ok(2,approx($a->sumover(), PDL->pdl([12, 6.5])));
ok(3,approx($a->prodover(), PDL->pdl([60, 9])));

$b = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
print "B: $b\n";
$c = ($b->xvals) + 10;
# print "C: $c\n";

# print "BW: ", $b->where, "\n";

ok(4,approx($b->where($b>4), PDL->pdl(5,6)));
ok(5,approx($b->which, PDL->pdl(0,1,2,7,8,10,11)));
# print "B, ",$b->which();
# print "C: $c\n";
# print "\nCI, ", $c->index($b->which());
# print "D\n";
ok(6,approx($c->where($b), PDL->pdl(10,11,12,17,18,20,21)));

