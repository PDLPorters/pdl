use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
	return $d < 0.01;
}

print "1..1\n";

# This is something that would cause an exception on 1.91_00:
# when the original was undef'd, xchghashes would barf.

$a = xvals zeroes(5,5);

$b = $a->slice(':,2:3');

$a = 1;  # Undefine orig. a

$b += 1;

ok(1,1);
