
# Test hist() function

use PDL::LiteF;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	if($ENV{PDL_T}) {
		if($result) { print "ok $no\n";return }
		my ($p,$f,$l) = caller;
		print "FAILED TEST $no AT $p $f $l\n";
	} else {
		print "not " unless $result ;
		print "ok $no\n" ;
	}
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..2\n";

$x = pdl [15.4,15.8,16.01,16.9,16.1,15.2,15.4,16.2,15.4,16.2,16.4];
($hx,$h)= hist ($x,15,17,0.1);

ok(1, tapprox($hx, pdl(qw/15.05   15.15 15.25   15.35   15.45   15.55   15.65
   15.75   15.85   15.95   16.05   16.15 16.25   16.35   16.45   16.55   16.65
   16.75   16.85   16.95/)) );

ok(2, tapprox($h, pdl(qw/0 1 0 0 3 0 0 0 1 0 1 3 0 1 0 0 0 0 1 0/)) );

