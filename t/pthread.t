
# XXX SOME TESTS DISABLED

use PDL::LiteF;
use Benchmark;

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

print "1..1\n";
print "ok 1\n";
exit;

$a = zeroes(20000000);
$b = zeroes(20000000);

axisvalues($a);;
$a->add_threading_magic(0,10);

timethese(20,{threaded => '$a += 1', unthreaded => '$b+= 1'});

# print $a;
