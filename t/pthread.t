
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

sub tapprox {
       my($a,$b,$mdiff) = @_;
       $mdiff = 0.01 unless defined($mdiff);
       my $c = abs($a-$b);
       my $d = max($c);
       $d < $mdiff;
}

if (PDL::Core::pthreads_enabled) {
  print "1..2\n";
  $a = zeroes(2000000);
  $b = zeroes(2000000);
  
  $a->add_threading_magic(0,10);
  
  timethese(50,{threaded => '$a += 1', unthreaded => '$b+= 1'});
  print $a->slice('0:20'),"\n";
  ok(1,tapprox($a,$b));

  $a = sequence(3,10);
  $b = ones(3);
  $a->add_threading_magic(1,2);
  $c = inner $a, $b;
  print $c,"\n";
  $a->remove_threading_magic;
  $cc = $a->sumover;
  print $cc,"\n";
  ok(2,tapprox($c,$cc));
} else {
  print "1..1\n";
  print "ok 1\n";
}
