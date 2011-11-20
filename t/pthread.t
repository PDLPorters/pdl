
# XXX SOME TESTS DISABLED

use PDL::LiteF;
use Benchmark;  # not using ':hireswallclock'

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
  print "1..27\n";
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
  
  # Try multi-dim cases
  $a = zeroes(200000,2,2);
  $b = zeroes(200000,2,2);
  $a->add_threading_magic(0,2);
  $a+=1;
  $b+=1;
  ok(3, tapprox($a, $b));

  ### Multi-dimensional incrementing case ###
  ##  This is performed multiple times to be sure that indexing isn't
  ##  messed up for the multiple pthreads
  my $testNo = 4;
  foreach (1..20){
  	$a = zeroes(3, 200000,2,2);
  	$a->add_threading_magic(1,2);
	$a += 1;
	ok( $testNo++, $a->max <  1.1  ); # Should never be greater than 1
   }


   ### Pthread Indexing Test ####
   ###  This checks for a problem seen in the dataflow back to the parent PDL (i.e. writeback xs code)
   ###    seen when pthreading is present 

   my $indexArg = pdl [[1]];

   my $lutEx = pdl [[1,0],[0,1]];

   # Do a pthreaded index operation
   $lutEx->add_threading_magic(1,2);
   $in = $lutEx->index($indexArg);

   # Remove pthreading magic. This is a check to see if pthreading doesn't cause
   #   errors in the lazy evaluation of the index operation that occurs in the following
   #   inplace-assignment operation.
   $lutEx->add_threading_magic(-1,-1);
   
   # Do inplace assignment so that data is written back to the parent pdl:
   #   The lazy evaluation of the index operation will occur here first
   $in .= 1;

   # Check for writeback to the parent PDL working (should have three ones in the array)
   my $lutExSum = $lutEx->sum;
   ok( $testNo++, tapprox($lutExSum, pdl(3)) );

   # Check for inplace assignment working. $in should be all ones
   my $inSum = $in->sum;
   ok( $testNo++, tapprox($inSum, pdl(2) ) );


   ### Pthread Indexing Test ####
   ###  Similar test to above, but the pthreading magic is changed (not just
   ###  deleted) after the index operation 

   $indexArg = pdl [[1]];

   $lutEx = pdl [[1,0,0,1],[0,1,0,1]];

   # Do a pthreaded index operation
   $lutEx->add_threading_magic(1,2);
   $in = $lutEx->index($indexArg);

   $in->make_physical; # make sure the initial indexing operation has taken place
                       # otherwise gets defered due to lazy evaluation.
		       
   # Remove pthreading magic, and then add it back on another dim with
   #  4 threads.  This is a check to see if pthreading doesn't cause
   #   errors in the writeback-code of the index operation that occurs in the following
   #   inplace-assignment operation.
   $lutEx->add_threading_magic(-1,-1);
   $lutEx->add_threading_magic(0,4);
   
   # Do inplace assignment so that data is written back to the parent pdl:
   #   The lazy evaluation of the index operation will occur here first
   $in .= 1;

   # Check for writeback to the parent PDL working (should have three ones in the array)
   #print $lutEx;
   $lutExSum = $lutEx->sum;
   ok( $testNo++, tapprox($lutExSum, pdl(5)) );

   # Check for inplace assignment working. $in should be all ones
   $inSum = $in->sum;
   ok( $testNo++, tapprox($inSum, pdl(2) ) );

} else {
  print "1..1\n";
  print "ok 1\n";
}

