# This test case is very similar to pthread.t, but it uses the auto pthread
#  interface, instead of specificaly setting pthread magic on individual PDLs

use Test::More;
use PDL::LiteF;
use Benchmark ':hireswallclock';

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
       my($a,$b,$mdiff) = @_;
       $mdiff = 0.01 unless defined($mdiff);
       my $c = abs($a-$b);
       my $d = max($c);
       $d < $mdiff;
}

plan skip_all => 'No threads' if !PDL::Core::pthreads_enabled;
plan tests => 26;

$a = zeroes(2000000);
$b = zeroes(2000000);

# Set target of 10 threads to create, with no lower limit on the size
#   of the PDL
set_autopthread_targ(10);
set_autopthread_size(0);
  
timethese(20,{threaded => '$a **= 1.3'});

ok( get_autopthread_actual() == 10); # should have split into 10 threads

# Set target to 0 for comparison to unthreaded
set_autopthread_targ(0);
timethese(20,{unthreaded => '$b **= 1.3'});

print $a->slice('0:20'),"\n";
ok(tapprox($a,$b));

# Another Test Case
$a = sequence(3,10);
$b = ones(3);
set_autopthread_targ(2);
$c = inner $a, $b;
print $c,"\n";
$cc = $a->sumover;
print $cc,"\n";
ok(tapprox($c,$cc));

# Try multi-dim cases
set_autopthread_targ(2);
$a = zeroes(200000,2,2);
$b = zeroes(200000,2,2);
$a+=1;
set_autopthread_targ(0); # Turn off pthreading for $b adding
$b+=1; 
ok( tapprox($a, $b));

### Multi-dimensional incrementing case ###
##  This is performed multiple times to be sure that indexing isn't
##  messed up for the multiple pthreads
my $testNo = 5;
set_autopthread_targ(2);
foreach (1..20){
      $a = zeroes(3, 200000,2,2);
      $a += 1;
      ok( $a->max <  1.1  ); # Should never be greater than 1
}


### Pthread Indexing Test ####
###  This checks for a problem seen in the dataflow back to the parent PDL (i.e. writeback xs code)
###    seen when pthreading is present 

my $indexArg = pdl [[1]];

my $lutEx = pdl [[1,0],[0,1]];

# Do a pthreaded index operation
$in = $lutEx->index($indexArg);

# Do inplace assignment so that data is written back to the parent pdl:
#   The lazy evaluation of the index operation will occur here first
$in .= 1;

# Check for writeback to the parent PDL working (should have three ones in the array)
my $lutExSum = $lutEx->sum;
ok( tapprox($lutExSum, pdl(3)) );

# Check for inplace assignment working. $in should be all ones
my $inSum = $in->sum;
ok( tapprox($inSum, pdl(2) ) );
