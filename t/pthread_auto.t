# This test case is very similar to pthread.t, but it uses the auto pthread
#  interface, instead of specificaly setting pthread magic on individual PDLs

use Test::More;
use PDL::LiteF;
use Benchmark ':hireswallclock';

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

plan skip_all => 'No threads' if !PDL::Core::pthreads_enabled;
plan tests => 26;

{
my $pa = zeroes(2000000);
my $pb = zeroes(2000000);

# Set target of 10 threads to create, with no lower limit on the size
#   of the PDL
set_autopthread_targ(10);
set_autopthread_size(0);
  
timethese(20,{threaded => sub { $pa **= 1.3 } });

ok( get_autopthread_actual() == 10); # should have split into 10 threads

# Set target to 0 for comparison to unthreaded
set_autopthread_targ(0);
timethese(20,{unthreaded => sub { $pb **= 1.3 } });

print $pa->slice('0:20'),"\n";
ok(all approx($pa,$pb));
}

{
# Another Test Case
my $pa = sequence(3,10);
my $pb = ones(3);
set_autopthread_targ(2);
my $pc = inner $pa, $pb;
print $pc,"\n";
my $cc = $pa->sumover;
print $cc,"\n";
ok(all approx($pc,$cc));
}

{
# Try multi-dim cases
set_autopthread_targ(2);
my $pa = zeroes(200000,2,2);
my $pb = zeroes(200000,2,2);
$pa+=1;
set_autopthread_targ(0); # Turn off pthreading for $pb adding
$pb+=1; 
ok( all approx($pa, $pb));
}

{
### Multi-dimensional incrementing case ###
##  This is performed multiple times to be sure that indexing isn't
##  messed up for the multiple pthreads
my $testNo = 5;
set_autopthread_targ(2);
my $pa;
foreach (1..20){
      $pa = zeroes(3, 200000,2,2);
      $pa += 1;
      ok( $pa->max <  1.1  ); # Should never be greater than 1
}
}


{
### Pthread Indexing Test ####
###  This checks for a problem seen in the dataflow back to the parent PDL (i.e. writeback xs code)
###    seen when pthreading is present 

my $indexArg = pdl [[1]];

my $lutEx = pdl [[1,0],[0,1]];

# Do a pthreaded index operation
my $in = $lutEx->index($indexArg);

# Do inplace assignment so that data is written back to the parent pdl:
#   The lazy evaluation of the index operation will occur here first
$in .= 1;

# Check for writeback to the parent PDL working (should have three ones in the array)
my $lutExSum = $lutEx->sum;
ok( all approx($lutExSum, pdl(3)) );

# Check for inplace assignment working. $in should be all ones
my $inSum = $in->sum;
ok( all approx($inSum, pdl(2) ) );
}
