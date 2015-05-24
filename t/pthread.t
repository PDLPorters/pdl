use Test::More;
use PDL::LiteF;
use Benchmark;  # not using ':hireswallclock'

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

plan skip_all => 'No threads' if !PDL::Core::pthreads_enabled;
plan tests => 27;

approx( pdl(0), pdl(0), 0.01); # set eps

{
my $pa = zeroes(2000000);
my $pb = zeroes(2000000);

$pa->add_threading_magic(0,10);

timethese(50,{threaded => '$pa += 1', unthreaded => '$pb+= 1'});
print $pa->slice('0:20'),"\n";
ok(all approx($pa,$pb));
}

{
my $pa = sequence(3,10);
my $pb = ones(3);
$pa->add_threading_magic(1,2);
my $pc = inner $pa, $pb;
print $pc,"\n";
$pa->remove_threading_magic;
my $cc = $pa->sumover;
print $cc,"\n";
ok(all approx($pc,$cc));
}

{
# Try multi-dim cases
my $pa = zeroes(200000,2,2);
my $pb = zeroes(200000,2,2);
$pa->add_threading_magic(0,2);
$pa+=1;
$pb+=1;
ok( all approx($pa, $pb));
}

### Multi-dimensional incrementing case ###
##  This is performed multiple times to be sure that indexing isn't
##  messed up for the multiple pthreads
my $pa;
foreach (1..20){
     $pa = zeroes(3, 200000,2,2);
     $pa->add_threading_magic(1,2);
     $pa += 1;
     ok( $pa->max <  1.1  ); # Should never be greater than 1
}


{
### Pthread Indexing Test ####
###  This checks for a problem seen in the dataflow back to the parent PDL (i.e. writeback xs code)
###    seen when pthreading is present 

my $indexArg = pdl [[1]];

my $lutEx = pdl [[1,0],[0,1]];

# Do a pthreaded index operation
$lutEx->add_threading_magic(1,2);
my $in = $lutEx->index($indexArg);

# Remove pthreading magic. This is a check to see if pthreading doesn't cause
#   errors in the lazy evaluation of the index operation that occurs in the following
#   inplace-assignment operation.
$lutEx->add_threading_magic(-1,-1);

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

{
### Pthread Indexing Test ####
###  Similar test to above, but the pthreading magic is changed (not just
###  deleted) after the index operation 

my $indexArg = pdl [[1]];

my $lutEx = pdl [[1,0,0,1],[0,1,0,1]];

# Do a pthreaded index operation
$lutEx->add_threading_magic(1,2);
my $in = $lutEx->index($indexArg);

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
my $lutExSum = $lutEx->sum;
ok( all approx($lutExSum, pdl(5)) );

# Check for inplace assignment working. $in should be all ones
my $inSum = $in->sum;
ok( all approx($inSum, pdl(2) ) );
}
