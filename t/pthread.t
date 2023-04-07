use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Benchmark qw(timethese :hireswallclock);

plan skip_all => 'No threads' if !PDL::Core::pthreads_enabled;

approx( pdl(0), pdl(0), 0.01); # set eps
set_autopthread_size(0);

for (
  [ 6, [6], 6, 0 ],
  [ 6, [5], 5, 0 ],
  [ 6, [4], 4, 0 ],
  [ 6, [7], 6, 0 ],
  [ 6, [7,12], 6, 1 ],
  [ 6, [5,12], 6, 1 ],
  [ 6, [12,7], 6, 0 ],
  [ 6, [12,5], 6, 0 ],
  [ 6, [7,5], 6, 0 ],
  [ 6, [4,5], 5, 1 ],
  [ 6, [5,4], 5, 0 ],
  [ 6, [4,5,12], 6, 2 ],
  [ 4, [9,6,2], 4, 1 ],
  [ 4, [6,9,2], 4, 0 ],
) {
  my ($thr_num, $size, $thr_want, $dim) = @$_;
  set_autopthread_targ($thr_num);
  (my $t = zeroes(@$size))++;
  is(get_autopthread_actual(), $thr_want, "right pthread no");
  is(get_autopthread_dim(), $dim, "right pthread dim");
}
set_autopthread_targ(0);

my ($pa, $pb, $thr_want);
my @T = (
  [
    sub { $_[0]->add_threading_magic(@_[1, 2]) },
    sub { $_[0]->remove_threading_magic },
    sub {},
    {threaded => sub { $pa **= 1.3 }, unthreaded => sub { $pb **= 1.3 }},
    'explicit',
  ],
  [
    sub { set_autopthread_targ($thr_want = $_[2]) },
    sub { set_autopthread_targ(0); },
    sub { is(get_autopthread_actual(), $thr_want, "right threadno auto") },
    {threaded => sub { $pa **= 1.3 }},
    'auto',
  ],
);

for (@T) {
  my ($thr_on, $thr_off, $thr_check, $bench_hash, $label) = @$_;
  {
  $pa = zeroes(2000000);
  $pb = zeroes(2000000);

  $thr_on->($pa, 0, 9);

  my $bench = timethese(20, $bench_hash);
  #diag explain $bench;

  $thr_check->();

  ok all(approx $pa,$pb), "pa and pb match $label" or diag "diff at:", ($pa != $pb)->whichND."";
  }

  {
  $pa = sequence(3,10);
  $pb = ones(3);
  $thr_on->($pa, 1, 2);
  my $pc = inner $pa, $pb;
  $thr_off->($pa);
  my $cc = $pa->sumover;
  ok all(approx($pc,$cc)), "inner $label" or diag "pc=$pc\ncc=$cc";
  }

  {
  # Try multi-dim cases
  $pa = zeroes(200000,2,2);
  $pb = zeroes(200000,2,2);
  $thr_on->($pa, 0, 2);
  $pa+=1;
  $thr_off->($pb);
  $pb+=1;
  ok all(approx $pa, $pb), "+= $label";
  }

  ### Multi-dimensional incrementing case ###
  ##  This is performed multiple times to be sure that indexing isn't
  ##  messed up for the multiple pthreads
  foreach (1..20){
    $pa = zeroes(3, 200000,2,2);
    $thr_on->($pa, 1, 2);
    $pa += 1;
    ok( $pa->max < 1.1, "multi-run $label" ); # Should never be greater than 1
  }

  {
  ### Pthread Indexing Test ####
  ###  This checks for a problem seen in the dataflow back to the parent PDL (i.e. writeback xs code)
  ###    seen when pthreading is present

  my $indexArg = pdl [[1]];

  my $lutEx = pdl [[1,0],[0,1]];

  # Do a pthreaded index operation
  $thr_on->($lutEx, 1, 2);
  my $in = $lutEx->index($indexArg);

  # Remove pthreading magic. This is a check to see if pthreading doesn't cause
  #   errors in the lazy evaluation of the index operation that occurs in the following
  #   inplace-assignment operation.
  $thr_off->($lutEx);

  # Do inplace assignment so that data is written back to the parent pdl:
  #   The lazy evaluation of the index operation will occur here first
  $in .= 1;

  # Check for writeback to the parent PDL working (should have three ones in the array)
  my $lutExSum = $lutEx->sum;
  ok all(approx($lutExSum, pdl(3))), "writeback $label";

  # Check for inplace assignment working. $in should be all ones
  my $inSum = $in->sum;
  ok all(approx($inSum, pdl(2) )), "inplace $label";
  }

  {
  ### Pthread Indexing Test ####
  ###  Similar test to above, but the pthreading magic is changed (not just
  ###  deleted) after the index operation

  my $indexArg = pdl [[1]];

  my $lutEx = pdl [[1,0,0,1],[0,1,0,1]];

  # Do a pthreaded index operation
  $thr_on->($lutEx, 1, 2);
  my $in = $lutEx->index($indexArg);

  $in->make_physical; # make sure the initial indexing operation has taken place
                      # otherwise gets defered due to lazy evaluation.

  # Remove pthreading magic, and then add it back on another dim with
  #  4 threads.  This is a check to see if pthreading doesn't cause
  #   errors in the writeback-code of the index operation that occurs in the following
  #   inplace-assignment operation.
  $thr_off->($lutEx);
  $thr_on->($lutEx, 0, 4);

  # Do inplace assignment so that data is written back to the parent pdl:
  #   The lazy evaluation of the index operation will occur here first
  $in .= 1;

  # Check for writeback to the parent PDL working (should have three ones in the array)
  my $lutExSum = $lutEx->sum;
  ok all(approx($lutExSum, pdl(5))), "writeback with different magic $label";

  # Check for inplace assignment working. $in should be all ones
  my $inSum = $in->sum;
  ok all(approx($inSum, pdl(2))), "inplace with different magic $label";
  }
}

# These tests check for proper deferred handling of barf and warn messages when pthreading.

## Check Handling of barf messages when pthreading ###
# These statements will cause pthread to happen in two pthreads
set_autopthread_targ(2);
set_autopthread_size(0);

# Because of the duplicate 8's interpolates barf (in the PPcode) will get
#  called. This should not cause a segfault
my $x = float( [1, 2, 3, 4, 5, 8, 9, 10], [1, 2, 3, 4, 5, 8, 8, 8] );
my $y = ($x * 3) * ($x - 2);

# Setup to silence warning messages
local $SIG{__WARN__} = sub {  };
# Catch barf messages by running in eval:
eval{
   my ( $ans, $err ) = interpolate(8.5, $x, $y );
};

like( $@, qr/identical abscissas/ , "interpolate barf" );

# warning message segfaults when pthreaded if messages not deferred properly
my $mask = zeroes(5,5);
local $SIG{__WARN__} = sub { die $_[0] };
$mask->badvalue(1);
eval{ PDL::gt($mask, 2, 0) };
like( $@, qr/badvalue is set to/, "safe barf" );

done_testing;
