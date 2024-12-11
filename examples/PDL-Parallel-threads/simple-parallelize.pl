use strict;
use warnings;
use PDL::Parallel::threads::SIMD qw(parallelize parallel_id);
 parallelize {
   my $pid = parallel_id;
   print "Hello from parallel thread $pid\n"
 } 10;
