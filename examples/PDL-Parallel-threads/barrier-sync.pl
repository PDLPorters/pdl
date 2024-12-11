use strict;
use warnings;

use PDL;
use PDL::NiceSlice;
use PDL::Parallel::threads qw(retrieve_pdls);
use PDL::Parallel::threads::SIMD qw(parallelize parallel_sync parallel_id);
my $pdl = zeroes(20);
$pdl->share_as('test');
#undef($pdl);

# Create and share a slice
my $slice = $pdl(10:15)->sever;
$slice->share_as('slice');

# Create and share a memory mapped ndarray
use PDL::IO::FastRaw;
my $mmap = mapfraw('foo.bin', {Creat => 1, Datatype => double, Dims => [50]});
$mmap->share_as('mmap');

END {
	unlink 'foo.bin';
	unlink 'foo.bin.hdr';
}

my $N_threads = 5;

parallelize {
	my $tid = parallel_id;
	my $pdl = retrieve_pdls('test');

	print "Thread id $tid says the ndarray is $pdl\n";
	parallel_sync;

	my $N_data_to_fix = $pdl->nelem / $N_threads;
	my $idx = sequence($N_data_to_fix) * $N_threads + $tid;
	$pdl($idx) .= $tid;
	parallel_sync;

	print "After set, thread id $tid says the ndarray is $pdl\n";
	parallel_sync;

	$pdl->set($tid, 0);
	parallel_sync;

	print "Thread id $tid says the ndarray is now $pdl\n";
} $N_threads;

print "mmap is $mmap\n";
parallelize {
	my $tid = parallel_id;
	my $mmap = retrieve_pdls('mmap');

	$mmap($tid) .= $tid;
} $N_threads;

print "now mmap is $mmap\n";

parallelize {
	my $tid = parallel_id;
	my $pdl = retrieve_pdls('test');

	print "Thread id is $tid\n";

	my $N_data_to_fix = $pdl->nelem / $N_threads;
	my $idx = sequence($N_data_to_fix - 1) * $N_threads + $tid;
	use PDL::NiceSlice;
	$pdl($idx) .= -$tid;

	my $slice = retrieve_pdls('slice');
	$slice($tid) .= -10 * $tid;
} $N_threads;

print "Final ndarray value is $pdl\n";
