use strict;
use warnings;

my $N_threads = $ARGV[0] || 2;

use PDL;
use PDL::Parallel::threads qw(retrieve_pdls);
use PDL::Parallel::threads::SIMD qw(parallel_sync parallelize parallel_id);
zeroes(100_000_000)->share_as('test');
use PDL::IO::FastRaw;
mapfraw('foo.dat', {Creat => 1, Dims => [$N_threads], Datatype => double})
	->share_as('mapped');

print "Main thread is about to rest for 5 seconds\n";
sleep 5;

parallelize {
	my $tid = parallel_id;
	my ($pdl, $mapped) = retrieve_pdls('test', 'mapped');

	print "Thread id $tid is about to sleep for 5 seconds\n";
	parallel_sync;
	sleep 5;
	parallel_sync;
} $N_threads;


END {
	# Clean up the testing files
	unlink $_ for qw(foo.dat foo.dat.hdr);
}
