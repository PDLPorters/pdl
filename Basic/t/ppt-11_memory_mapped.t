use strict;
use warnings;

BEGIN {
	use Config;
	if (! $Config{'useithreads'}) {
		print("1..0 # Skip: Perl not compiled with 'useithreads'\n");
		exit(0);
	}
}

use threads;
use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls);
use PDL::IO::FastRaw;

use Test::More;

my $N_threads = 10;
mapfraw('foo.dat', {Creat => 1, Dims => [$N_threads], Datatype => double})
		->share_as('workspace');

# Spawn a bunch of threads that do the work for us
use PDL::NiceSlice;
threads->create(sub {
	my $tid = shift;
	my $workspace = retrieve_pdls('workspace');
	$workspace($tid) .= sqrt($tid + 1);
}, $_) for 0..$N_threads-1;

# Reap the threads
for my $thr (threads->list) {
	$thr->join;
}

my $expected = (sequence($N_threads) + 1)->sqrt;
my $workspace = retrieve_pdls('workspace');
ok(all($expected == $workspace), 'Sharing memory mapped ndarrays works');

END {
	# Clean up the testing files
	unlink $_ for qw(foo.dat foo.dat.hdr);
}

done_testing;
