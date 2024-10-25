use strict;
use warnings;

BEGIN {
	use Config;
	if (! $Config{'useithreads'}) {
		print("1..0 # Skip: Perl not compiled with 'useithreads'\n");
		exit(0);
	}
}

# Tests if the threads can create data and share amongst themselves

use Test::More;
use Test::Exception;

use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls);
use PDL::Parallel::threads::SIMD qw(parallelize parallel_id parallel_sync);

# Run the parallel block in which the threads create and share each other's
# data
my $N_threads = 5;
my @data_is_correct : shared;
my @could_get_data : shared;
my @bad_is_correct : shared;
parallelize {
	my $pid = parallel_id;

	# Create data that is unique to this thread
	my $pdl = ones(10) * $pid;
	$pdl->share_as("data$pid");
	my $bad = ones(cfloat, 5);
	$bad->badvalue(17);
	$bad->setbadat(2);
	$bad->share_as("bad$pid");

	# We will get the data from the *previous* thread (modulo the number of
	# threads, of course: circular boundary conditions)
	my $thread_to_grab = $pid - 1;
	$thread_to_grab = $N_threads - 1 if $pid == 0;

	# Synchronize; make sure all the threads have had a chance to create
	# their data
	parallel_sync;

	# This should be in an eval block in case the data pull fails
	eval {
		# Pull in the data:
		my $to_test = retrieve_pdls("data$thread_to_grab");
		$could_get_data[$pid] = 1;

		# Make sure it's what we expected
		$data_is_correct[$pid] = all($to_test == $thread_to_grab)->sclr
			or diag("For thread $pid, expected ${thread_to_grab}s but got $to_test");

		$to_test = retrieve_pdls("bad$thread_to_grab");
		my $isbad = $to_test->isbad;
		$bad_is_correct[$pid] = all($isbad == pdl(0,0,1,0,0))->sclr || diag "got=$isbad\nto_test=$to_test";

		1;
	} or do {
		diag("data pull for pid $pid failed: $@");
		$could_get_data[$pid] = 0;
		$data_is_correct[$pid] = 0;
		$bad_is_correct[$pid] = 0;
	};

} $N_threads;

my @expected = (1) x $N_threads;
is_deeply(\@could_get_data, \@expected,
	'Threads could access data created by sibling threads')
	or diag("expected all 1s, actually got @could_get_data");
is_deeply(\@data_is_correct, \@expected,
	'Data created by sibling threads worked correctly')
	or diag("expected all 1s, actually got @data_is_correct");
is_deeply(\@bad_is_correct, \@expected,
	'Data created by sibling threads badflags survived correctly')
	or diag("expected all 1s, actually got @data_is_correct");

# Make sure the retrieval causes a croak
for (1..$N_threads-1) {
	throws_ok {
		retrieve_pdls("data$_")
	} qr/was created in a thread that has ended or is detached/
	, "Retrieving shared data created by already-terminated thread $_ croaks";
}

done_testing();
