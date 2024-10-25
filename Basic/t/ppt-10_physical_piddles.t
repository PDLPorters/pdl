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
use threads::shared;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls);

# This is a somewhat complicated test script. The goals are to test the
# following:
# 1) Can we share data for any data type?
# 2) Does each thread think it succeeded at setting the data?
# 3) Does the end result confirm that each thread changed the data?
# 4) Are we prevented from sharing slices?
#
# Here we allocate shared work space for each PDL data type. We then create
# a collection of threads and have each thread modify the contents of one
# part of the shared memory.
#
# While there, each thread does a number of things. It sets a value in the
# shared memory, it confirms that the now-set value is correct, and it
# builds the hash of expected values from such checks. That last part need
# not be done in the threads explicitly, but it makes it easier to write. :-)
#
# After all the threads return, we check that all the values agree with what
# we expect, which is fairly easy (though not entirely trivial) to construct
# by hand. I encorporate square-roots into the calculations to ensure good
# bit coverage of the tests, at least for the floating point numbers.
#
# The last step simply confirms that sharing slices croaks, a pretty easy
# pair of tests.

# Allocate workspace with one extra slot (to verify zeroeth element troubles)
my $N_threads = 20;
my %workspaces = (
	c => sequence(byte, $N_threads, 2)->share_as('workspace_c'),
	s => sequence(short, $N_threads, 2)->share_as('workspace_s'),
	n => sequence(ushort, $N_threads, 2)->share_as('workspace_n'),
	l => sequence(long, $N_threads, 2)->share_as('workspace_l'),
	q => sequence(longlong, $N_threads, 2)->share_as('workspace_q'),
	f => sequence(float, $N_threads, 2)->share_as('workspace_f'),
	d => sequence($N_threads, 2)->share_as('workspace_d'),
);

# Remove longlong if Perl doesn't like longlong types
eval {
	pack('q*', 10);
} or do {
	delete $workspaces{q};
};

###############################################
# Spawn a bunch of threads that work together #
###############################################

use PDL::NiceSlice;
my @success : shared;
my @expected : shared;
threads->create(sub {
	my $tid = shift;

	my (%expected_hash, %success_hash, %bits_hash);
	for my $type_letter (keys %workspaces) {
		my $workspace = retrieve_pdls("workspace_$type_letter");

		# Build this up one thread at a time
		$expected_hash{$type_letter} = 1;

		# Have this thread touch one of the values, and have it double-check
		# that the value is correctly set
		my $tid_plus_1 = double($tid + 1);
		my $five       = double(5);
		$workspace($tid) .= pdl($workspace->type, $tid_plus_1->sqrt + $five->sqrt);
		my $to_test = zeros($workspace->type, 1);
		$to_test(0) .= pdl($workspace->type, $tid_plus_1->sqrt + $five->sqrt);
		$success_hash{$type_letter}
			= ($workspace->at($tid,0) == $to_test->at(0));
	}

	# Make sure the results for each type have a space in shared memory
	$expected[$tid] = shared_clone(\%expected_hash);
	$success[$tid] = shared_clone(\%success_hash);

}, $_) for 0..$N_threads-1;

# Reap the threads
for my $thr (threads->list) {
	$thr->join;
}

########################
# Now test the results #
########################

# Do all the threads think that they were successful at setting their value?
is_deeply(\@success, \@expected, 'All threads changed their local values');
# Do the results of all but the zeroeth element agree with what we expect?

for my $type_letter (keys %workspaces) {
	my $workspace = $workspaces{$type_letter};
	my $type = $workspace->type;
	# Allocate the expected results with the proper type
	my $expected = zeroes($type, $N_threads, 2);
	# Perform the arithmetic using double precision (on the right side of
	# this asignment) before down-casting to the workspace's type
	$expected .= (zeroes($N_threads, 2)->xvals + 1)->sqrt + pdl(5)->sqrt;
	# Perform an exact comparison. The operations may have high bit coverage,
	# but they should also be free from bit noise, I hope.
	ok(all($workspace == $expected), "Sharing $type ndarrays works")
		or diag("Got workspace of $workspace; expected $expected");
}

######################################################
# Test croaking behavior for slices of various kinds #
######################################################

# Test what happens when we try to share a slice
my $slice = $workspaces{d}->(2:-3);
throws_ok {
	$slice->share_as('slice');
} qr/share_pdls: Could not share an ndarray under.*because the ndarray does not have any allocated memory/
, 'Sharing a slice croaks';

my $rotation = $workspaces{d}->rotate(5);
throws_ok {
	$rotation->share_as('rotation')
} qr/share_pdls: Could not share an ndarray under.*because the ndarray does not have any allocated memory/
, 'Sharing a rotation (slice) croaks';

done_testing();
