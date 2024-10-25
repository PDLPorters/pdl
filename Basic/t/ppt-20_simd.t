use strict;
use warnings;

BEGIN {
	use Config;
	if (! $Config{'useithreads'}) {
		print("1..0 # Skip: Perl not compiled with 'useithreads'\n");
		exit(0);
	}
}

use Test::More;
use Test::Warn;

#use PDL;
use PDL::Parallel::threads::SIMD qw(parallelize parallel_sync parallel_id);

my $N_threads = 20;
use threads;

# Test basic croaking behavior for function calls that should not work
warning_is {
	my $pid = parallel_id;
} 'Cannot get parallel_id outside of a parallelized block'
		, 'parallel_id not allowed outside of parallelize block';

warning_is {
	parallel_sync;
} 'Cannot call parallel_sync outside of a parallelized block'
		, 'parallel_sync not allowed outside of parallelize block';

# Create five threads that each spawn five threads
my @after_first_block : shared;
my @after_second_block : shared;
my @pids : shared;
my @recursive_simd_allowed : shared;

my @workspace : shared;

parallelize {
	# Get the pid and log the presence
	my $pid = parallel_id;
	$pids[$pid] = 1;

	$workspace[$pid] = $pid + 1;

	# First barrier sync: make sure everybody has updated workspace
	parallel_sync;

	# Make sure that the previosu pid set the correct value before we reached
	# this point.
	my $pid_to_check = $pid - 1;
	$pid_to_check = $N_threads - 1 if $pid_to_check < 0;
	$after_first_block[$pid] = 1;
	$after_first_block[$pid] = 0
		if $workspace[$pid_to_check] != $pid_to_check + 1;

	# Update the workspace value
	$workspace[$pid_to_check] = -$pid;

	# Second barrier sync: make sure we could perform the first check and
	# the assignment
	parallel_sync;

	# Make sure that the newly changed value, from the other thread, is
	# correct here.
	$pid_to_check = $pid + 1;
	$pid_to_check = 0 if $pid_to_check == $N_threads;
	$after_second_block[$pid] = 1;
	$after_second_block[$pid] = 0 if $workspace[$pid] != -$pid_to_check;

	# Check recursive parallelized block
	eval {
		parallelize {
			my $a = 1;
		} 5;
		$recursive_simd_allowed[$pid] = 1;
	} or do {
		$recursive_simd_allowed[$pid] = 0;
	};

} $N_threads;

my @expected = (1) x $N_threads;
is_deeply(\@after_first_block, \@expected, 'First barrier synchronization works');
is_deeply(\@after_second_block, \@expected, 'Second barrier synchronization works');
@expected = (0) x $N_threads;
is_deeply(\@recursive_simd_allowed, \@expected, 'Recursive paralleliztion not (yet) allowed');

done_testing;
