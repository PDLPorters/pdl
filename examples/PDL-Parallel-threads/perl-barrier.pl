use strict;
use warnings;
use threads qw(yield);
use threads::shared qw(cond_wait);

my $N_threads = 4;

# Allocate the shared memory outside the threads
my @data :shared;

# Needed for barrier_sync
my $barrier_count :shared = 0;
my $barrier_state :shared = 'ready';

# Launch the threads, then have this (parent) thread join the fray
threads->create(\&main) for (1..$N_threads-1);
main();

# Reap the remaining threads
for my $thr (threads->list) {
	$thr->join;
}

sub barrier_sync {
	yield until $barrier_state eq 'ready' or $barrier_state eq 'up';

	lock($barrier_count);
	$barrier_state = 'up';
	$barrier_count++;

	if ($barrier_count == $N_threads) {
		$barrier_count--;
		$barrier_state = 'down';
		cond_broadcast($barrier_count);
		yield;
	}
	else {
		cond_wait($barrier_count) while $barrier_state eq 'up';
		$barrier_count--;
		$barrier_state = 'ready' if $barrier_count == 0
	}
}

# This is the code that actually does calculations
sub main {
	my $tid = threads->self->tid;

	$data[$tid] = $tid;
	barrier_sync;

	print "Thread id $tid says the array is ", join(', ', @data), "\n";
	barrier_sync;

	$data[$tid] = 0;
	barrier_sync;

	print "Thread id $tid says the array is now ", join(', ', @data), "\n";
}

