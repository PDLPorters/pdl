use strict;
use warnings;

BEGIN {
	use Config;
	if (! $Config{'useithreads'}) {
		print("1..0 # Skip: Perl not compiled with 'useithreads'\n");
		exit(0);
	}
	if (defined $Devel::Cover::VERSION) {
		print("1..0 # Skip: Devel::Cover no like ithreads\n");
		exit(0);
	}
}

use threads;
use PDL::LiteF;
use PDL::Parallel::threads qw(retrieve_pdls free_pdls);
use PDL::IO::FastRaw;
use File::Temp qw(tempdir);
use File::Spec::Functions;

use Test::More;
use Test::PDL;

my $tmpdir = tempdir( CLEANUP=>1 );
my $name = catfile($tmpdir, "foo.dat");

my $N_threads = 10;
mapfraw($name, {Creat => 1, Dims => [$N_threads], Datatype => double})
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
is_pdl $expected, $workspace, 'Sharing memory mapped ndarrays works';

free_pdls 'workspace'; # so Windows can remove the file

done_testing;
