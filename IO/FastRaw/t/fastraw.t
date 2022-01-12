use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use File::Temp qw(tempdir);
use File::Spec::Functions;
use PDL::IO::FastRaw;
# PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $tmpdir = tempdir( CLEANUP=>1 );
my $name = catfile($tmpdir, "tmp0");
my $header = catfile($tmpdir, "headerfile" . $$);

# A function that tells us if two ndarrays are approximately the same
sub tapprox {
	my($x,$y) = @_;
	my $c = abs($x-$y);
	return (max($c) < 0.01);
}

# Set up the working filename and make sure we're working with a clean slate:

# **TEST 2** save an ndarray to disk
my $x = pdl [2,3],[4,5],[6,7];
writefraw($x,$name);
ok((-f $name and -f ($name . '.hdr')), "Writing should create a file and header file");

# **TEST 3** read it back, and make sure it gives the same ndarray
my $y = readfraw($name);
ok(tapprox($x,$y), "A ndarray and its saved copy should be about equal");

# some mapfraw tests
SKIP:
{
	my $c = eval { mapfraw($name) };
        if ($@) {
           diag("$@");
           if ($@ =~ m/mmap not supported/) {
              skip('no mmap support', 4);
           }
        }

	# **TEST 4** compare mapfraw ndarray with original ndarray	
	ok(tapprox($x,$c), "A ndarray and its mapfraw representation should be about equal");
	
	# **TEST 5** modifications should be saved when $c goes out of scope
	$c += 1;
	undef $c;
	$y = readfraw($name);
	ok(tapprox($x+1,$y), "Modifications to mapfraw should be saved to disk no later than when the ndarray ceases to exist");
	
	# We're starting a new test, so we'll remove the files we've created so far
	# and clean up the memory, just to be super-safe
	unlink $name, $name . '.hdr';
	undef $x;
	undef $y;
	
	# **TEST 6** test creating a pdl via mapfraw
	# First create and modify the ndarray
	$x = mapfraw($name, {Creat => 1, Datatype => &float, Dims => [3,2]});
	$x += xvals $x;
	$x += 0.1 * yvals $x;
	# save the contents
	undef $x;
	# Load it back up and see if the values are what we expect
	$y = readfraw($name);
	ok(tapprox($y, PDL->pdl([[0,1,2],[0.1,1.1,2.1]])),
		"mapfraw should be able to create new ndarrays");
	
	# **TEST 7** test the created type
	ok($y->type->[0] == (&float)->[0], 'type should be of the type we specified (float)');
}

# Clean things up a bit
unlink $name, $name . '.hdr', $header;
undef $x;
undef $y;

# Test the file header options:

# **TEST 8** test the use of a custom header for writing
$x = pdl [2,3],[4,5],[6,7];
writefraw($x,$name,{Header => $header});
ok(-f $header, "writefraw should create the special header file when specified");

# **TEST 9** test the use of a custom header for reading
$y = readfraw($name,{Header => $header});
ok(tapprox($x,$y), "Should be able to read given a specified header");

# mapfraw custom header tests
SKIP: 
{
	my $c = eval { mapfraw($name,{Header => $header}) };
        if ($@) {
           diag("$@");
           if ($@ =~ m/mmap not supported/) {
              skip('no mmap support', 1);
           }
        }

	# **TEST 10** test custom headers for mapfraw
	ok(tapprox($x,$c), "mapfraw should be able to work with a specified header");
}

# Clean things up for exit
unlink $name, $header;

done_testing;
