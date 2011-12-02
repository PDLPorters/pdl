
no warnings qw(misc);
use PDL::LiteF;
# PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use strict;
use warnings;

# Load the testing harness and PDL
use Test::More tests => 10;
use PDL;

# Get a temporary directory and file name, which obviously we'll need for testing
# saving and reading of data.
use PDL::Config;
my $tmpdir = $PDL::Config{TEMPDIR};
my $name = $tmpdir . "/tmp0";
my $header = $tmpdir . "/headerfile" . $$;
unlink $name, $name . '.hdr', $header;	# just to be absolutely sure

# A function that tells us if two piddles are approximately the same
sub tapprox {
	my($a,$b) = @_;
	my $c = abs($a-$b);
	return (max($c) < 0.01);
}

# **TEST 1** make sure FastRaw loads
BEGIN { use_ok( 'PDL::IO::FastRaw' ); }

# Set up the working filename and make sure we're working with a clean slate:

# **TEST 2** save a piddle to disk
my $a = pdl [2,3],[4,5],[6,7];
writefraw($a,$name);
ok((-f $name and -f ($name . '.hdr')), "Writing should create a file and header file");

# **TEST 3** read it back, and make sure it gives the same piddle
my $b = readfraw($name);
ok(tapprox($a,$b), "A piddle and it's saved copy should be about equal");

# some mapfraw tests
SKIP:
{
	# should not be run on Windows
	skip( 'no mmap support on win32 (yet?)', 4) if ($^O =~ /win32/i);

	# **TEST 4** compare mapfraw piddle with original piddle	
	my $c = mapfraw($name);
	ok(tapprox($a,$c), "A piddle and it's mapfraw representation should be about equal");
	
	# **TEST 5** modifications should be saved when $c goes out of scope
	$c += 1;
	undef $c;
	$b = readfraw($name);
	ok(tapprox($a+1,$b), "Modifications to mapfraw should be saved to disk no later than when the piddle ceases to exist");
	
	# We're starting a new test, so we'll remove the files we've created so far
	# and clean up the memory, just to be super-safe
	unlink $name, $name . '.hdr';
	undef $a;
	undef $b;
	
	# **TEST 6** test creating a pdl via mapfraw
	# First create and modify the piddle
	$a = mapfraw($name, {Creat => 1, Datatype => &float, Dims => [3,2]});
	$a += xvals $a;
	$a += 0.1 * yvals $a;
	# save the contents
	undef $a;
	# Load it back up and see if the values are what we expect
	$b = readfraw($name);
	ok(tapprox($b, PDL->pdl([[0,1,2],[0.1,1.1,2.1]])),
		"mapfraw should be able to create new piddles");
	
	# **TEST 7** test the created type
	ok($b->type->[0] == (&float)->[0], 'type should be of the type we specified (float)');
}

# Clean things up a bit
unlink $name, $name . '.hdr', $header;
undef $a;
undef $b;

# Test the file header options:

# **TEST 8** test the use of a custom header for writing
$a = pdl [2,3],[4,5],[6,7];
writefraw($a,$name,{Header => $header});
ok(-f $header, "writefraw should create the special header file when specified");

# **TEST 9** test the use of a custom header for reading
$b = readfraw($name,{Header => $header});
ok(tapprox($a,$b), "Should be able to read given a specified header");

# mapfraw custom header tests
SKIP: 
{
	# should not be run on Windows
	skip( 'no mmap support on win32 (yet?)', 1) if ($^O =~ /win32/i);
	
	# **TEST 10** test custom headers for mapfraw
	my $c = mapfraw($name,{Header => $header});
	ok(tapprox($a,$c), "mapfraw should be able to work with a specified header");
}

# Clean things up for exit
unlink $name, $header;
