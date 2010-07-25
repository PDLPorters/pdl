# flexraw's read/write tests, copied from fastraw's tests.
# There are still many tests to write; see the notes at the bottom
# of this document.

use PDL::LiteF;
# PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use strict;
use warnings;

# Load the testing harness and PDL
use Test::More tests => 9;
use PDL;

# Get a temporary directory and file name, which obviously we'll need for testing
# saving and reading of data.
use PDL::Config;
my $tmpdir = $PDL::Config{TEMPDIR};
my $name = $tmpdir . "/tmp0";
unlink $name, $name . '.hdr';	# just to be absolutely sure

# **TEST 1** make sure FastRaw loads
BEGIN { use_ok( 'PDL::IO::FlexRaw' ); }

# Set up the working filename and make sure we're working with a clean slate:

# **TEST 2** save a piddle to disk
my $a = pdl [2,3],[4,5],[6,7];
my $header = writeflex($name, $a);
ok((-f $name), "writeflex should create a file");

# **TEST 3** save a header to disk
writeflexhdr($name, $header);
ok(-f "$name.hdr", "writeflexhdr should create a header file");

# **TEST 4** read it back, and make sure it gives the same piddle
my $b = readflex($name);
ok(all(approx($a,$b)), "A piddle and it's saved copy should be about equal");

# some mapflex tests
SKIP:
{
   # skip these tests is SKIP_KNOWN_PROBLEMS is set
   skip( 'Known_problems sf.net bug #3031068', 5) if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS});
   # should not be run on Windows
   skip( 'no mmap support on win32 (yet?)', 5) if ($^O =~ /win32/i);

   # **TEST 5** compare mapfraw piddle with original piddle	
   my $c = mapflex($name);
   ok(all(approx($a,$c)), "A piddle and it's mapflex representation should be about equal");

   # **TEST 6** modifications should be saved when $c goes out of scope
   # THIS TEST FAILS.
   # This failure is recorded in sf.net bug 3031068.
   # Presently, making $c go out of scope does not free the memory
   # mapping associated with mapflex, so this modification is never
   # saved to the file (or at least it's not saved immediately).
   $c += 1;
   undef $c;
   $b = readflex($name);
   ok(all(approx($a+1,$b)), "Modifications to mapfraw should be saved to disk no later than when the piddle ceases to exist");

   # We're starting a new test, so we'll remove the files we've created so far
   # and clean up the memory, just to be super-safe
   unlink $name, $name . '.hdr';
   undef $a;
   undef $b;

   # **TEST 7** test creating a pdl via mapfraw
   # First create and modify the piddle
   $header = [{Dims => [3,2], Type => 'double'}];
   # Fix this specification.
   $a = mapflex($name, $header, {Creat => 1});
   ok(defined($a), 'mapflex create piddle');
   skip('no mapflex piddle to check', 2);
   $a += xvals $a;
   $a += 0.1 * yvals $a;
   # save the contents
   undef $a;
   # Load it back up and see if the values are what we expect
   $b = readflex($name);
   # **TEST 8**
   ok(all(approx($b, PDL->pdl([[0,1,2],[0.1,1.1,2.1]]))),
      "mapfraw should be able to create new piddles");

   # **TEST 9** test the created type
   ok($b->type->[0] == (&float)->[0], 'type should be of the type we specified (float)');
}

# Clean things up a bit
unlink $name, $name . '.hdr';
undef $a;
undef $b;

# Test the file header options:

# Tests to write still:
# Test using file handles instead of file names
# test read_flexhdr
# test gzip stuff
