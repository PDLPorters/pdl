use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Test::PDL;
use File::Temp qw(tempdir);
use File::Spec::Functions;
use PDL::IO::FastRaw;
# PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $tmpdir = tempdir( CLEANUP=>1 );
my $name = catfile($tmpdir, "tmp0");
my $name_hdr = "$name.hdr";
my $header = catfile($tmpdir, "headerfile" . $$);

sub startdata { pdl [2,3],[4,5],[6,7] }
sub cleanfiles { unlink for grep -f, $name, $name_hdr, $header }

# save an ndarray to disk
my $x = startdata();
writefraw($x,$name);
ok((-f $name and -f ($name_hdr)), "Writing should create a file and header file");

# read it back, and make sure it gives the same ndarray
my $y = readfraw($name);
is_pdl $x, $y, "A ndarray and its saved copy should be about equal";

# Clean things up a bit
undef $x; undef $y;
cleanfiles();

# save an ndarray to disk
$x = startdata();
writefraw($x,"$name.g");
my $x1 = pdl [10,11];
gluefraw($x1,"$name.g");
$y = readfraw("$name.g");
is_pdl $y, pdl([2,3],[4,5],[6,7],[10,11]), "glued data correct";
unlink "$name.g", "$name.g.hdr";
# Clean things up a bit
undef $x; undef $y;

# test the use of a custom header for writing
$x = startdata();
writefraw($x,$name,{Header => $header});
ok -f $header, "writefraw should create the special header file when specified";

# test the use of a custom header for reading
$y = readfraw($name,{Header => $header});
is_pdl $x, $y, "Should be able to read given a specified header";

# some mapfraw tests
SKIP:
{
	writefraw($x = startdata(), $name);
	my $c = eval { mapfraw($name) };
        if ($@) {
           diag("$@");
           if ($@ =~ m/mmap not supported/) {
              skip('no mmap support', 5);
           }
        }

	# compare mapfraw ndarray with original ndarray
	is_pdl $x, $c, "A ndarray and its mapfraw representation should be about equal";

	# modifications should be saved when $c goes out of scope
	$c += 1;
	undef $c;
	$y = readfraw($name);
	is_pdl $x+1,$y, "Modifications to mapfraw should be saved to disk no later than when the ndarray ceases to exist";

	# We're starting a new test, so we'll remove the files we've created so far
	# and clean up the memory, just to be super-safe
	undef $x; undef $y;
	cleanfiles();

	# test creating a pdl via mapfraw
	# First create and modify the ndarray
	$x = mapfraw($name, {Creat => 1, Datatype => float, Dims => [3,2]});
	ok $x->allocated, 'mapfraw-ed shows is allocated';
	$x += xvals $x;
	$x += 0.1 * yvals $x;
	# save the contents
	undef $x; undef $y;
	# Load it back up and see if the values are what we expect
	$y = readfraw($name);
	is_pdl $y, float([[0,1,2],[0.1,1.1,2.1]]),
		"mapfraw should be able to create new ndarrays";

	# test the created type
	ok($y->type == float, 'type should be of the type we specified (float)');

        # mapfraw custom header tests
        # Clean things up a bit
        undef $x; undef $y;
        cleanfiles();

        # test the use of a custom header for writing
        $x = startdata();
        writefraw($x,$name,{Header => $header});
        ok(-f $header, "writefraw should create the special header file when specified");
	$c = eval { mapfraw($name,{Header => $header}) };
        if ($@) {
           diag("$@");
           if ($@ =~ m/mmap not supported/) {
              skip('no mmap support', 1);
           }
        }

	# test custom headers for mapfraw
	is_pdl $x, $c, "mapfraw works with a specified header";
}

done_testing;
