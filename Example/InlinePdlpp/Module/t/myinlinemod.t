# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
use PDL::MyInlineMod;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

use PDL::LiteF;
my $a = zeroes 10;
my $b = $a->myinc;
print "$b\n";

print 'not ' unless all $b == 1;
print "ok 2\n";

my $c = $a->plus2;
print "$c\n";
print 'not ' unless all $c == 2;
print "ok 3\n";

