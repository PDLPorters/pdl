#!/usr/bin/perl
#
# Last saved: Sat 24 Apr 2010 10:40:53 AM

use strict;
use warnings;
use PDL;
use Test::More;

# Here's the function that is being tested
sub PDL::Core::new_pdl_from_string {
	my ($new, $value, $this, $type) = @_;
	
	# some basic checks that can/should be run on everything.
	# Make sure that each closing bracket followed by an opening bracket
	# has a comma in between them:
	$value =~ s/\]\s*\[/],[/g;
	
	# Semicolons require special handling:
	if ($value =~ /;/) {
		$value =~ s/(\[[^\]]+;[^\]]+\])/[$1]/g;
		$value =~ s/;/],[/g;
	}

	# Append zeroes after ending decimal points
	$value =~ s/(\d)\.\s+/$1.0 /g;
	# Insert zeroes in front of starting decimal points
	$value =~ s/([^\d])\./${1}0./g;
	
	# make unambiguous addition/subtraction (white-space on both sides
	# of operator) by removing white-space from both sides
	$value =~ s/(\d)\s+([+-])\s+(?=[\d])/$1$2/g;

	# Replace white-space separators with commas:
	$value =~ s/([.\d])\s+(?=[+\-\d])/$1,/g;
	

	# Let's see if the darn thing compiles as normal Perl code, in which
	# case we just assume the result is a stringified bunch of nested
	# arrays:
	my $val = eval $value;
	if (ref $val eq 'ARRAY') {
		return PDL::Core::pdl_avref($val,$this,$type);
	}
	else {
		barf "PDL::Core::new_pdl_from_string: error happened!\n";
	}
}

my $compare = pdl([
	[1, 0, 8],
	[6, 3, 5],
	[3, 0, 5],
	[2, 4, 2]
]);
#print "Compare is $compare\n";

my $test_string = <<EOPDL;
   [
     [1, 0, 8],
     [6, 3, 5],
     [3, 0, 5],
     [2, 4, 2],
   ]
EOPDL

#diag("test string is: $test_string\n");

my $t1 = pdl $test_string;
ok(all(approx($t1, $compare)), "Properly interpret good PDL input string");
#print "t1 is $t1\n";

# See what happens when we remove the end commas
$test_string =~ s/\],/]/g;

my $t2 = pdl $test_string;
#print "t2 is $t2\n";
ok(all(approx($t2, $compare)), "Properly interpret good PDL input string sans ending commas");

my $t3 = pdl '[1, 0, 8; 6, 3, 5; 3, 0, 5; 2, 4, 2]';
ok(all(approx($t3, $compare)), "Properly handle semicolongs");

my $t4 = pdl "$compare";
ok(all(approx($t4, $compare)), "Properly interpret good PDL output string");

# Now some more interesting tests
my $t5 = pdl "[1 - 4]";
$compare = pdl [-3];
ok(all(approx($t5, $compare)), "Does not interfere with subtraction in statement");

my $t6 = pdl "[1 -4]";
$compare = pdl [1, -4];
ok(all(approx($t6, $compare)), "Properly identifies negative numbers with white-space");

ok(all(approx(pdl("[1 - .4]"), pdl(0.6))), "Properly handles decimals");

my $t8 = pdl <<EOPDL;
[
	[1,2,3; 4,-5,6]
	[7 +8, 8 + 9; 10, - .11, 12e3]
]
EOPDL

$compare = pdl([[[1,2,3], [4,-5,6]],[[7,8,8+9],[10,-.11,12e3]]]);
ok(all(approx($t8, $compare)), "Properly handles all sorts of stuff!");

$compare = pdl [-2];
my $t9 = pdl '[1  + 2 - 5]';
ok(all(approx($t9, $compare)), "Another operator check");

$compare = pdl [1, 2, -5];
my $t10 = pdl '[1  +2 -5]';
ok(all(approx($t10, $compare)), "Yet another operator check");



done_testing();
