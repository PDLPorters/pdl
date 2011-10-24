#!/usr/bin/perl
#
# This tests the new PDL constructor with a string argument.
# There are two goals from the new functionality: (1) allow
# MATLAB to use familiar syntax to create arrays, and
# (2) to allow cut-n-paste of PDL print output as input
# for scripts and programs
#

use Test::More tests => 30;
use strict;
use warnings;

#############################
# Loading and ISA tests - 2 #
#############################

BEGIN {
   # if we've got this far in the tests then 
   # we can probably assume PDL::LiteF works!
   #
   use_ok( "PDL::LiteF" );
}

isa_ok( pdl("[1,2]"), "PDL", qq{pdl("[1,2]") returns a piddle} );

###################
# Basic Tests - 5 #
###################

ok( all(pdl([1,2])==pdl("[1,2]")), qq{pdl(ARRAY REF) equals pdl("ARRAY REF")});

my $compare = pdl([
	[1, 0, 8],
	[6, 3, 5],
	[3, 0, 5],
	[2, 4, 2]
]);

my $test_string = <<EOPDL;
   [
     [1, 0, 8],
     [6, 3, 5],
     [3, 0, 5],
     [2, 4, 2],
   ]
EOPDL

my $t1 = pdl $test_string;
ok(all(approx($t1, $compare)), "properly interprets good PDL input string");

# See what happens when we remove the end commas
$test_string =~ s/\],/]/g;

my $t2 = pdl $test_string;
ok(all(approx($t2, $compare)), "properly interprets good PDL input string sans ending commas");

my $t3 = pdl '[1, 0, 8; 6, 3, 5; 3, 0, 5; 2, 4, 2]';
ok(all(approx($t3, $compare)), "properly handles semicolons");

my $t4 = pdl "$compare";
ok(all(approx($t4, $compare)), "properly interprets good PDL output string");

###########################
# Signs and operators - 6 #
###########################

# Now some more interesting tests
my $t5 = pdl "[1 - 4]";
$compare = pdl [-3];
ok(all(approx($t5, $compare)), "does not interfere with subtraction in statement");

my $t6 = pdl "[1 -4]";
$compare = pdl [1, -4];
ok(all(approx($t6, $compare)), "properly identifies negative numbers with white-space");

ok(all(approx(pdl("[1 - .4]"), pdl(0.6))), "properly handles decimals");

my $t8 = pdl <<EOPDL;
[
	[1,2,3; 4,-5,6]
	[7 +8, 8 + 9; 10, - .11, 12e3]
]
EOPDL

$compare = pdl([[[1,2,3], [4,-5,6]],[[7,8,8+9],[10,-.11,12e3]]]);
ok(all(approx($t8, $compare)), "properly handles all sorts of stuff!");

$compare = pdl [-2];
my $t9 = pdl '[1  + 2 - 5]';
ok(all(approx($t9, $compare)), "Another operator check for pdl_from_string");

$compare = pdl [1, 2, -5];
my $t10 = pdl '[1  +2 -5]';
ok(all(approx($t10, $compare)), "Yet another operator check for pdl_from_string");

#######################################
# Semicolons as column seperators - 2 #
#######################################

$compare = pdl [[1], [2], [3]];
my $t11 = pdl '[1;2;3]';
ok(all(approx($t11, $compare)), "column check");

$compare = pdl([[1,2,3],[4,5,6]]);
my $t12 = pdl q[1 2 3; 4 5 6];
ok(all(approx($t12, $compare)), "implicit bracketing check");

##################################
# Implicit bracketing checks - 8 #
##################################

$compare = pdl([1,2,3,4]);
my $t13 = pdl q[1 2 3 4];
my $t14 = pdl q[1,2,3,4];
my $t15 = pdl '[1 2 3 4]';
my $t16 = pdl '[1,2,3,4]';

ok(all(approx($t13, $compare)), "Double-check implicit bracketing - no brackets");
ok(all(approx($t14, $compare)), "Double-check implicit bracketing - no brackets and commas");
ok(all(approx($t15, $compare)), "Double-check implicit bracketing - brackets");
ok(all(approx($t16, $compare)), "Double-check implicit bracketing - brackets and commas");

# check dimensions of tests
ok($t13->ndims == 1, "Implicit bracketing gets proper number of dimensions - no brackets, no commas");
ok($t14->ndims == 1, "Implicit bracketing gets proper number of dimensions - no brackets, commas");
ok($t15->ndims == 1, "Implicit bracketing gets proper number of dimensions - brackets, no commas");
ok($t16->ndims == 1, "Implicit bracketing gets proper number of dimensions - brackets and commas");

############################
# Bad, inf, nan checks - 7 #
############################

# First term should be -inf
my $bad_values = pdl q[nan inf -inf bad];
# nan test: nan is never considered equal to itself
ok($bad_values->at(0) != $bad_values->at(0), 'properly handles nan')
	or diag("Zeroeth bad value should be nan but it describes itself as " . $bad_values->at(0));
# inf test: inf + 1 == inf
ok($bad_values->at(1) + 1 == $bad_values->at(1), 'properly handles inf')
	or diag("First bad value should be inf but it describes itself as " . $bad_values->at(1));
# inf test: -inf + 1 == -inf
ok($bad_values->at(2) + 1 == $bad_values->at(2), 'properly handles -inf')
	or diag("Second bad value should be -inf but it describes itself as " . $bad_values->at(2));
# bad test
ok($bad_values->isbad->at(3), 'properly handles bad values')
	or diag("Third bad value should be BAD but it describes itself as " . $bad_values->slice(3));

my $infty = pdl 'inf';
my $nan = pdl 'nan';
my $bad = pdl 'bad';
ok($infty + 1 == $infty, "pdl 'inf' works by itself");
ok($nan != $nan, "pdl 'nan' works by itself");
ok($bad->isbad, "pdl 'bad' works by itself");


# Basic 2D array
# pdl> p $a = pdl q[ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
# pdl> p $a = pdl q[ 1 2 3 ; 4 5 6 ]
# pdl> p $a = pdl '[ [ 1, 2, 3 ], [ 4, 5, 6 ] ]';
# 
# [
#  [1 2 3]
#  [4 5 6]
# ]

# Basic 1D array
# pdl> p $b = pdl [ 1, 2, 3, 4, 5, 6 ]
# pdl> p $b = pdl q[ 1 2 3 4 5 6 ]
# pdl> p $b = pdl q[1,2,3,4,5,6]
# [1 2 3 4 5 6] 

# 1D array with signs
# pdl> p $c = pdl [ 7, -2, +5 ]
# pdl> p $c = pdl q[ 7 -2 +5 ]
# pdl> p $c = pdl q[ 7, -2, +5 ]
# [7 -2 5] 

# 1D array with mixed ops and signs
# pdl> p $d = pdl [ 7 - 2, +5 ]
# pdl> p $d = pdl q[ 7 - 2 +5 ]
# [5 5]

# ...another
# pdl> p $d = pdl [ 7, -2 + 5 ]
# pdl> p $d = pdl q[ 7 -2 + 5 ]
# [7 3]

# 1D array with ops, not signs
# pdl> p $d = pdl [ 7 - 2 + 5 ]
# pdl> p $d = pdl q[ 7 - 2 + 5 ]
# 10

# A [2,3,4] shape piddle
# pdl> p $d = pdl [ [ [0, 1], [4, 0], [0, 3] ],
#                   [ [2, 0], [4, 0], [4, 1] ],
#                   [ [0, 1], [3, 2], [1, 4] ],
#                   [ [1, 2], [2, 2], [2, 1] ] ];
# 
# [
#  [
#   [0 1]
#   [4 0]
#   [0 3]
#  ]
#  [
#   [2 0]
#   [4 0]
#   [4 1]
#  ]
#  [
#   [0 1]
#   [3 2]
#   [1 4]
#  ]
#  [
#   [1 2]
#   [2 2]
#   [2 1]
#  ]
# ]
# 
# ...the same, just different formatting...
#
# [
#  [ [0 1] [4 0] [0 3] ]
#  [ [2 0] [4 0] [4 1] ]
#  [ [0 1] [3 2] [1 4] ]
#  [ [1 2] [2 2] [2 1] ]
# ]

# A 3x3 2D array
# pdl> p pdl [ [1, 2, 3], [2, 1, 0], [2, 2, 1] ];
# pdl> p $e = pdl q[ [ 1 2 3 ] ; [ 2 1 0 ] ; [ 2 2 1 ] ]; 
# pdl> p pdl q[  1 2 3 ; 2 1 0 ; 2 2 1 ]  # this should be the same
#
# [
#  [1 2 3]
#  [2 1 0]
#  [2 2 1]
# ]
