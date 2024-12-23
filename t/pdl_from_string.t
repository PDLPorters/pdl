# This tests the new PDL constructor with a string argument.
# There are three goals from the new functionality: (1) to allow
# MATLAB to use familiar syntax to create arrays, (2) to allow
# cut-n-paste of PDL print output as input for scripts and programs,
# and (3) to allow easy ways to construct nan and inf values in ndarrays.
# (4) to allow complex numbers to be round-tripped in native
# complex (i.e. Math::Complex) format

use strict;
use warnings;
use Test::More;
use Config;
use PDL::LiteF;

isa_ok( pdl("[1,2]"), "PDL", qq{pdl("[1,2]") returns an ndarray} );

# Basic Tests #
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
my $expected = pdl(1.2e3);
my $got = pdl q[1.2e3];
is($got, $expected, "Correctly interprets [1.2e3]");
$expected = pdl(1.2e3, 4, 5.6e-7);
$got = pdl q[1.2e3 4 5.6e-7];
ok(all($got == $expected), "Correctly interprets [1.2e3 4 5.6e-7]");
$expected = pdl(1.2e3, 4, 5.e-7);
$got = pdl q[1.2e3 4 5.e-7];
ok(all($got == $expected), "Correctly interprets [1.2e3 4 5.e-7]");

for (
  '[i 1-i]',
  ['[-i 1 -i]', qr/(-0)?-i 1 (-0)?-i/],
  ['[-i 1 -i 1]', qr/(-0)?-i 1 (-0)?-i 1/],
  ['[-i 1-i 1]', qr/(-0)?-i 1-i 1/],
  '[i 1+i]',
  ['[i 1 +i]', '[i 1 i]'],
  ['[i 2e+2+i]', '[i 200+i]'],
  ['[i 2e2+i]', '[i 200+i]'],
  '[2i 1-2i]',
  '[2i 1+2i]',
  '[0.5-2i 1.5-2i]',
  '[0.5 0.5-2i 2i 1.5 1.5-2i]',
) {
  my ($input, $expected) = ref($_) ? @$_ : ($_, $_);
  my $got = eval { pdl($input).'' };
  is $@, '', "parsed '$input' with no exception";
  $expected = qr/\A\Q$expected\E\z/ if !ref $expected;
  like $got, $expected, "complex number round-tripped '$input'";
}

# Signs and operators #
# Now some more interesting tests
my $t5 = pdl "[1 -4]";
$compare = pdl [1, -4];
ok(all(approx($t5, $compare)), "properly identifies negative numbers with white-space separation");

my $t6 = pdl "[1 - 4]";
$compare = pdl [1,-4];
ok(all(approx($t6, $compare)), "properly affixes negation operator to right operand");

ok(all(approx(pdl("[1 - .4]"), pdl([1,-0.4]))), "properly handles decimals");

my $t8 = pdl <<EOPDL;
[
	[1,2,3; 4,-5,6]
	[7 +8, 8 + 9; 10, - .11, 12e3]
]
EOPDL

$compare = pdl([[[1,2,3], [4,-5,6]],[[7,8,8,9],[10,-.11,12e3]]]);
ok(all(approx($t8, $compare)), "properly handles all sorts of stuff!");

$compare = pdl [1,2,-5];
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
# Implicit bracketing checks - 9 #
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

$expected = pdl [];
$got = pdl q[];
ok(all($got == $expected), 'Blank strings are interpreted as empty arrays');
# This generates an annoying warning, and the ndarray should be Empty anyway
#$expected = pdl [];
$got = pdl q[[]];
ok(all($got == $expected), 'Empty bracket is correctly interpreted');

# Bad, inf, nan checks #
my $bad_values = pdl q[nan inf -inf bad];

ok $bad_values->at(0) != $bad_values->at(0), 'properly handles nan'
        or diag("Zeroeth bad value should be nan but it describes itself as "
        . $bad_values->at(0));
# inf test: inf == inf but inf * 0 != 0
ok((	$bad_values->at(1) == $bad_values->at(1)
		and $bad_values->at(1) * 0.0 != 0.0), 'properly handles inf')
	or diag("First bad value should be inf but it describes itself as " . $bad_values->at(1));
# inf test: -inf == -1 * inf
ok((	$bad_values->at(2) == $bad_values->at(2)
		and $bad_values->at(2) * 0.0 != 0.0), 'properly handles -inf')
	or diag("Second bad value should be -inf but it describes itself as " . $bad_values->at(2));
ok($bad_values->at(2) == -$bad_values->at(1), "negative inf is numerically equal to -inf");
ok($bad_values->isbad->at(3), 'properly handles bad values')
	or diag("Third bad value should be BAD but it describes itself as " . $bad_values->slice(3));

my $infty = inf();
my $min_inf = -inf();
my $nan = nan();

my $bad = pdl 'bad';

ok $infty == $infty && $infty * 0.0 != 0.0, "pdl 'inf' works by itself"
        or diag "pdl 'inf' gave me $infty";
ok $min_inf == $min_inf && $min_inf * 0.0 != 0.0, "pdl '-inf' works by itself"
        or diag "pdl '-inf' gave me $min_inf";
ok($min_inf == -$infty, "pdl '-inf' == -pdl 'inf'");

ok((   $nan != $nan), "pdl 'nan' works by itself")
   or diag("pdl 'nan' gave me $nan");

ok $bad->isbad, "pdl 'bad' works by itself"
	or diag "pdl 'bad' gave me $bad";

# Checks for windows strings:
$infty = pdl q[1.#INF];
$nan = pdl q[-1.#IND];

ok $infty == $infty && $infty * 0 != 0, "pdl '1.#INF' works"
        or diag "pdl '1.#INF' gave me $infty";
ok $nan != $nan, "pdl '-1.#IND' works"
        or diag "pdl '-1.#IND' gave me $nan";

# Pi and e checks #
$expected = pdl(1)->exp;
# using approx() here since PDL only has support for double data
# so there will be differences in the least significant places for
# perls compiled with uselongdouble
#
$got = pdl q[e];
ok(approx($got, $expected, 1e-12), 'q[e] returns exp(1)')
	or diag("Got $got");
# using approx() here since PDL only has support for double data
# so there will be differences in the least significant places for
# perls compiled with uselongdouble
#
$got = pdl q[E];
ok(approx($got, $expected, 1e-12), 'q[E] returns exp(1)')
	or diag("Got $got");
$expected = pdl(1, exp(1));
$got = pdl q[1 e];
ok(all($got == $expected), 'q[1 e] returns [1 exp(1)]')
	or diag("Got $got");
$got = pdl q[1 E];
ok(all($got == $expected), 'q[1 E] returns [1 exp(1)]')
	or diag("Got $got");
$expected = pdl(exp(1), 1);
$got = pdl q[e 1];
ok(all($got == $expected), 'q[e 1] returns [exp(1) 1]')
	or diag("Got $got");
$got = pdl q[E 1];
ok(all($got == $expected), 'q[E 1] returns [exp(1) 1]')
	or diag("Got $got");
$expected = pdl(1, exp(1), 2);
$got = pdl q[1 e 2];
ok(all($got == $expected), 'q[1 e 2] returns [1 exp(1) 2]')
	or diag("Got $got");
$got = pdl q[1 E 2];
ok(all($got == $expected), 'q[1 E 2] returns [1 exp(1) 2]')
	or diag("Got $got");

# Already checked all the permutations of e, so just make sure that it
# properly substitutes pi
$expected = pdl(1, 4 * atan2(1,1));
$got = pdl q[1 pi];
ok(all($got == $expected), 'q[1 pi] returns [1 4*atan2(1,1)]')
	or diag("Got $got");
$got = pdl q[1 PI];
ok(all($got == $expected), 'q[1 PI] returns [1 4*atan2(1,1)]')
	or diag("Got $got");
$expected = pdl(4 * atan2(1,1), 1);
$got = pdl q[pi 1];
ok(all($got == $expected), 'q[pi 1] returns [4*atan2(1,1) 1]')
	or diag("Got $got");

# Security checks #
# Check croaking on arbitrary bare-words:
eval {pdl q[1 foobar 2]};
isnt($@, '', 'croaks on arbitrary string input');
eval {pdl q[$a $b $c]};
isnt($@, '', 'croaks with non-interpolated strings');

# Install a function that knows if it's been executed.
{
	my $e_was_run = 0;
	sub PDL::Core::e { $e_was_run++ }
	sub PDL::Core::e123 { $e_was_run++ }
	my $to_check = q[1 e 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1 +e 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1 e+ 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1e 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1e+ 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1+e 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1+e+ 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e in [$to_check]");
	$e_was_run = 0;
	$to_check = q[1 e123 2];
	eval {pdl $to_check};
	is($e_was_run, 0, "Does not execute local function e123 in [$to_check]");
	$e_was_run = 0;
}

###############################
# Useful croaking output - 36 #
###############################

eval{ pdl q[1 l 3] };
isnt($@, '', 'Croaks when invalid character is specified');
like($@, qr/found disallowed character\(s\) 'l'/, 'Gives meaningful explanation of problem');
eval{ pdl q[1 po 3] };
isnt($@, '', 'Croaks when invalid characters are specified');
like($@, qr/found disallowed character\(s\) 'po'/, 'Gives meaningful explanation of problem');

# checks for croaking behavior for consecutive signs like +-2:
eval{ pdl q[1 +-2 3] };
like($@, qr/found a \w+ sign/, 'Good error when consecutive signs');
eval{ pdl q[1 -+2 3] };
like($@, qr/found a \w+ sign/, 'Good error when consecutive signs');

# 'larger word' croak checks (36)
foreach my $special (qw(bad inf pi)) {
	foreach my $append (qw(2 e l)) {
		eval "pdl q[1 $special$append 2]";
		isnt($@, '', "Croaks when it finds $special$append");
		like($@, qr/larger word/, 'Gives meaningful explanation of problem');
		eval "pdl q[1 $append$special 2]";
		isnt($@, '', "Croaks when it finds $append$special");
		like($@, qr/larger word/, 'Gives meaningful explanation of problem');
	}
}

# e croaks (6)
my $special = 'e';
foreach my $append (qw(2 e l)) {
		eval "pdl q[1 $special$append 2]";
		isnt($@, '', "Croaks when it finds $special$append");
		eval "pdl q[1 $append$special 2]";
		isnt($@, '', "Croaks when it finds $append$special");
}

## Issue information
##
## Name: BAD value parsing breakage
##
## Parsing of BAD values fails to set the correct BAD value when parsing from
## the string "[BAD]".
##
## <http://sourceforge.net/p/pdl/bugs/367/>
## <https://github.com/PDLPorters/pdl/issues/47>

# input string -> expected string
my $cases = {
	q|BAD|         => q|BAD|,
	q|BAD BAD|     => q|[BAD BAD]|,
	q|BAD BAD BAD| => q|[BAD BAD BAD]|,
	q|[BAD]|       => q|[BAD]|,
	q|[ BAD ]|     => q|[BAD]|,
	q|[BAD BAD]|   => q|[BAD BAD]|,
	q|[ BAD BAD ]| => q|[BAD BAD]|,
};

while( my ($case_string, $expected_string) = each %$cases ) {
	my $bad_pdl = pdl( $case_string );
	subtest "Testing case: $case_string" => sub {
		ok( $bad_pdl->badflag, 'has badflag enabled');
		ok( $bad_pdl->isbad->all, 'all values in PDL are BAD');

		is($bad_pdl->string, $expected_string, "PDL stringifies back to input string: @{[ $bad_pdl->string ]}");
	};
}

is pdl(ushort, ['-5'])."", "[65531]", "ushort-typed ['-5'] converted right";
is pdl(ushort, '[-5]')."", "[65531]", "ushort-typed '[-5]' converted right";
is pdl(ushort, [-5])."", "[65531]", "ushort-typed [-5] converted right";

done_testing;

# Basic 2D array
# pdl> p $x = pdl q[ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
# pdl> p $x = pdl q[ 1 2 3 ; 4 5 6 ]
# pdl> p $x = pdl '[ [ 1, 2, 3 ], [ 4, 5, 6 ] ]';
#
# [
#  [1 2 3]
#  [4 5 6]
# ]

# Basic 1D array
# pdl> p $y = pdl [ 1, 2, 3, 4, 5, 6 ]
# pdl> p $y = pdl q[ 1 2 3 4 5 6 ]
# pdl> p $y = pdl q[1,2,3,4,5,6]
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

# A [2,3,4] shape ndarray
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
