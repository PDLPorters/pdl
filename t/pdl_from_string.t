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
use Test::PDL;

isa_ok pdl("[1,2]"), "PDL", qq{pdl("[1,2]") returns an ndarray};

# Basic Tests #
is_pdl pdl([1,2]), pdl("[1,2]"), qq{pdl(ARRAY REF) equals pdl("ARRAY REF")};
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
is_pdl pdl($test_string), $compare, "properly interprets good PDL input string";
# See what happens when we remove the end commas
$test_string =~ s/\],/]/g;
is_pdl pdl($test_string), $compare, "properly interprets good PDL input string sans ending commas";
is_pdl pdl('[1, 0, 8; 6, 3, 5; 3, 0, 5; 2, 4, 2]'), $compare, "properly handles semicolons";
is_pdl pdl("$compare"), $compare, "properly interprets good PDL output string";
is_pdl pdl(q[1.2e3]), pdl(1.2e3), "Correctly interprets [1.2e3]";
is_pdl pdl(q[1.2e3 4 5.6e-7]), pdl(1.2e3, 4, 5.6e-7), "Correctly interprets [1.2e3 4 5.6e-7]";
is_pdl pdl(q[1.2e3 4 5.e-7]), pdl(1.2e3, 4, 5.e-7), "Correctly interprets [1.2e3 4 5.e-7]";

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
is_pdl pdl("[1 -4]"), pdl([1, -4]), "properly identifies negative numbers with white-space separation";

is_pdl pdl("[1 - 4]"), pdl([1,-4]), "properly affixes negation operator to right operand";

is_pdl pdl("[1 - .4]"), pdl([1,-0.4]), "properly handles decimals";

my $t8 = pdl <<EOPDL;
[
	[1,2,3; 4,-5,6]
	[7 +8, 8 + 9; 10, - .11, 12e3]
]
EOPDL
$compare = pdl([[[1,2,3], [4,-5,6]],[[7,8,8,9],[10,-.11,12e3]]]);
is_pdl $t8, $compare, "properly handles all sorts of stuff!";

is_pdl pdl('[1  + 2 - 5]'), pdl([1,2,-5]), "Another operator check for pdl_from_string";

is_pdl pdl('[1  +2 -5]'), pdl([1, 2, -5]), "Yet another operator check for pdl_from_string";

#######################################
# Semicolons as row seperators - 2 #
#######################################
is_pdl pdl('[1;2;3]'), pdl([[1], [2], [3]]), "column check";
is_pdl pdl('1 2 3;4 5 6'), pdl([[1,2,3],[4,5,6]]), "implicit bracketing check";

##################################
# Implicit bracketing checks - 9 #
##################################

$compare = pdl([1,2,3,4]);
is_pdl pdl(q[1 2 3 4]), $compare, "Double-check implicit bracketing - no brackets";
is_pdl pdl(q[1,2,3,4]), $compare, "Double-check implicit bracketing - no brackets and commas";
is_pdl pdl('[1 2 3 4]'), $compare, "Double-check implicit bracketing - brackets";
is_pdl pdl('[1,2,3,4]'), $compare, "Double-check implicit bracketing - brackets and commas";

is_pdl pdl(q[]), pdl([]), 'Blank strings are interpreted as empty arrays';
is_pdl pdl(q[[]]), pdl([]), 'Empty bracket is correctly interpreted';

# Bad, inf, nan checks #
is_pdl pdl(q[nan inf -inf bad]), pdl(nan(), inf(), -inf(), pdl(0)->setbadif(1));
my $infty = pdl('inf');
my $min_inf = pdl('-inf');
my $bad = pdl('bad');
is_pdl $infty, inf(), "pdl 'inf' works by itself";
is_pdl $min_inf, -inf(), "pdl '-inf' works by itself";
is_pdl $min_inf, -$infty, "pdl '-inf' == -pdl 'inf'";
is_pdl pdl('nan'), nan(), "pdl 'nan' works by itself";
ok $bad->isbad, "pdl 'bad' works by itself"
	or diag "pdl 'bad' gave me $bad";

# Checks for windows strings:
is_pdl pdl(q[1.#INF]), inf(), "pdl '1.#INF' works";
is_pdl pdl(q[-1.#IND]), nan(), "pdl '-1.#IND' works";

# Pi and e checks #
my $expected = pdl(1)->exp;
is_pdl pdl(q[e]), $expected, 'q[e] returns exp(1)';
is_pdl pdl(q[E]), $expected, 'q[E] returns exp(1)';
$expected = pdl(1, exp(1));
is_pdl pdl(q[1 e]), $expected, 'q[1 e] returns [1 exp(1)]';
is_pdl pdl(q[1 E]), $expected, 'q[1 E] returns [1 exp(1)]';
$expected = pdl(exp(1), 1);
is_pdl pdl(q[e 1]), $expected, 'q[e 1] returns [exp(1) 1]';
is_pdl pdl(q[E 1]), $expected, 'q[E 1] returns [exp(1) 1]';
$expected = pdl(1, exp(1), 2);
is_pdl pdl(q[1 e 2]), $expected, 'q[1 e 2] returns [1 exp(1) 2]';
is_pdl pdl(q[1 E 2]), $expected, 'q[1 E 2] returns [1 exp(1) 2]';

# Already checked all the permutations of e, so just make sure that it
# properly substitutes pi
$expected = pdl(1, 4 * atan2(1,1));
is_pdl pdl(q[1 pi]), $expected, 'q[1 pi] returns [1 4*atan2(1,1)]';
is_pdl pdl(q[1 PI]), $expected, 'q[1 PI] returns [1 4*atan2(1,1)]';
is_pdl pdl(q[pi 1]), pdl(4 * atan2(1,1), 1), 'q[pi 1] returns [4*atan2(1,1) 1]';

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
  for my $to_check (q[1 e 2], q[1 +e 2], q[1 e+ 2], q[1e 2], q[1e+ 2],
    q[1+e 2], q[1+e+ 2], q[1 e123 2]
  ) {
    $e_was_run = 0;
    eval {pdl $to_check};
    is($e_was_run, 0, "Does not execute local function e in [$to_check]");
  }
}

###############################
# Useful croaking output - 36 #
###############################

eval{ pdl q[1 l 3] };
like($@, qr/found disallowed character\(s\) 'l'/, 'good error when invalid character is specified');
eval{ pdl q[1 po 3] };
like($@, qr/found disallowed character\(s\) 'po'/, 'good error when invalid characters are specified');

# checks for croaking behavior for consecutive signs like +-2:
eval{ pdl q[1 +-2 3] };
like $@, qr/found a \w+ sign/, 'Good error when consecutive signs';
eval{ pdl q[1 -+2 3] };
like $@, qr/found a \w+ sign/, 'Good error when consecutive signs';

foreach my $special (qw(bad inf pi e)) {
  foreach my $append (qw(2 e l)) {
    for my $str ("$special$append", "$append$special") {
      eval {pdl qq[1 $str 2]};
      my $re = $str eq 'e2' ? qr/exponentiation/ :
        $str eq '2e' ? qr/Incorrect/ :
        qr/larger word/;
      like $@, $re, "Good error for '$str'";
    }
  }
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
    ok $bad_pdl->badflag, 'has badflag enabled';
    ok $bad_pdl->isbad->all, 'all values in PDL are BAD';
    is $bad_pdl->string, $expected_string, "PDL stringifies ok";
  };
}

is pdl(ushort, ['-5'])."", "[65531]", "ushort-typed ['-5'] converted right";
is pdl(ushort, '[-5]')."", "[65531]", "ushort-typed '[-5]' converted right";
is pdl(ushort, [-5])."", "[65531]", "ushort-typed [-5] converted right";

# capture indx() on big-endian - https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1092246
is pdl(indx, 2)."", "2", "indx-typed 2 packed/upd_data-ed right";

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
