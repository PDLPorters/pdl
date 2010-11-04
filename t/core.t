# -*-perl-*-
#
# test some PDL core routines
#

use strict;
use Test::More tests => 61;

BEGIN {
    # if we've got this far in the tests then 
    # we can probably assume PDL::LiteF works!
    #
    use_ok( "PDL::LiteF" );
}
$| = 1;

sub tapprox ($$) {
    my ( $a, $b ) = @_;
    my $d = abs( $a - $b );
    print "diff = [$d]\n";
    return $d <= 0.0001;
}

my $a_long = sequence long, 10;
my $a_dbl  = sequence 10;

my $b_long = $a_long->slice('5');
my $b_dbl  = $a_dbl->slice('5');

my $c_long = $a_long->slice('4:7');
my $c_dbl  = $a_dbl->slice('4:7');

# test 'sclr' method
#
is $b_long->sclr, 5, "sclr test of 1-elem pdl (long)";
is $c_long->sclr, 4, "sclr test of 3-elem pdl (long)";

ok tapprox( $b_dbl->sclr, 5 ), "sclr test of 1-elem pdl (dbl)";
ok tapprox( $c_dbl->sclr, 4 ), "sclr test of 3-elem pdl (dbl)";

# switch multielement check on
is( PDL->sclr({Check=>'barf'}), 2, "changed error mode of sclr" );

eval '$c_long->sclr';
like $@, qr/multielement piddle in 'sclr' call/, "sclr failed on multi-element piddle (long)";

eval '$c_dbl->sclr';
like $@, qr/multielement piddle in 'sclr' call/, "sclr failed on multi-element piddle (dbl)";

# test reshape barfing with negative args
#
eval 'my $d_long = $a_long->reshape(0,-3);';
like $@, qr/invalid dim size/, "reshape() failed with negative args (long)";

eval 'my $d_dbl = $a_dbl->reshape(0,-3);';
like $@, qr/invalid dim size/, "reshape() failed with negative args (dbl)";

# test reshape with no args
my ( $a, $b, $c );

$a = ones 3,1,4;
$b = $a->reshape;
ok eq_array( [ $b->dims ], [3,4] ), "reshape()";

# test reshape(-1) and squeeze
$a = ones 3,1,4;
$b = $a->reshape(-1);
$c = $a->squeeze;
ok eq_array( [ $b->dims ], [3,4] ), "reshape(-1)";
ok all( $b == $c ), "squeeze";

$c++; # check dataflow
print "a: $a\nb: $b\nc: $c\n";
ok all( $b == $c ), "dataflow"; # should flow back to b
ok all( $a == 2 ), "dataflow";

# test topdl

isa_ok( PDL->topdl(1),       "PDL", "topdl(1) returns a piddle" );
isa_ok( PDL->topdl([1,2,3]), "PDL", "topdl([1,2,3]) returns a piddle" );

# test $PDL::undefval support in pdl (bug #886263)
#
is $PDL::undefval, 0, "default value of $PDL::undefval is 0";

$a = [ [ 2, undef ], [3, 4 ] ];
$b = pdl( $a );
$c = pdl( [ 2, 0, 3, 4 ] )->reshape(2,2);
ok all( $b == $c ), "undef converted to 0 (dbl)";
ok eq_array( $a, [[2,undef],[3,4]] ), "pdl() has not changed input array";

$b = pdl( long, $a );
$c = pdl( long, [ 2, 0, 3, 4 ] )->reshape(2,2);
ok all( $b == $c ), "undef converted to 0 (long)";

do { 
    local($PDL::undefval) = -999;
    $a = [ [ 2, undef ], [3, 4 ] ];
    $b = pdl( $a );
    $c = pdl( [ 2, -999, 3, 4 ] )->reshape(2,2);
    ok all( $b == $c ), "undef converted to -999 (dbl)";
    
    $b = pdl( long, $a );
    $c = pdl( long, [ 2, -999, 3, 4 ] )->reshape(2,2);
    ok all( $b == $c ), "undef converted to -999 (long)";
} while(0);

##############
# Funky constructor cases

# pdl of a pdl
$a = pdl(pdl(5));
ok all( $a== pdl(5)), "pdl() can piddlify a piddle";

TODO: {
   local $TODO = 'Known_problems bug sf.net #3011879' if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS});

   # pdl of mixed-dim pdls: pad within a dimension
   $a = pdl( zeroes(5), ones(3) );
   ok all($a == pdl([0,0,0,0,0],[1,1,1,0,0])),"Piddlifying two piddles catenates them and pads to length" or diag("a=$a\n");
}
   
# pdl of mixed-dim pdls: pad a whole dimension
$a = pdl( [[9,9],[8,8]], xvals(3)+1 );
ok all($a == pdl([[[9,9],[8,8],[0,0]] , [[1,0],[2,0],[3,0]] ])),"can catenate mixed-dim piddles" or diag("a=$a\n");

TODO: {
   local $TODO = 'Known_problems bug sf.net #3011879' if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS});

   # pdl of mixed-dim pdls: a hairier case
   $c = pdl [1], pdl[2,3,4], pdl[5];
   ok all($c == pdl([[[1,0,0],[0,0,0]],[[2,3,4],[5,0,0]]])),"Can catenate mixed-dim piddles: hairy case" or diag("c=$c\n");;

   # same thing, with undefval set differently
   do {
      local($PDL::undefval) = 99;
      $c = pdl [1], pdl[2,3,4], pdl[5];
      ok all($c == pdl([[[1,99,99],[99,99,99]],[[2,3,4],[5,99,99]]])), "undefval works for padding" or diag("c=$c\n");;
   } while(0);
}
    
# end

# cat problems
eval {cat(1, pdl(1,2,3), {}, 6)};
ok ($@ ne '', 'cat barfs on non-piddle arguments');
like ($@, qr/Arguments 0, 2 and 3 are not piddles/, 'cat correctly identifies non-piddle arguments');
$@ = '';
eval {cat(1, pdl(1,2,3))};
like($@, qr/Argument 0 is not a piddle/, 'cat uses good grammar when discussing non-piddles');
$@ = '';

my $two_dim_array = cat(pdl(1,2), pdl(1,2));
eval {cat(pdl(1,2,3,4,5), $two_dim_array, pdl(1,2,3,4,5), pdl(1,2,3))};
ok ($@ ne '', 'cat barfs on mismatched piddles');
like($@, qr/The dimensions of arguments 1 and 3 do not match/
	, 'cat identifies all piddles with differing dimensions');
like ($@, qr/\(argument 0\)/, 'cat identifies the first actual piddle in the arg list');
$@ = '';
eval {cat(pdl(1,2,3), pdl(1,2))};
like($@, qr/The dimensions of argument 1 do not match/
	, 'cat uses good grammar when discussing piddle dimension mismatches');
$@ = '';
eval {cat(1, pdl(1,2,3), $two_dim_array, 4, {}, pdl(4,5,6), pdl(7))};
ok ($@ ne '', 'cat barfs combined screw-ups');
like($@, qr/Arguments 0, 3 and 4 are not piddles/
	, 'cat properly identifies non-piddles in combined screw-ups');
like($@, qr/arguments 2 and 6 do not match/
	, 'cat properly identifies piddles with mismatched dimensions in combined screw-ups');
like($@, qr/\(argument 1\)/,
	'cat properly identifies the first actual piddle in combined screw-ups');
$@ = '';





#### pdl STRING constructor tests ####

isa_ok( pdl("[1,2]"), "PDL", qq{pdl("[1,2]") returns a piddle} );
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

#diag("test string is: $test_string\n");

my $t1 = pdl $test_string;
ok(all(approx($t1, $compare)), "Unstringify properly interprets good PDL input string");

# See what happens when we remove the end commas
$test_string =~ s/\],/]/g;

my $t2 = pdl $test_string;
ok(all(approx($t2, $compare)), "Unstringify properly interprets good PDL input string sans ending commas");

my $t3 = pdl '[1, 0, 8; 6, 3, 5; 3, 0, 5; 2, 4, 2]';
ok(all(approx($t3, $compare)), "Unstringify properly handles semicolongs");

my $t4 = pdl "$compare";
ok(all(approx($t4, $compare)), "Unstringify properly interprets good PDL output string");

# Now some more interesting tests
my $t5 = pdl "[1 - 4]";
$compare = pdl [-3];
ok(all(approx($t5, $compare)), "Unstringify does not interfere with subtraction in statement");

my $t6 = pdl "[1 -4]";
$compare = pdl [1, -4];
ok(all(approx($t6, $compare)), "Unstringify properly identifies negative numbers with white-space");

ok(all(approx(pdl("[1 - .4]"), pdl(0.6))), "Unstringify properly handles decimals");

my $t8 = pdl <<EOPDL;
[
	[1,2,3; 4,-5,6]
	[7 +8, 8 + 9; 10, - .11, 12e3]
]
EOPDL

$compare = pdl([[[1,2,3], [4,-5,6]],[[7,8,8+9],[10,-.11,12e3]]]);
ok(all(approx($t8, $compare)), "Unstringify properly handles all sorts of stuff!");

$compare = pdl [-2];
my $t9 = pdl '[1  + 2 - 5]';
ok(all(approx($t9, $compare)), "Another operator check for unstringify");

$compare = pdl [1, 2, -5];
my $t10 = pdl '[1  +2 -5]';
ok(all(approx($t10, $compare)), "Yet another operator check for unstringify");

$compare = pdl [[1], [2], [3]];
my $t11 = pdl '[1;2;3]';
ok(all(approx($t11, $compare)), "Unstringify column check");

$compare = pdl([[1,2,3],[4,5,6]]);
my $t12 = pdl q[1 2 3; 4 5 6];
ok(all(approx($t12, $compare)), "Unstringify implicit bracketing check");

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
ok($t13->ndims == 1, "Implicit bracketing gets proper number of dimensions - no brackets");
ok($t14->ndims == 1, "Implicit bracketing gets proper number of dimensions - no brackets and commas");
ok($t15->ndims == 1, "Implicit bracketing gets proper number of dimensions - brackets");
ok($t16->ndims == 1, "Implicit bracketing gets proper number of dimensions - brackets and commas");
