# -*-perl-*-
#
# test some PDL core routines
#

use strict;
use Test::More tests => 56;

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
ok all( $b == $c ), "dataflow"; # should flow back to b
ok all( $a == 2 ), "dataflow";

# test topdl

isa_ok( PDL->topdl(1),       "PDL", "topdl(1) returns a piddle" );
isa_ok( PDL->topdl([1,2,3]), "PDL", "topdl([1,2,3]) returns a piddle" );
isa_ok( PDL->topdl(1,2,3),   "PDL", "topdl(1,2,3) returns a piddle" );
$a=PDL->topdl(1,2,3);
ok (($a->nelem == 3  and  all($a == pdl(1,2,3))), "topdl(1,2,3) returns a 3-piddle containing (1,2,3)");


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

# pdl of mixed-dim pdls: a hairier case
$c = pdl [1], pdl[2,3,4], pdl[5];
ok all($c == pdl([[[1,0,0],[0,0,0]],[[2,3,4],[5,0,0]]])),"Can catenate mixed-dim piddles: hairy case" or diag("c=$c\n");

# same thing, with undefval set differently
do {
    local($PDL::undefval) = 99;
    $c = pdl [1], pdl[2,3,4], pdl[5];
    ok all($c == pdl([[[1,99,99],[99,99,99]],[[2,3,4],[5,99,99]]])), "undefval works for padding" or diag("c=$c\n");;
} while(0);

# empty pdl cases
eval {$a = zeroes(2,0,1);};
ok(!$@,"zeroes accepts empty PDL specification");

eval { $b = pdl($a,sequence(2,0,1)); };
ok((!$@ and all(pdl($b->dims) == pdl(2,0,1,2))), "catenating two empties gives an empty");

eval { $b = pdl($a,sequence(2,1,1)); };
ok((!$@ and all(pdl($b->dims) == pdl(2,1,1,2))), "catenating an empty and a nonempty treats the empty as a filler");

eval { $b = pdl($a,5) };
ok((!$@ and all(pdl($b->dims)==pdl(2,1,1,2))), "catenating an empty and a scalar on the right works");
ok( all($b==pdl([[[0,0]]],[[[5,0]]])), "catenating an empty and a scalar on the right gives the right answer");

eval { $b = pdl(5,$a) };
ok((!$@ and all(pdl($b->dims)==pdl(2,1,1,2))), "catenating an empty and a scalar on the left works");
ok( all($b==pdl([[[5,0]]],[[[0,0]]])), "catenating an empty and a scalar on the left gives the right answer");
    
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

eval {$a = cat(pdl(1),pdl(2,3));};
ok(!$@, 'cat(pdl(1),pdl(2,3)) succeeds');
ok( ($a->ndims==2 and $a->dim(0)==2 and $a->dim(1)==2), 'weird cat case has the right shape');
ok( all( $a == pdl([1,1],[2,3]) ), "cat does the right thing with catting a 0-pdl and 2-pdl together");
$@='';

# new_or_inplace
$a = sequence(byte,5);


$b = $a->new_or_inplace;
ok( all($b==$a) && ($b->get_datatype ==  $a->get_datatype), "new_or_inplace with no pref returns something like the orig.");

$b++;
ok(all($b!=$a),"new_or_inplace with no inplace flag returns something disconnected from the orig.");

$b = $a->new_or_inplace("float,long");
ok($b->type eq 'float',"new_or_inplace returns the first type in case of no match");

$b = $a->inplace->new_or_inplace;
$b++;
ok(all($b==$a),"new_or_inplace returns the original thing if inplace is set");
ok(!($b->is_inplace),"new_or_inplace clears the inplace flag");

