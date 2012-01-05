#!/usr/bin/perl
#
# Test for bug in the pdl constructor for mixed arguments.
# Separate from core.t because the problem crashes perl
# and I'd like to keep the granularity of the core.t tests
#
use Test::More tests => 80;
use PDL::LiteF;
use PDL::Config;

my $scalar = 1;
my $pdl_e = pdl([]);
my $pdl_s = pdl(2);
my $pdl_v = pdl(3,4);
my $pdl_vec2 = pdl([9,10]);
my $pdl_m = pdl([5,6],[7,8]);
my $pdl_row = pdl([[10,11]]);
my $pdl_col = pdl([[12],[13]]);


##############################
# Test the basics (21 tests)
isa_ok($pdl_s, 'PDL');

is $pdl_s->ndims(), 0, "scalar goes to scalar PDL";
is $pdl_s, 2, "PDL gets assigned scalar value";

is $pdl_v->ndims(), 1, "vector dims";
is $pdl_v->dim(0), 2, "vector size is 2";
is !!($pdl_v->at(0)==3 && $pdl_v->at(1)==4), 1, "vector contents";

is $pdl_vec2->ndims(), 1, "vector2 dims";
is $pdl_vec2->dim(0),2, "vector2 size is 2";
is !!($pdl_vec2->at(0)==9 && $pdl_vec2->at(1)==10), 1, "vector2 contents";

is $pdl_m->ndims(), 2, "matrix dims";
is $pdl_m->dim(0), 2, "matrix is 2 wide";
is $pdl_m->dim(1), 2, "matrix is 2 high";
is !!($pdl_m->at(0,0)==5 && $pdl_m->at(1,0)==6 && $pdl_m->at(0,1)==7 && $pdl_m->at(1,1)==8), 1, "matrix contents";

is $pdl_row->ndims(), 2, "row dims";
is $pdl_row->dim(0), 2, "row is 2 wide";
is $pdl_row->dim(1), 1, "row is 1 tall";
is !!($pdl_row->at(0,0)==10 && $pdl_row->at(1,0)==11), 1, "row contents";

is $pdl_col->ndims(), 2, "col dims";
is $pdl_col->dim(0), 1, "col is 1 wide";
is $pdl_col->dim(1), 2, "col is 2 tall";
is !!($pdl_col->at(0,0)==12 && $pdl_col->at(0,1)==13), 1, "col contents";

##############################
# Test more complex array-ingestion case (6 tests) with padding
my @a = (1,[2,3],[[4,5],[6,7]]);
my $pdl_a = pdl(@a);
my @testvals = ( [ [0,0,0], 1 ],
		 [ [1,0,0], 0 ],
		 [ [0,1,0], 0 ],
		 [ [1,1,0], 0 ],
		 [ [0,0,1], 2 ],
		 [ [1,0,1], 0 ],
		 [ [0,1,1], 3 ],
		 [ [1,1,1], 0 ],
		 [ [0,0,2], 4 ],
		 [ [1,0,2], 5 ],
		 [ [0,1,2], 6 ],
		 [ [1,1,2], 7 ]
    );

is $pdl_a->ndims(), 3, 'complex array case dims';
is $pdl_a->dim(0), 2, 'complex dim 0';
is $pdl_a->dim(1), 2, 'complex dim 1';
is $pdl_a->dim(2), 3, 'complex dim 2';

my $test_ok = 1;
for my $i(0..$#testvals) {
    $test_ok *= $pdl_a->at( @{$testvals[$i]->[0]} ) == $testvals[$i]->[1];
}
is $test_ok, 1, "contents of complex array-ingestion case";

{
    local $PDL::undefval = 99;
    $pdl_a = pdl(@a);
    $test_ok = 1;
    for my $i(0..$#testvals) {
	$test_ok *= $pdl_a->at( @{$testvals[$i]->[0]} ) == ($testvals[$i]->[1] || 99);
    }
    is $test_ok, 1, "complex array-ingestion with variant padding";
}

##############################
# Test some basic PDL-as-PDL cases

## Ingest a scalar PDL
my $p = pdl($pdl_s);
isa_ok($p, 'PDL');
is $p->ndims(), 0, "scalar PDL goes to scalar PDL";
is $p, $pdl_s, "pdl(pdl(2)) same as pdl(2)";

## Ingest five scalar PDLs -- should make a 1-D array
$p = pdl($pdl_s, $pdl_s, $pdl_s, $pdl_s, $pdl_s);
isa_ok($p, 'PDL');
is $p->ndims(), 1, "two scalar PDLs -> a vector";
is $p->dim(0), 5, "5-vector";
is $p->at(0), $pdl_s, 'vector element 0 ok';
is $p->at(1), $pdl_s, 'vector element 1 ok';
is $p->at(2), $pdl_s, 'vector element 2 ok';
is $p->at(3), $pdl_s, 'vector element 3 ok';
is $p->at(4), $pdl_s, 'vector element 4 ok';

## Ingest a vector PDL and a scalar PDL - should make a 2-D array
$p = pdl($pdl_v, $pdl_s);
isa_ok($p, 'PDL');
is $p->ndims(), 2, 'pdl($pdl_v, $pdl_s) -> 2x2 matrix';
is $p->dim(0), 2, '2 wide';
is $p->dim(1), 2, '2 high';
is $p->at(0,0), $pdl_v->at(0), "vector element 0 got copied OK";
is $p->at(1,0), $pdl_v->at(1), "vector element 1 got copied OK";
is $p->at(0,1), $pdl_s, "scalar copied OK";
is $p->at(1,1), $PDL::undefval, "scalar got padded OK";

## Ingest a scalar PDL and a vector PDL - should make a 2-D array
$p = pdl($pdl_s, $pdl_v);
isa_ok($p, 'PDL');
is $p->ndims(), 2, 'pdl($pdl_s, $pdl_v) -> 2x2 matrix';
is $p->dim(0), 2, '2 wide';
is $p->dim(1), 2, '2 high';
is $p->at(0,0), $pdl_s, "scalar copied OK";
is $p->at(1,0), $PDL::undefval, "scalar got padded OK";
is $p->at(0,1), $pdl_v->at(0), "vector element 0 got copied OK";
is $p->at(1,1), $pdl_v->at(1), "vector element 1 got copied OK";

## A more complicated case 
$p = pdl($pdl_s, 5, $pdl_v, $pdl_m, [$pdl_v, $pdl_v]);
isa_ok($p,'PDL');
is $p->ndims(), 3, 'complicated case -> 3-d PDL';
is $p->dim(0), 2, 'complicated case -> dim 0 is 2';
is $p->dim(1), 2, 'complicated case -> dim 1 is 2';
is $p->dim(2), 5, 'complicated case -> dim 1 is 5';
@testvals = ([ [0,0,0], 2 ],   [ [1,0,0], 0 ],   [ [0,1,0], 0 ],  [ [1,1,0], 0 ], 
	     [ [0,0,1], 5 ],   [ [1,0,1], 0 ],   [ [0,1,1], 0 ],  [ [1,1,1], 0 ],
	     [ [0,0,2], 3 ],   [ [1,0,2], 0 ],   [ [0,1,2], 4 ],  [ [1,1,2], 0 ],
	     [ [0,0,3], 5 ],   [ [1,0,3], 6 ],   [ [0,1,3], 7 ],  [ [1,1,3], 8 ],
	     [ [0,0,4], 3 ],   [ [1,0,4], 4 ],   [ [0,1,4], 3 ],  [ [1,1,4], 4 ]
    );
$test_ok = 1;
for my $i(0..$#testvals) {
    $test_ok *= $p->at(@{$testvals[$i]->[0]}) == $testvals[$i]->[1];
}
is $test_ok, 1, "contents of complicated case";

##############################
# test empty PDLs.
$p = pdl($pdl_e);
is $p->nelem, 0, "piddlifying an empty piddle yields 0 elements";

$p = pdl($pdl_e, $pdl_e);
is $p->ndims, 2, "piddlifying two 0-PDLs makes a 2D-PDL";
is $p->dim(0),0, "piddlifying two empty piddles makes a 0x2-PDL";
is $p->dim(1),2, "piddlifying two empty piddles makes a 0x2-PDL";
eval { $p->at(0,0) };
ok( $@ =~ m/^Position out of range/ , "can't index an empty PDL with at" );

$p = pdl(pdl([4]),5);
is $p->ndims, 2,  "catenating a 1-PDL and a scalar yields a 2D PDL";
is $p->dim(0), 1, "catenating a 1-PDL and a scalar yields a 1x2-PDL";
is $p->dim(1), 2, "catenating a 1-PDL and a scalar yields a 1x2-PDL";
is $p->at(0,0), 4, "catenating a 1-PDL and a scalar does the Right Thing";
is $p->at(0,1), 5, "catenating a 1-PDL and a scalar does the Right Thing, redux";

$p = pdl($pdl_e, 5);
is $p->ndims, 2,  "catenating an empty and a scalar yields a 2D PDL";
is $p->dim(0), 1, "catenating an empty and a scalar yields a 1x2-PDL";
is $p->dim(1), 2, "catenating an empty and a scalar yields a 1x2-PDL";
is $p->at(0,0), $PDL::undefval, "padding OK for empty & scalar case";
is $p->at(0,1), 5, "scalar OK for empty & scalar";


$p = pdl(5, $pdl_e);
is $p->ndims, 2,  "catenating a scalar and an empty yields a 2D PDL";
is $p->dim(0), 1, "catenating a scalar and an empty yields a 1x2-PDL";
is $p->dim(1), 2, "catenating a scalar and an empty yields a 1x2-PDL";
is $p->at(0,0), 5, "scalar OK for scalar & empty";
is $p->at(0,1), $PDL::undefval, "padding OK for scalar & empty";






# This is from sf.net bug #3011879
my @c;
$c[0][0]=pdl(0,4,2,1);
$c[1][0]=pdl(0,0,1,1);
$c[2][0]=pdl(0,0,0,1);
$c[0][1]=pdl(0,0,3,1);
$c[1][1]=pdl(0,0,2,1);
$c[2][1]=pdl(5,1,1,1);
my $d = pdl(@c);




