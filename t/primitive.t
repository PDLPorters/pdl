use strict;
use Test::More;
use PDL::LiteF;
use PDL::Types;

sub tapprox {
    my($x,$y) = @_;
    $_ = pdl($_) for $x, $y;
    if(join(',',$x->dims) ne join(',',$y->dims)) {
       diag "APPROX: $x $y\n";
       diag "UNEQDIM\n";
       return 0;
    }
    my $d = max( abs($x-$y) );
    if($d >= 0.01) {
       diag "APPROX: $x $y\n";
       diag "# APPROXFAIL: $x $y\n";
    }
    $d < 0.01;
}

my $x = PDL->pdl([[5,4,3],[2,3,1.5]]);

ok(tapprox($x->average(), PDL->pdl([4, 2.16666])), "average");
ok(tapprox($x->sumover(), PDL->pdl([12, 6.5])), "sumover");
ok(tapprox($x->prodover(), PDL->pdl([60, 9])), "prodover");

my $y = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
# diag "Y: $y\n";
my $c = ($y->xvals) + 10;
# diag "C: $c\n";

# diag "YW: ", $y->where, "\n";
ok(tapprox($y->where($y>4), PDL->pdl(5,6)), "where with >");
ok(tapprox($y->which, PDL->pdl(0,1,2,7,8,10,11)), "which");

# diag "Y, ",$y->which();
# diag "C: $c\n";
# diag "\nCI, ", $c->index($y->which());
# diag "D\n";

ok(tapprox($c->where($y), PDL->pdl(10,11,12,17,18,20,21)), "where with mask");

##############################
# originally in pptest
$x = ones(byte,3000);
dsumover($x,($y=null));
is($y->get_datatype, $PDL_D, "get_datatype" );
is($y->at, 3000, "at" );

my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5];
my $q = zeroes 5;
minimum_n_ind $p, $q;
ok(tapprox($q, pdl(0, 6, 7, 1, 9)), "minimum_n_ind");

##############################
# check that our random functions work with Perl's srand
TODO: {
   local $TODO = 'Some CPAN Testers fails for OpenBSD';

   srand 5;
   my $r1 = random 10;
   srand 5;
   my $r2 = random 10;
   ok(tapprox($r1, $r2), "random and srand");

   srand 10;
   $r1 = grandom 10;
   srand 10;
   $r2 = grandom 10;
   ok(tapprox($r1, $r2), "grandom and srand");
}
##############################
# Test that whichND works OK
my $r = xvals(10,10)+10*yvals(10,10);
$x = whichND( $r % 12 == 0 );

# Nontrivial case gives correct coordinates
ok(eval { sum($x != pdl([0,0],[2,1],[4,2],[6,3],[8,4],[0,6],[2,7],[4,8],[6,9]))==0 }, "whichND");
ok($x->type eq 'indx', "whichND returns indx-type ndarray for non-trivial case");
# Empty case gives matching Empty
$x = whichND( $r*0 );
ok($x->nelem==0, "whichND( 0*\$r ) gives an Empty PDL");
ok($x->ndims==2, "whichND( 0*\$r ) has 2 dims");
ok(($x->dim(0)==2 and $x->dim(1)==0), "whichND( 0*\$r ) is 2x0");
ok($x->type eq 'indx', "whichND( 0*\$r) type is indx");

# Scalar PDLs are treated as 1-PDLs
$x = whichND(pdl(5));
ok($x->nelem==1 && $x==0, "whichND scalar PDL");
ok($x->type eq 'indx', "whichND returns indx-type ndarray for scalar ndarray mask");

# Scalar empty case returns a 1-D vector of size 0
$x = whichND(pdl(0));
ok($x->nelem==0,  "whichND of 0 scalar is empty");
ok($x->ndims==1,  "whichND of 0 scalar has 1 dim");
ok($x->dim(0)==0, "whichND of 0 scalar: return 0 dim size is 0");
ok($x->type eq 'indx', "whichND returns indx-type ndarray for scalar empty case");

# Empty case returns Empty
$y = whichND( which(pdl(0)) );                              
ok($y->nelem==0, "whichND of Empty mask");
ok($y->type eq 'indx', "whichND returns indx-type ndarray for empty case");

# Nontrivial empty mask case returns matching Empty -- whichND(Empty[2x0x2]) should return Empty[3x0]
$y = whichND(zeroes(2,0,2));
ok(($y->ndims==2 and $y->dim(0)==3 and $y->dim(1)==0), "whichND(Empty[2x0x2]) returns Empty[3x0]");

##############################
# Simple test case for interpND
my $index;
my $z;
$x = xvals(10,10)+yvals(10,10)*10;
$index = cat(3+xvals(5,5)*0.25,7+yvals(5,5)*0.25)->reorder(2,0,1);
$z = 73+xvals(5,5)*0.25+2.5*yvals(5,5);
eval { $y = $x->interpND($index) };
ok(!$@);
ok(sum($y != $z) == 0, "interpND");

##############################
# Test glue
$x = xvals(2,2,2);
$y = yvals(2,2,2);
$c = zvals(2,2,2);
our $d;
eval { $d = $x->glue(1,$y,$c) };
ok(!$@);
ok(zcheck($d - pdl([[0,1],[0,1],[0,0],[1,1],[0,0],[0,0]],
                   [[0,1],[0,1],[0,0],[1,1],[1,1],[1,1]])), "glue");

##############################
# test new empty ndarray handling
$x = which ones(4) > 2;
$y = $x->long;
$c = $x->double;

ok(isempty $x, "isempty");
ok($y->avg == 0, "avg of Empty");
ok(! any isfinite $c->average, "isfinite of Empty");

##############################
# Test uniqvec
$x = pdl([[0,1],[2,2],[0,1]]);
$y = $x->uniqvec;
eval { $c = all($y==pdl([[0,1],[2,2]])) };
ok(!$@ && $c && $y->ndims==2, "uniqvec");

$x = pdl([[0,1]])->uniqvec;
eval { $c = all($x==pdl([[0,1]])) };
ok(!$@ && $c && $x->ndims==2, "uniqvec");

$x = pdl([[0,1,2]]); $x = $x->glue(1,$x,$x);
$y = $x->uniqvec;
eval { $c = all($y==pdl([0,1,2])) };
ok(!$@ && $c && $y->ndims==2, "uniqvec");

##############################
# Test bad handling in selector
$y = xvals(3);
ok(tapprox($y->which,PDL->pdl(1,2)), "which");
setbadat $y, 1;
ok(tapprox($y->which,PDL->pdl([2])), "which w BAD");
setbadat $y, 0;
setbadat $y, 2;
is($y->which->nelem,0, "which nelem w BAD");

############################
# Test intersect & setops
my $temp = sequence(10);
$x = which(($temp % 2) == 0);
$y = which(($temp % 3) == 0);
$c = setops($x, 'AND', $y);
ok(tapprox($c, pdl([0, 6])), "setops AND");
ok(tapprox(intersect($x,$y),pdl([0,6])), "intersect same as setops AND");
$c = setops($x,'OR',$y);
ok(tapprox($c, pdl([0,2,3,4,6,8,9])), "setops OR");
$c = setops($x,'XOR',$y);
ok(tapprox($c, pdl([2,3,4,8,9])), "setops XOR");
#Test intersect again
my $intersect_test=intersect(pdl(1,-5,4,0), pdl(0,3,-5,2));
ok (all($intersect_test==pdl(-5,0)), 'Intersect test values');

##############################
# Test uniqind
$x = pdl([0,1,2,2,0,1]);
$y = $x->uniqind;
eval { $c = all($y==pdl([0,1,3])) };
ok(!$@ && $c && $y->ndims==1, "uniqind");

$y = pdl(1,1,1,1,1)->uniqind;         # SF bug 3076570
ok(! $y->isempty);
eval { $c = all($y==pdl([0])) };
ok(!$@ && $c && $y->ndims==1, "uniqind, SF bug 3076570");

##############################
# Test whereND
$x = sequence(4,3,2);
$y = pdl(0,1,1,0);
$c = whereND($x,$y);
ok(all(pdl($c->dims)==pdl(2,3,2))) and
ok(all($c==pdl q[ [ [ 1  2] [ 5  6] [ 9 10] ]
                 [ [13 14] [17 18] [21 22] ] ]),
                                 "whereND [4]");
$y = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];
$c = whereND($x,$y);
ok(all(pdl($c->dims)==pdl(4,2))) and
ok(all($c==pdl q[ 2  3  5  8 ; 14 15 17 20 ]),
                            "whereND [4,3]");
$y = (random($x)<0.3);
$c = whereND($x,$y);
ok(all($c==where($x,$y)), "whereND vs where");
# sf.net bug #3415115, whereND fails to handle all zero mask case
$y = zeros(4);
$c = whereND($x,$y);
ok($c->isempty, 'whereND of all-zeros mask');
# Make sure whereND functions as an lvalue:
$x = sequence(4,3);
$y = pdl(0, 1, 1, 1);
eval { $x->whereND($y) *= -1 };
is($@, '', 'using whereND in lvalue context does not croak');
ok(all($x->slice("1:-1") < 0), 'whereND in lvalue context works');

#Test fibonacci.
my $fib=fibonacci(15);
my $fib_ans = pdl(1,1,2,3,5,8,13,21,34,55,89,144,233,377,610);
ok(all($fib == $fib_ans), 'Fibonacci sequence');

#Test which_both.
my $which_both_test=pdl(1,4,-2,0,5,0,1);
my ($nonzero,$zero)=which_both($which_both_test);
ok(all($nonzero==pdl(0,1,2,4,6)), 'Which_both nonzero indices');
ok(all($zero==pdl(3,5)), 'Which_both zero indices');

###### Testing Begins #########
my $im = new PDL [
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
 ];
my @minMax = $im->minmax;
ok($minMax[0] == 1, "minmax min" );
ok($minMax[1] == 13, "minmax max" );
ok(($im x $im)->sum == 3429, "matrix multiplication" );
my @statsRes = $im->stats;
ok(tapprox($statsRes[0],5.36), "stats: mean" );
ok(tapprox($statsRes[1],4.554), "stats: prms");
ok(tapprox($statsRes[2],3), "stats: median");
ok(tapprox($statsRes[3],1), "stats: min");
ok(tapprox($statsRes[4],13), "stats: max");
ok(tapprox($statsRes[6],4.462), "stats: rms");

@statsRes = $im->short->stats; # Make sure that stats are promoted to floating-point
ok(tapprox($statsRes[0],5.36), "stats: float mean");
ok(tapprox($statsRes[1],4.554), "stats: float prms");
ok(tapprox($statsRes[2],3), "stats: float median");
ok(tapprox($statsRes[3],1), "stats: float min");
ok(tapprox($statsRes[4],13), "stats: float max");
ok(tapprox($statsRes[6],4.462), "stats: float rms");

my $ones = ones(5,5);
@statsRes = $im->stats($ones);
# print "StatRes with moments = ".join(", ",@statsRes)."\n";
ok(tapprox($statsRes[0],5.36), "stats: trivial weights mean" );
ok(tapprox($statsRes[1],4.554), "stats: trivial weights prms" );
ok(tapprox($statsRes[2],3), "stats: trivial weights median" );
ok(tapprox($statsRes[3],1), "stats: trivial weights min" );
ok(tapprox($statsRes[4],13), "stats: trivial weights max" );
ok(tapprox($statsRes[6],4.462), "stats: trivial weights rms");

# complex matmult
my $cm1 = cdouble(1, czip(1, 1), 1);
my $cm2 = cdouble(2, 3, i());
my $got = $cm1 x $cm2->dummy(0);
ok all(approx $got, czip(5, 4)), 'complex matmult' or diag $got;

# which ND test
my $a1 = PDL->sequence(10,10,3,4);  

# $PDL::whichND_no_warning = 1;
# ($x, $y, $z, $w)=whichND($a1 == 203);
my ($x, $y, $z, $w) = whichND($a1 == 203)->mv(0,-1)->dog;  # quiet deprecation warning
ok($a1->at($x->list,$y->list,$z->list,$w->list) == 203, "whichND" );

$a1 = pdl(1,2,3,4);
my $b1 = append($a1,2);
ok(int(sum($b1))==12, "append");

# clip tests
ok(tapprox($im->hclip(5)->sum,83), "hclip" );
ok(tapprox($im->lclip(5)->sum,176), "lclip" );
ok(tapprox($im->clip(5,7)->sum,140), "clip" );
# with NaN badvalue
$im = sequence(3);
$im->badvalue(nan());
$im->badflag(1);
$im->set(1, nan());
my $clipped = $im->lclip(0);
is $clipped.'', '[0 BAD 2]', 'ISBAD() works when badvalue is NaN';

# indadd Test:
$a1 = pdl( 1,2,3);
my $ind = pdl( 1,4,6);
my $sum = zeroes(10);
indadd($a1,$ind, $sum);
ok(tapprox($sum->sum,6), "indadd" );

#one2nd test
$a1 = zeroes(3,4,5);
my $indicies = pdl(0,1,4,6,23,58,59);
($x,$y,$z)=$a1->one2nd($indicies);
ok(all( $x==pdl(0,1,1,0,2,1,2) ), "one2nd x");
ok(all( $y==pdl(0,0,1,2,3,3,3) ), "one2nd y");
ok(all( $z==pdl(0,0,0,0,1,4,4) ), "one2nd z");

{
my $yvalues =  (new PDL( 0..5))   - 20;
my $xvalues = -(new PDL (0..5))*.5;
my $x = new PDL(-2);
is( $x->interpol($xvalues,$yvalues), -16 );
}

# Some of these tests are based upon those in Chapter 5 of Programming
# Pearls, by J. Bentley
{
# choose a non-factor of two odd number for the length
my $N = 723;

my $ones = ones( $N );
my $idx  = sequence( $N );
my $x    = $idx * 10;

# create ordered duplicates so can test insertion points. This creates
# 7 sequential duplicates of the values 0-99
my $ndup = 7;
my $xdup = double long sequence( $ndup * 100 ) / $ndup;

# get insertion points and values
my ( $xdup_idx_insert_left, $xdup_idx_insert_right, $xdup_values ) = do {

    my ( $counts, $values ) = do { my @q = $xdup->rle; where( @q, $q[0] > 0 ) };

    ( $counts->cumusumover - $counts->at( 0 ), $counts->cumusumover, $values );

};

# The tests are table driven, with appropriate inputs and outputs for
# forward and reverse sorted arrays.  The tests sort the input array
# against itself, so we have a very good idea of which indices should
# be returned.  Most of the tests use that.  There are also specific
# tests for the endpoints as specified in the documentation, which
# may be easier for humans to parse and validate.

my %search = (

    sample => {

        all_the_same_element => $N - 1,    # finds right-most element

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx,
            nequal_p =>
              do { my $t = $idx + 1; $t->set( -1, $t->at( -1 ) - 1 ); $t },
            xdup => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left,
                values => $xdup_values,
            },
            docs => [
                '          V <= xs[0] : i = 0                      ' => [ (  0, -1, 0 ),
									  (  0,  0, 0 ),
									],
                'xs[0]  < V <= xs[-1] : i s.t. xs[i-1] < V <= xs[i]' => [ (  0,  1,  1 ),
									  (  1,  0,  1 ),
									  ( -1,  0, $N-1 ),
									],
                'xs[-1] < V           : i = $xs->nelem -1          ' => [ ( -1,  0, $N-1 ),
									  ( -1,  1, $N-1 ),
									],
            ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => do { my $t = $idx - 1; $t->set( 0, 0 ); $t },
            xdup     => {
                set => $xdup->slice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_left,
                values => $xdup_values,
            },
            docs => [
		'          V > xs[0]  : i = 0                      ' => [(0,  1, 0) ],
		'xs[0]  >= V > xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [(0,  0, 0),
									 (0, -1, 0),
									 (1,  0, 1),
									],
		'xs[-1] >= V          : i = $xs->nelem - 1         ' => [(-1,  0, $N-1),
									 (-1, -1, $N-1),
									],
            ],
       }

    },

    insert_leftmost => {

        all_the_same_element => 0,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => $idx + 1,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left,
                values => $xdup_values,
            },
	    docs => [
		'         V <= xs[0]  : i = 0                      ' => [ ( 0, -1, 0 ),
									  ( 0,  0,  0)
									],
		'xs[0]  < V <= xs[-1] : i s.t. xs[i-1] < V <= xs[i]' => [ ( 0, 1, 1 ),
									  ( 1, 0, 1 ),
									  ( -1, 0, $N-1 ),
									],
		'xs[-1] < V           : i = $xs->nelem             ' => [
									 ( -1, 1, $N ),
									],

	    ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx,
            nequal_p => $idx - 1,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_left,
                values => $xdup_values,
            },
           docs => [
	       '          V >  xs[0]  : i = -1                     ' => [ ( 0,   1, -1 ), ],
	       'xs[0]  >= V >= xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [ ( 0,   0,  0 ),
									  ( 0,  -1,  0 ),
									],
	       'xs[-1] >= V           : i = $xs->nelem -1          ' => [ ( -1,  0, $N-1 ),
									  ( -1, -1, $N-1 ),
									],

           ],
        },
    },

    insert_rightmost => {

        all_the_same_element => $N,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx + 1,
            nequal_m => $idx,
            nequal_p => $idx + 1,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_right,
                values => $xdup_values,
                idx_offset => -1,   # returns index of element *after* the value
            },
	    docs => [
		'          V < xs[0]  : i = 0                      ' => [ ( 0, -1, 0 ) ],
		'xs[0]  <= V < xs[-1] : i s.t. xs[i-1] <= V < xs[i]' => [ ( 0, 0, 1 ),
									  ( 0, 1, 1 ),
									  ( 1, 0, 2 ),
									],
		'xs[-1] <= V          : i = $xs->nelem             ' => [ ( -1, 0, $N ),
									  ( -1, 1, $N ),
									],
            ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx - 1,
            nequal_m => $idx,
            nequal_p => $idx - 1,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - 1 - $xdup_idx_insert_right,
                values => $xdup_values,
                idx_offset => +1,   # returns index of element *after* the value
            },
	    docs => [
		'         V >= xs[0]  : i = -1                     ' => [ ( 0,   1, -1 ),
									  ( 0,   0, -1 ),
									],
		'xs[0]  > V >= xs[-1] : i s.t. xs[i] >= V > xs[i+1]' => [ ( 0,  -1, 0 ),
									  ( -1,  1, $N-2 ),
									  ( -1,  0, $N-2 ),
									],
		'xs[-1] > V           : i = $xs->nelem -1          ' => [ ( -1,  -1, $N-1 ) ]
            ],
        },
    },

    match => {

        all_the_same_element => ( $N ) >> 1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => -( $idx + 1 ),
            nequal_p => -( $idx + 1 + 1 ),
            xdup     => {
                set    => $xdup,
                values => $xdup_values,
            },
	    docs => [
		'V < xs[0]  : i = -1' => [ ( 0,   -1, -1 ), ],
		'V == xs[n] : i = n' => [ ( 0,  0, 0 ),
					  ( -1, 0, $N-1 ) ],
		'xs[0] > V > xs[-1], V != xs[n] : -(i+1) s.t. xs[i] > V > xs[i+1]' => [ ( 0,   1, -( 1 + 1)  ),
											( 1,  -1, -( 1 + 1 ) ),
											( 1,   1, -( 2 + 1 ) ),
											( -1,  -1, -( $N - 1 + 1 ) ),
										      ],
		' V > xs[-1] : -($xs->nelem - 1 + 1)' => [ ( -1,   1, -( $N + 1)  ), ]
            ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => -( $idx + 1 ),
            nequal_p => -( $idx + 1 - 1 ),
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                values => $xdup_values,
            },
	    docs => [
		'V > xs[0]  : i = 0' => [ ( 0,  1, 0 ), ],
		'V == xs[n] : i = n' => [ ( 0,  0, 0 ),
					  ( -1, 0, $N-1 ) ],
		'xs[0] < V < xs[-1], V != xs[n] : -(i+1) s.t. xs[i-1] > V > xs[i]' => [ ( 0,  -1, -( 0 + 1)  ),
											( 1,   1, -( 0 + 1 ) ),
											( 1,  -1, -( 1 + 1 ) ),
											( -1,  -1, -( $N - 1 + 1 ) ),
										      ],
		' xs[-1] > V: -($xs->nelem - 1 + 1)' => [ ( -1,   -1, -( $N - 1 + 1)  ), ]
            ],
        },
    },

    bin_inclusive => {

        all_the_same_element => $N - 1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx,
            nequal_m => $idx - 1,
            nequal_p => $idx,
            xdup     => {
                set    => $xdup,
                idx    => $xdup_idx_insert_left + $ndup - 1,
                values => $xdup_values,
            },
	    docs => [
		'          V < xs[0]  : i = -1                     ' => [ ( 0, -1, -1 ), ],
		'xs[0]  <= V < xs[-1] : i s.t. xs[i] <= V < xs[i+1]' => [ ( 0,  0,  0 ),
									  ( 0,  1,  0 ),
									  ( 1, -1,  0 ),
									  ( 1,  0,  1 ),
									  ( -1, -1, $N-2 ),
									],
		'xs[-1] <= V          : i = $xs->nelem - 1         ' => [
									  ( -1, 0,  $N-1 ),
									  ( -1, 1,  $N-1 ),
									]
            ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx,
            nequal_m => $idx + 1,
            nequal_p => $idx,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - ( 1 + $xdup_idx_insert_left + $ndup - 1 ),
                values => $xdup_values,
            },
	    docs => [
		'          V >= xs[0]  : i = 0                        ' => [ (0, 1, 0 ),
									     (0, 0, 0 )
									 ],
		'xs[0]  >  V >= xs[-1] : i s.t. xs[i+1] > V >= xs[i]' => [ ( 0, -1, 1 ),
									   ( 1,  1, 1 ),
									   ( 1,  0, 1 ),
									   ( 1, -1, 2 ),
									   ( -1, 0, $N-1 ),
									 ],
		'xs[-1] >  V           : i = $xs->nelem -1          ' => [ ( -1, -1, $N ) ],
            ],
        },
    },

    bin_exclusive => {

        all_the_same_element => -1,

        forward => {
            idx      => $idx,
            x        => $x,
            equal    => $idx - 1,
            nequal_m => $idx - 1,
            nequal_p => $idx,
            xdup     => {
                set        => $xdup,
                idx        => $xdup_idx_insert_left - 1,
                values     => $xdup_values,
                idx_offset => 1,
            },
	    docs => [
		'          V <= xs[0]  : i = -1                     ' => [ ( 0, -1, -1 ),
									   ( 0,  0, -1 ),
									 ],
		'xs[0]  <  V <= xs[-1] : i s.t. xs[i] < V <= xs[i+1]' => [ ( 0,  1, 0 ),
									   ( 1, -1, 0 ),
									   ( 1,  0, 0 ),
									   ( 1,  1, 1 ),
									   ( -1, -1, $N-2 ),
									   ( -1, 0, $N-2 ),
									],
		'xs[-1] <  V           : i = $xs->nelem - 1         ' => [
									  ( -1, 1, $N-1 ),
									 ],
            ],
        },

        reverse => {
            idx      => $idx,
            x        => $x->mslice( [ -1, 0 ] ),
            equal    => $idx + 1,
            nequal_m => $idx + 1,
            nequal_p => $idx,
            xdup     => {
                set => $xdup->mslice( [ -1, 0 ] ),
                idx => $xdup->nelem - ( 1 + $xdup_idx_insert_left - 1 ),
                values     => $xdup_values,
                idx_offset => -1,
            },
	    docs => [
		'          V >  xs[0]  : i = 0                      ' => [ ( 0,  1, 0 ), ],
		'xs[0]  >  V >  xs[-1] : i s.t. xs[i-1] >= V > xs[i]' => [ ( 0,  0, 1 ),
									   ( 0, -1, 1 ),
									   ( -1, 1, $N-1 ),
									 ],
		'xs[-1] >= V           : i = $xs->nelem -1          ' => [ ( -1, 0, $N ),
									   ( -1, -1, $N ),
									 ],
	    ],
        },
    },

);

for my $mode (
    sort keys %search
  )
{

    my $data   = $search{$mode};

    subtest $mode => sub {

        my ( $got, $exp );
        for my $sort_direction ( qw[ forward reverse ] ) {

            subtest $sort_direction => sub {

		my $so = $data->{$sort_direction}
		  or plan( skip_all => "not testing $sort_direction!\n" );

                ok(
                    all(
                        ( $got = vsearch( $so->{x}, $so->{x}, { mode => $mode } ) )
			==
			( $exp = $so->{equal} )
                    ),
                    'equal elements'
                ) or diag "got     : $got\nexpected: $exp\n";

                ok(
                    all(
                        ( $got = vsearch( $so->{x} - 5, $so->{x}, { mode => $mode } ) )
                        ==
			( $exp = $so->{nequal_m} )
                    ),
                    'non-equal elements x[i] < xs[i] (check lower bound)'
                ) or diag "got     : $got\nexpected: $exp\n";

                ok(
                    all(
                        ( $got = vsearch( $so->{x} + 5, $so->{x}, { mode => $mode } ) )
                        ==
			( $exp = $so->{nequal_p} )
                    ),
                    'non-equal elements x[i] > xs[i] (check upper bound)'
                ) or diag "got     : $got\nexpected: $exp\n";


		# duplicate testing.

		# check for values. note that the rightmost routine returns
		# the index of the element *after* the last duplicate
		# value, so we need an offset
		ok(
		    all(
			( $got = $so->{xdup}{set}->index( vsearch( $so->{xdup}{values}, $so->{xdup}{set}, { mode => $mode } )
							                 + ($so->{xdup}{idx_offset} || 0) ) )
			==
			( $exp = $so->{xdup}{values} )
		    ),
		    'duplicates values'
		) or diag "got     : $got\nexpected: $exp\n";

		# if there are guarantees about which duplicates are returned, test it
		if ( exists $so->{xdup}{idx} ) {

		    ok(
			all(
			    ( $got = vsearch( $so->{xdup}{values}, $so->{xdup}{set}, { mode => $mode } ) )
			    ==
			    ( $exp = $so->{xdup}{idx} )
			),
			'duplicate indices'
		    ) or diag "got     : $got\nexpected: $exp\n";

		}

		if ( exists $so->{docs} ) {

		    while( my ($label, $inputs ) = splice( @{$so->{docs}}, 0, 2 )  ) {

			while( @$inputs ) {

			    my ( $idx, $offset, $exp ) = splice( @$inputs, 0, 3 );
			    my $value = $so->{x}->at($idx) + $offset;

			    is ( $got = ( vsearch( $value, $so->{x}, { mode => $mode } )->sclr), $exp, "$label: ($idx, $offset)" );

			}
		    }
		}


            };
        }

        ok(
            all(
                ( $got = vsearch( $ones, $ones, { mode => $mode } ) )
                ==
                ( $exp = $data->{all_the_same_element} )
            ),
            'all the same element'
        ) or diag "got     : $got\nexpected: $exp\n";
    };

}

# test vsearch API to ensure backwards compatibility
{
    my $vals = random( 100 );
    my $xs = sequence(100) / 99;

    # implicit output ndarray
    my $indx0 = vsearch( $vals, $xs );

    my $ret = vsearch( $vals, $xs, my $indx1 = PDL->null() );

    is( $ret, undef, "no return from explicit output ndarray" );

    ok ( all ( $indx0 == $indx1 ),
	 'explicit ndarray == implicit ndarray' );
}
}

done_testing;
