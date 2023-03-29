use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use PDL::Types;

sub tapprox {
    my($x,$y) = @_;
    $_ = pdl($_) for $x, $y;
    if((my $dims_x = join(',',$x->dims)) ne (my $dims_y = join(',',$y->dims))) {
       diag "APPROX: $x $y\n";
       diag "UNEQDIM: |$dims_x| |$dims_y|\n";
       return 0;
    }
    return 1 if $x->isempty and $y->isempty;
    my $d = max( abs($x-$y) );
    if($d >= 0.01) {
       diag "got=$x expected=$y\n";
    }
    $d < 0.01;
}

ok tapprox(pdl(1,2,3)->cmpvec(pdl(3,2,1)), -1), 'cmpvec less';
ok tapprox(pdl(3,2,1)->cmpvec(pdl(1,2,3)), 1), 'cmpvec more';
ok tapprox(pdl(3,2,1)->cmpvec(pdl(3,2,1)), 0), 'cmpvec same';
is pdl('[1 BAD]')->cmpvec(pdl(3,2)).'', '-1', 'cmpvec bad before';
is pdl('[BAD 1]')->cmpvec(pdl(3,2)).'', 'BAD', 'cmpvec bad';

ok tapprox(pdl(3,2,1)->eqvec(pdl(1,2,3)), 0), 'eqvec diff';
ok tapprox(pdl(3,2,1)->eqvec(pdl(3,2,1)), 1), 'eqvec same';
is pdl('[2 1 BAD]')->eqvec(pdl(1,3,2)).'', 'BAD', 'eqvec bad before';
is pdl('[2 BAD 1]')->eqvec(pdl(2,3,2)).'', 'BAD', 'eqvec bad';

my $x = PDL->pdl([[5,4,3],[2,3,1.5]]);
ok(tapprox($x->average(), PDL->pdl([4, 2.16666])), "average");
ok(tapprox($x->sumover(), PDL->pdl([12, 6.5])), "sumover");
ok(tapprox($x->prodover(), PDL->pdl([60, 9])), "prodover");

my $y = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
my $c = ($y->xvals) + 10;
ok(tapprox($y->where($y>4), PDL->pdl(5,6)), "where with >");
ok(tapprox($y->which, PDL->pdl(0,1,2,7,8,10,11)), "which");
ok(tapprox($c->where($y), PDL->pdl(10,11,12,17,18,20,21)), "where with mask");
ok zeroes(3)->which->isempty, 'which on all-zero returns empty';

$y = sequence(10) + 2;
my ($big, $small) = where_both($y, $y > 5);
$big += 2, $small -= 1;
ok tapprox($big, pdl('[8 9 10 11 12 13]')), 'where_both big + 2 is right';
ok tapprox($small, pdl('[1 2 3 4]')), 'where_both small - 2 is right';
ok tapprox($y, pdl('[1 2 3 4 8 9 10 11 12 13]')), 'where_both dataflow affected orig';

{
  my $orig = ones(byte, 300);
  my $xvals = $orig->xvals;
  is $xvals->at(280), 280, 'non-wrapped xvals from byte ndarray';
}

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
$q = minimum_n_ind($p, 5);
ok(tapprox($q, pdl(0, 6, 7, 1, 9)), "minimum_n_ind usage 2");
minimum_n_ind($p, $q = null, 5);
ok(tapprox($q, pdl(0, 6, 7, 1, 9)), "minimum_n_ind usage 3");
$p = pdl '[1 BAD 3 4 7 9 1 1 6 2 5]';
$q = zeroes 5;
minimum_n_ind $p, $q;
is $q.'', '[0 6 7 9 2]', "minimum_n_ind BAD";
$p = pdl '[1 BAD 3 4 BAD BAD]';
$q = zeroes 5;
minimum_n_ind $p, $q;
is $q.'', '[0 2 3 BAD BAD]', "minimum_n_ind insufficient good";
$p = pdl '[1 BAD 3 4 BAD BAD 3 1 5 8 9]';
$q = zeroes 5;
minimum_n_ind $p, $q;
is $q.'', '[0 7 2 6 3]', "minimum_n_ind some bad, sufficient good";

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
is $x->type, 'indx', "whichND returns indx-type ndarray for non-trivial case";
# Empty case gives matching Empty
$x = whichND( $r*0 );
is $x->nelem, 0, "whichND( 0*\$r ) gives an Empty PDL";
is_deeply [$x->dims], [2,0], "whichND( 0*\$r ) is 2x0";
is $x->type, 'indx', "whichND( 0*\$r) type is indx";

# Scalar PDLs are treated as 1-PDLs
$x = whichND(pdl(5));
is $x->nelem, 1, "whichND scalar PDL";
is $x, 0, "whichND scalar PDL";
is $x->type, 'indx', "whichND returns indx ndarray for scalar ndarray mask";

# Scalar empty case returns a 1-D vector of size 0
$x = whichND(pdl(0));
is $x->nelem, 0,  "whichND of 0 scalar is empty";
is_deeply [$x->dims], [0], "whichND of 0 scalar: return 0 dim size is 0";
is $x->type, 'indx', "whichND returns indx-type ndarray for scalar empty case";

# Empty case returns Empty
$y = whichND( which(pdl(0)) );                              
is $y->nelem, 0, "whichND of Empty mask";
is $y->type, 'indx', "whichND returns indx-type ndarray for empty case";

# Nontrivial empty mask case returns matching Empty -- whichND(Empty[2x0x2]) should return Empty[3x0]
$y = whichND(zeroes(2,0,2));
is_deeply [$y->dims], [3,0], "whichND(Empty[2x0x2]) returns Empty[3x0]";

$r = zeroes(7, 7); $r->set(3, 4, 1);
is $r->whichND.'', <<EOF, 'whichND works right (was failing on 32-bit)';
\n[\n [3 4]\n]
EOF

$x = pdl('[[i 2+3i] [4+5i 6+7i]]');
ok tapprox $x->norm, pdl(<<'EOF'), 'native complex norm works' or diag $x->norm;
[
 [0.267261i 0.534522+0.801783i]
 [0.356348+0.445435i 0.534522+0.623609i]
]
EOF

##############################
# Simple test case for interpND
$x = xvals(10,10)+yvals(10,10)*10;
my $index = cat(3+xvals(5,5)*0.25,7+yvals(5,5)*0.25)->reorder(2,0,1);
my $z = 73+xvals(5,5)*0.25+2.5*yvals(5,5);
eval { $y = $x->interpND($index) };
is $@, '';
is sum($y != $z), 0, "interpND";

##############################
# Test glue
$x = xvals(2,2,2);
$y = yvals(2,2,2);
$c = zvals(2,2,2);
our $d;
eval { $d = $x->glue(1,$y,$c) };
is $@, '';
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
is $@, '';
ok $c, "uniqvec";
is $y->ndims, 2, "uniqvec";

$x = pdl([[0,1]])->uniqvec;
eval { $c = all($x==pdl([[0,1]])) };
is $@, '';
ok $c, "uniqvec";
is $x->ndims, 2, "uniqvec";

$x = pdl([[0,1,2]]); $x = $x->glue(1,$x,$x);
$y = $x->uniqvec;
eval { $c = all($y==pdl([0,1,2])) };
is $@, '';
ok $c, "uniqvec";
is $y->ndims, 2, "uniqvec";

##############################
# Test bad handling in selector
$y = xvals(3);
ok(tapprox($y->which,PDL->pdl(1,2)), "which");
setbadat $y, 1;
ok(tapprox($y->which,PDL->pdl([2])), "which w BAD");
setbadat $y, 0;
setbadat $y, 2;
is($y->which->nelem,0, "which nelem w BAD");

{
ok tapprox(sequence(4)->uniq, sequence(4)), 'sequence(4)->uniq';
ok tapprox(ones(4)->uniq, ones(1)), 'ones(4)->uniq';
ok tapprox(empty()->uniq, empty()), 'empty()->uniq';
ok tapprox(pdl([[1]])->uniq, ones(1)), '2-deep uniq flattens'; # Data::Frame relies
}

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
ok tapprox($intersect_test, pdl(-5,0)), 'Intersect test values';
{
# based on cases supplied by @jo-37
my @cases = (
  [ pdl(1), empty(), empty() ],
  [ ones(1), empty(), empty() ],
  [ ones(4), empty(), empty() ],
  [ sequence(4), empty(), empty() ],
  [ pdl(1), ones(2), ones(1) ],
  [ ones(1), ones(2), ones(1) ],
  [ ones(4), ones(2), ones(1) ],
  [ sequence(4), ones(2), ones(1) ],
);
ok tapprox(setops($_->[0], 'AND', $_->[1]), $_->[2]), "$_->[0] AND $_->[1]" for @cases;
}

##############################
# Test uniqind
$x = pdl([0,1,2,2,0,1]);
$y = $x->uniqind;
eval { $c = all($y==pdl([0,1,3])) };
is $@, '';
ok $c, "uniqind" or diag "got: $y";
is $y->ndims, 1, "uniqind";

$y = pdl(1,1,1,1,1)->uniqind;         # SF bug 3076570
ok(! $y->isempty);
eval { $c = all($y==pdl([0])) };
is $@, '';
ok $c, "uniqind";
is $y->ndims, 1, "uniqind, SF bug 3076570";

##############################
# Test whereND
$x = sequence(4,3,2);
$y = pdl(0,1,1,0);
$c = whereND($x,$y);
is_deeply [$c->dims], [2,3,2];
ok tapprox($c, pdl q[[[1 2] [5 6] [9 10]] [[13 14] [17 18] [21 22]]]), "whereND [4]";
$y = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];
$c = whereND($x,$y);
is_deeply [$c->dims], [4,2];
ok tapprox($c, pdl q[ 2  3  5  8 ; 14 15 17 20 ]), "whereND [4,3]";
$y = (random($x)<0.3);
$c = whereND($x,$y);
ok tapprox($c->squeeze, where($x,$y)), "whereND vs where";
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
ok tapprox($fib, $fib_ans), 'Fibonacci sequence';

#Test which_both.
my $which_both_test=pdl(1,4,-2,0,5,0,1);
my ($nonzero,$zero)=which_both($which_both_test);
ok tapprox($nonzero, pdl(0,1,2,4,6)), 'Which_both nonzero indices';
ok tapprox($zero, pdl(3,5)), 'Which_both zero indices';

###### Testing Begins #########
my $im = PDL->new([
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
 ]);
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
ok(tapprox($statsRes[0],5.36), "stats: trivial weights mean" );
ok(tapprox($statsRes[1],4.554), "stats: trivial weights prms" );
ok(tapprox($statsRes[2],3), "stats: trivial weights median" );
ok(tapprox($statsRes[3],1), "stats: trivial weights min" );
ok(tapprox($statsRes[4],13), "stats: trivial weights max" );
ok(tapprox($statsRes[6],4.462), "stats: trivial weights rms");

# complex matmult
my $cm1 = pdl('1 1+i 1');
my $cm2 = pdl('2 3 i')->transpose;
my $got = $cm1 x $cm2;
ok tapprox($got, pdl('[[5+4i]]')), 'complex matmult';
throws_ok { scalar $cm1->transpose x $cm2 } qr/mismatch/, 'good error on mismatch matmult';

{
my $pa = pdl [[ 1,  2,  3,  0],
      [ 1, -1,  2,  7],
      [ 1,  0,  0,  1]];
my $pb = pdl [[1, 1],
     [0, 2],
     [0, 2],
     [1, 1]];
my $pc = pdl [[ 1, 11],
      [ 8, 10],
      [ 2,  2]];
my $res = $pa x $pb;
ok tapprox($pc,$res);
matmult($pa, $pb, $res = null);
ok tapprox($pc,$res), 'res=null';
my $pa_sliced = $pa->dummy(0, 3)->dummy(-1, 3)->make_physical->slice('(1),,,(1)');
$res = $pa_sliced x $pb;
ok tapprox($pc,$res);
$res = zeroes(2, 3);
matmult($pa, $pb, $res);
ok tapprox($pc,$res), 'res=zeroes';
$res = ones(2, 3);
matmult($pa, $pb, $res);
ok tapprox($pc,$res), 'res=ones';
my $eq = float [[1,1,1,1]];  # a 4,1-matrix ( 1 1 1 1 )
# Check collapse: output should be a 1x2...
ok tapprox($eq x $pb  , pdl([[2,6]])); # ([4x1] x [2x4] -> [1x2])
# Check dimensional exception: mismatched dims should throw an error
dies_ok {
	my $pz = $pb x $eq; # [2x4] x [4x1] --> error (2 != 1)
};
{
# Check automatic scalar multiplication
my $pz;
lives_ok { $pz = $pb x 2; };
ok tapprox($pz,$pb * 2);
}
{
my $pz;
lives_ok { $pz = pdl(3) x $pb; };
ok tapprox($pz,$pb * 3);
}
}

# which ND test
my $a1 = PDL->sequence(10,10,3,4);  

($x, $y, $z, my $w) = whichND($a1 == 203)->mv(0,-1)->dog;
ok($a1->at($x->list,$y->list,$z->list,$w->list) == 203, "whichND" );

$a1 = pdl(1,2,3,4);
my $b1 = append($a1,2);
ok(int(sum($b1))==12, "append");
$b1 = append(null, null);
ok !$b1->isnull, 'append(null, null) returns non-null';
ok $b1->isempty, 'append(null, null) returns an empty';
append(null, null, $b1);
ok !$b1->isnull, 'append(null, null, b1) sets non-null';
ok $b1->isempty, 'append(null, null, b1) sets an empty';
$b1 = indx(1,2);
is $b1->type, 'indx', '$indx_pdl is an indx pdl';
$b1 = $b1->append(-1);
is $b1->type, 'indx', 'append($indx_pdl, -1) returns an indx pdl';
is $b1.'', '[1 2 -1]', 'append($indx_pdl, -1) correct content';

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
my $indices = pdl(0,1,4,6,23,58,59);
($x,$y,$z)=$a1->one2nd($indices);
ok tapprox($x, pdl(0,1,1,0,2,1,2)), "one2nd x";
ok tapprox($y, pdl(0,0,1,2,3,3,3)), "one2nd y";
ok tapprox($z, pdl(0,0,0,0,1,4,4)), "one2nd z";

{
my $yvalues =  PDL->new(0..5) - 20;
my $xvalues = -PDL->new(0..5)*.5;
my $x = PDL->new(-2);
is( $x->interpol($xvalues,$yvalues), -16, "interpolate: real-valued" );
}

{
my $yvalues =  (PDL->new(0..5) - 20) * (1+i()) ;
my $xvalues = -PDL->new(0..5)*.5;
my $x = PDL->new(-2);
is( $x->interpol($xvalues,$yvalues), -16 - 16*i, "interpolate: complex-valued" );
ok( !eval { $x->interpol($xvalues*i(),$yvalues) } , "interpolate: x must be real" );
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

                ok tapprox(
                    vsearch( $so->{x}, $so->{x}, { mode => $mode } ),
                    $so->{equal}
                ),
                'equal elements';

                ok tapprox(
                    vsearch( $so->{x} - 5, $so->{x}, { mode => $mode } ),
                    $so->{nequal_m}
                ),
                'non-equal elements x[i] < xs[i] (check lower bound)';

                ok tapprox(
                    vsearch( $so->{x} + 5, $so->{x}, { mode => $mode } ),
                    $so->{nequal_p}
                ),
                'non-equal elements x[i] > xs[i] (check upper bound)';

		# duplicate testing.

		# check for values. note that the rightmost routine returns
		# the index of the element *after* the last duplicate
		# value, so we need an offset
                ok tapprox(
                    $so->{xdup}{set}->index( vsearch( $so->{xdup}{values}, $so->{xdup}{set}, { mode => $mode } )
                                                                     + ($so->{xdup}{idx_offset} || 0) ),
                    $so->{xdup}{values}
                ),
                'duplicates values';

		# if there are guarantees about which duplicates are returned, test it
		if ( exists $so->{xdup}{idx} ) {
		    ok tapprox(
                        vsearch( $so->{xdup}{values}, $so->{xdup}{set}, { mode => $mode } ),
                        $so->{xdup}{idx}
                    ),
                    'duplicate indices';
		}
		if ( exists $so->{docs} ) {
		    while( my ($label, $inputs ) = splice( @{$so->{docs}}, 0, 2 )  ) {
			while( @$inputs ) {
			    my ( $idx, $offset, $exp ) = splice( @$inputs, 0, 3 );
			    my $value = $so->{x}->at($idx) + $offset;
			    is vsearch( $value, $so->{x}, { mode => $mode } )->sclr, $exp, "$label: ($idx, $offset)";
			}
		    }
		}
            };
        }

        ok tapprox(
            vsearch( $ones, $ones, { mode => $mode } )->uniq->squeeze,
            $data->{all_the_same_element}
        ),
        'all the same element';
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
    ok tapprox($indx0, $indx1), 'explicit ndarray == implicit ndarray';
}
}

my $vdim = 4;
my $v1 = zeroes($vdim);
my $v2 = pdl($v1);
$v2->set(-1,1);

ok $v1->cmpvec($v2)<0, "cmpvec:1d:<";
ok $v2->cmpvec($v1)>0, "cmpvec:1d:>";
is $v1->cmpvec($v1)->sclr, 0, "cmpvec:1d:==";

##-- 4..5: qsortvec, qsortveci
my $p2d  = pdl([[1,2],[3,4],[1,3],[1,2],[3,3]]);

ok tapprox($p2d->qsortvec, pdl(long,[[1,2],[1,2],[1,3],[3,3],[3,4]])), "qsortvec";
ok tapprox($p2d->dice_axis(1,$p2d->qsortveci), $p2d->qsortvec), "qsortveci";

my $which = pdl(long,[[0,0],[0,0],[0,1],[0,1],[1,0],[1,0],[1,1],[1,1]]);
my $find  = $which->slice(",0:-1:2");

ok tapprox($find->vsearchvec($which), pdl(long,[0,2,4,6])), "vsearchvec():match";
ok tapprox(pdl([-1,-1])->vsearchvec($which), 0), "vsearchvec():<<";
ok tapprox(pdl([2,2])->vsearchvec($which), $which->dim(1)-1), "vsearchvec():>>";

my $vtype = long;
my $universe = pdl($vtype,[ [0,0],[0,1],[1,0],[1,1] ]);
$v1 = $universe->dice_axis(1,pdl([0,1,2]));
$v2 = $universe->dice_axis(1,pdl([1,2,3]));

($c,my $nc) = $v1->unionvec($v2);
ok tapprox($c, pdl($vtype, [ [0,0],[0,1],[1,0],[1,1],[0,0],[0,0] ])), "unionvec:list:c";
is $nc, $universe->dim(1), "unionvec:list:nc";
my $cc = $v1->unionvec($v2);
ok tapprox($cc, $universe), "unionvec:scalar";

($c,$nc) = $v1->intersectvec($v2);
ok tapprox($c, pdl($vtype, [ [0,1],[1,0],[0,0] ])), "intersectvec:list:c";
is $nc->sclr, 2, "intersectvec:list:nc";
$cc = $v1->intersectvec($v2);
ok tapprox($cc, $universe->slice(",1:2")), "intersectvec:scalar";

($c,$nc) = $v1->setdiffvec($v2);
ok tapprox($c, pdl($vtype, [ [0,0], [0,0],[0,0] ])), "setdiffvec:list:c";
is $nc, 1, "setdiffvec:list:nc";
$cc = $v1->setdiffvec($v2);
ok tapprox($cc, pdl($vtype, [[0,0]])), "setdiffvec:scalar";

my $all = sequence(20);
my $amask = ($all % 2)==0;
my $bmask = ($all % 3)==0;
my $a   = $all->where($amask);
my $b   = $all->where($bmask);

ok tapprox(scalar($a->union_sorted($b)), $all->where($amask | $bmask)), "union_sorted";
ok tapprox(scalar($a->intersect_sorted($b)),  $all->where($amask & $bmask)), "intersect_sorted";
ok tapprox(scalar($a->setdiff_sorted($b)), $all->where($amask & $bmask->not)), "setdiff_sorted";

##--------------------------------------------------------------
## dim-checks and implicit broadcast dimensions
##  + see https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4

sub test_broadcast_dimensions {
  ##-- unionvec
  my $empty = zeroes(3,0);
  my $uw = pdl([[-3,-2,-1],[1,2,3]]);
  my $wx = pdl([[1,2,3],[4,5,6]]);
  my $xy = pdl([[4,5,6],[7,8,9]]);

  # unionvec: basic
  ok tapprox(scalar($uw->unionvec($wx)), pdl([[-3,-2,-1],[1,2,3],[4,5,6]])), "unionvec - broadcast dims - uw+wx";
  ok tapprox(scalar($uw->unionvec($xy)), pdl([[-3,-2,-1],[1,2,3],[4,5,6],[7,8,9]])), "unionvec - broadcast dims - uw+xy";
  ok tapprox(scalar($empty->unionvec($wx)), $wx), "unionvec - broadcast dims - 0+wx";
  ok tapprox(scalar($wx->unionvec($empty)), $wx), "unionvec - broadcast dims - wx+0";
  ok tapprox(scalar($empty->unionvec($empty)), $empty), "unionvec - broadcast dims - 0+0";

  # unionvec: broadcasting
  my $k = 2;
  my $kempty = $empty->slice(",,*$k");
  my $kuw = $uw->slice(",,*$k");
  my $kwx = $wx->slice(",,*$k");
  my $kxy = $xy->slice(",,*$k");
  ok tapprox(scalar($kuw->unionvec($wx)), pdl([[-3,-2,-1],[1,2,3],[4,5,6]])->slice(",,*$k")), "unionvec - broadcast dims - uw(*k)+wx";
  ok tapprox(scalar($kuw->unionvec($xy)), pdl([[-3,-2,-1],[1,2,3],[4,5,6],[7,8,9]])->slice(",,*$k")), "unionvec - broadcast dims - uw(*k)+xy";
  ok tapprox(scalar($kempty->unionvec($wx)), $kwx), "unionvec - broadcast dims - 0(*k)+wx";
  ok tapprox(scalar($kwx->unionvec($empty)), $kwx), "unionvec - broadcast dims - wx(*k)+0";
  ok tapprox(scalar($kempty->unionvec($empty)), $kempty), "unionvec - broadcast dims - 0(*k)+0";

  ##-- intersectvec

  my $needle0 = pdl([[-3,-2,-1]]);
  my $needle1 = pdl([[1,2,3]]);
  my $needles = pdl([[-3,-2,-1],[1,2,3]]);
  my $haystack = pdl([[1,2,3],[4,5,6],[7,8,9],[10,11,12]]);

  # intersectvec: basic
  ok tapprox(scalar($needle0->intersectvec($haystack)), $empty), "intersectvec - broadcast dims - needle0&haystack";
  ok tapprox(scalar($needle1->intersectvec($haystack)), $needle1), "intersectvec - broadcast dims - needle1&haystack";
  ok tapprox(scalar($needles->intersectvec($haystack)), $needle1), "intersectvec - broadcast dims - needles&haystack";
  ok tapprox(scalar($haystack->intersectvec($haystack)), $haystack), "intersectvec - broadcast dims - haystack&haystack";
  ok tapprox(scalar($haystack->intersectvec($empty)), $empty), "intersectvec - broadcast dims - haystack&empty";
  ok tapprox(scalar($empty->intersectvec($haystack)), $empty), "intersectvec - broadcast dims - empty&haystack";

  # intersectvec: broadcasting
  my $kneedle0 = $needle0->slice(",,*$k");
  my $kneedle1 = $needle1->slice(",,*$k");
  my $kneedles = pdl([[[-3,-2,-1]],[[1,2,3]]]);
  my $khaystack = $haystack->slice(",,*$k");
  ok tapprox(scalar($kneedle0->intersectvec($haystack)), $kempty), "intersectvec - broadcast dims - needle0(*k)&haystack";
  ok tapprox(scalar($kneedle1->intersectvec($haystack)), $kneedle1), "intersectvec - broadcast dims - needle1(*k)&haystack";
  ok tapprox(
	scalar($kneedles->intersectvec($haystack)),
	pdl([[[0,0,0]],[[1,2,3]]])), "intersectvec - broadcast dims - needles(*k)&haystack";
  ok tapprox(scalar($khaystack->intersectvec($haystack)), $khaystack), "intersectvec - broadcast dims - haystack(*k)&haystack";
  ok tapprox(scalar($khaystack->intersectvec($empty)), $kempty), "intersectvec - broadcast dims - haystack(*k)&empty";
  ok tapprox(scalar($kempty->intersectvec($haystack)), $kempty), "intersectvec - broadcast dims - empty(*k)&haystack";

  ##-- setdiffvec

  # setdiffvec: basic
  ok tapprox(scalar($haystack->setdiffvec($needle0)), $haystack), "setdiffvec - broadcast dims - haystack-needle0";
  ok tapprox(scalar($haystack->setdiffvec($needle1)), $haystack->slice(",1:-1")), "setdiffvec - broadcast dims - haystack-needle1";
  ok tapprox(scalar($haystack->setdiffvec($needles)), $haystack->slice(",1:-1")), "setdiffvec - broadcast dims - haystack-needles";
  ok tapprox(scalar($haystack->setdiffvec($haystack)), $empty), "setdiffvec - broadcast dims - haystack-haystack";
  ok tapprox(scalar($haystack->setdiffvec($empty)), $haystack), "setdiffvec - broadcast dims - haystack-empty";
  ok tapprox(scalar($empty->setdiffvec($haystack)), $empty), "setdiffvec - broadcast dims - empty-haystack";

  # setdiffvec: broadcasting
  ok tapprox(scalar($khaystack->setdiffvec($needle0)), $khaystack), "setdiffvec - broadcast dims - haystack(*k)-needle0";
  ok tapprox(scalar($khaystack->setdiffvec($needle1)), $khaystack->slice(",1:-1,")), "setdiffvec - broadcast dims - haystack(*k)-needle1";
  ok tapprox(scalar($khaystack->setdiffvec($needles)), $khaystack->slice(",1:-1,")), "setdiffvec - broadcast dims - haystack(*k)-needles";
  ok tapprox(scalar($khaystack->setdiffvec($haystack)), $kempty), "setdiffvec - broadcast dims - haystack(*k)-haystack";
  ok tapprox(scalar($khaystack->setdiffvec($empty)), $khaystack), "setdiffvec - broadcast dims - haystack(*k)-empty";
  ok tapprox(scalar($kempty->setdiffvec($haystack)), $kempty), "setdiffvec - broadcast dims - empty(*k)-haystack";
}
test_broadcast_dimensions();

## intersectvec tests as suggested by ETJ/mowhawk2
##  + see https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4
sub test_intersect_implicit_dims {
  # intersectvec: from ETJ/mowhawk2 a la https://stackoverflow.com/a/71446817/3857002
  my $toto = pdl([1,2,3], [4,5,6]);
  my $titi = pdl(1,2,3);
  my $notin = pdl(7,8,9);
  my ($c);

  ok tapprox($c=intersectvec($titi,$toto), [[1,2,3]]), 'intersectvec - implicit dims - titi&toto';
  ok tapprox($c=intersectvec($notin,$toto), zeroes(3,0)), 'intersectvec - implicit dims - notin&toto';
  ok tapprox($c=intersectvec($titi->dummy(1), $toto), [[1,2,3]]), 'intersectvec - implicit dims - titi(*1)&toto';
  ok tapprox($c=intersectvec($notin->dummy(1), $toto), zeroes(3,0)), 'intersectvec - implicit dims - notin(*1)&toto';

  my $needle0_in = pdl([1,2,3]); # 3
  my $needle0_notin = pdl([9,9,9]); # 3
  my $needle_in = $needle0_in->dummy(1);  # 3x1: [[1 2 3]]
  my $needle_notin = $needle0_notin->dummy(1); # 3x1: [[-3 -2 -1]]
  my $needles = pdl([[1,2,3],[9,9,9]]); # 3x2: $needle0_in->cat($needle0_notin)
  my $haystack = pdl([[1,2,3],[4,5,6]]); # 3x2

  sub intersect_ok {
    my ($label, $a,$b, $c_want,$nc_want,$c_sclr_want) = @_;
    my ($c, $nc) = intersectvec($a,$b);
    my $c_sclr = intersectvec($a,$b);
    ok tapprox($c, $c_want), "$label - result";
    ok tapprox($nc, $nc_want), "$label - counts";
    ok tapprox($c_sclr, $c_sclr_want), "$label - scalar";
  }

  intersect_ok('intersectvec - implicit dims - needle0_in&haystack',
	       $needle0_in, $haystack,
	       [[1,2,3]], 1, [[1,2,3]]
	      );
  intersect_ok('intersectvec - implicit dims - needle_in&haystack',
	       $needle_in, $haystack,
	       [[1,2,3]], 1, [[1,2,3]]
	      );

  intersect_ok('intersectvec - implicit dims - needle0_notin&haystack',
	       $needle0_notin, $haystack,
	       [[0,0,0]], 0, zeroes(3,0)
	      );
  intersect_ok('intersectvec - implicit dims - needle_notin&haystack',
	       $needle_notin, $haystack,
	       [[0,0,0]], 0, zeroes(3,0)
	      );

  intersect_ok('intersectvec - implicit dims - needles&haystack',
	       $needles, $haystack,
	       [[1,2,3],[0,0,0]], 1, [[1,2,3]]
	      );

  # now we want to know whether each needle is "in" one by one, not really
  # a normal intersect, so we insert a dummy in haystack in order to broadcast
  # the "nc" needs to come back as a 4x2
  my $needles8 = pdl( [[[1,2,3],[4,5,6],[8,8,8],[8,8,8]],
		       [[4,5,6],[9,9,9],[1,2,3],[9,9,9]]]); # 3x4x2

  # need to manipulate above into suitable inputs for intersect to get right output
  # + dummy dim here also ensures singleton query-vector-sets are (trivially) sorted
  my $needles8x = $needles8->slice(",*1,,"); # 3x*x4x2 # dummy of size 1 inserted in dim 1

  # haystack: no changes needed; don't need same number of dims, broadcast engine will add dummy/1s at top
  my $haystack8 = $haystack;
  my $c_want8 = [
		 [[[1,2,3]],[[4,5,6]],[[0,0,0]],[[0,0,0]]],
		 [[[4,5,6]],[[0,0,0]],[[1,2,3]],[[0,0,0]]],
		];
  my $nc_want8 = [[1,1,0,0],
		  [1,0,1,0]];

  intersect_ok('intersectvec - implicit dims - needles8x&haystack8',
	       $needles8x, $haystack8,
	       $c_want8, $nc_want8, $c_want8
	      );
}
test_intersect_implicit_dims();

## dim-checks and implicit broadcast dimensions
##  + analogous to https://github.com/moocow-the-bovine/PDL-VectorValued/issues/4
sub test_v_broadcast_dimensions {
  # data: basic
  my $empty = zeroes(0);
  my $v1_2 = pdl([1,2]);
  my $v3_4 = pdl([3,4]);
  my $v1_4 = $v1_2->cat($v3_4)->flat;

  # data: broadcasting
  my $k = 2;
  my $kempty = $empty->slice(",*$k");
  my $kv1_2 = $v1_2->slice(",*$k");
  my $kv3_4 = $v3_4->slice(",*$k");
  my $kv1_4 = $v1_4->slice(",*$k");

  #-- union_sorted
  ok tapprox(scalar($v1_2->union_sorted($v3_4)), $v1_4), "union_sorted - broadcast dims - 12+34";
  ok tapprox(scalar($v3_4->union_sorted($v1_4)), $v1_4), "union_sorted - broadcast dims - 34+1234";
  ok tapprox(scalar($empty->union_sorted($v1_4)), $v1_4), "union_sorted - broadcast dims - 0+1234";
  ok tapprox(scalar($v1_4->union_sorted($empty)), $v1_4), "union_sorted - broadcast dims - 1234+0";
  ok tapprox(scalar($empty->union_sorted($empty)), $empty), "union_sorted - broadcast dims - 0+0";
  #
  ok tapprox(scalar($kv1_2->union_sorted($v3_4)), $kv1_4), "union_sorted - broadcast dims - 12(*k)+34";
  ok tapprox(scalar($kv3_4->union_sorted($v1_4)), $kv1_4), "union_sorted - broadcast dims - 34(*k)+1234";
  ok tapprox(scalar($kempty->union_sorted($v1_4)), $kv1_4), "union_sorted - broadcast dims - 0(*k)+1234";
  ok tapprox(scalar($kv1_4->union_sorted($empty)), $kv1_4), "union_sorted - broadcast dims - 1234(*k)+0";
  ok tapprox(scalar($kempty->union_sorted($empty)), $kempty), "union_sorted - broadcast dims - 0(*k)+0";

  #-- intersect_sorted
  ok tapprox(scalar($v1_2->intersect_sorted($v3_4)), $empty), "intersect_sorted - broadcast dims - 12&34";
  ok tapprox(scalar($v3_4->intersect_sorted($v1_4)), $v3_4), "intersect_sorted - broadcast dims - 34&1234";
  ok tapprox(scalar($empty->intersect_sorted($v1_4)), $empty), "intersect_sorted - broadcast dims - 0&1234";
  ok tapprox(scalar($v1_4->intersect_sorted($empty)), $empty), "intersect_sorted - broadcast dims - 1234&0";
  ok tapprox(scalar($empty->intersect_sorted($empty)), $empty), "intersect_sorted - broadcast dims - 0&0";
  #
  ok tapprox(scalar($kv1_2->intersect_sorted($v3_4)), $kempty), "intersect_sorted - broadcast dims - 12(*k)&34";
  ok tapprox(scalar($kv3_4->intersect_sorted($v1_4)), $kv3_4), "intersect_sorted - broadcast dims - 34(*k)&1234";
  ok tapprox(scalar($kempty->intersect_sorted($v1_4)), $kempty), "intersect_sorted - broadcast dims - 0(*k)&1234";
  ok tapprox(scalar($kv1_4->intersect_sorted($empty)), $kempty), "intersect_sorted - broadcast dims - 1234(*k)&0";
  ok tapprox(scalar($kempty->intersect_sorted($empty)), $kempty), "intersect_sorted - broadcast dims - 0(*k)&0";

  #-- setdiff_sorted
  ok tapprox(scalar($v1_2->setdiff_sorted($v3_4)), $v1_2), "setdiff_sorted - broadcast dims - 12-34";
  ok tapprox(scalar($v3_4->setdiff_sorted($v1_4)), $empty), "setdiff_sorted - broadcast dims - 34-1234";
  ok tapprox(scalar($v1_4->setdiff_sorted($empty)), $v1_4), "setdiff_sorted - broadcast dims - 1234-0";
  ok tapprox(scalar($empty->setdiff_sorted($v1_4)), $empty), "setdiff_sorted - broadcast dims - 0-1234";
  ok tapprox(scalar($empty->setdiff_sorted($empty)), $empty), "setdiff_sorted - broadcast dims - 0-0";
  #
  ok tapprox(scalar($kv1_2->setdiff_sorted($v3_4)), $kv1_2), "setdiff_sorted - broadcast dims - 12(*k)-34";
  ok tapprox(scalar($kv3_4->setdiff_sorted($v1_4)), $kempty), "setdiff_sorted - broadcast dims - 34(*k)-1234";
  ok tapprox(scalar($kv1_4->setdiff_sorted($empty)), $kv1_4), "setdiff_sorted - broadcast dims - 1234(*k)-0";
  ok tapprox(scalar($kempty->setdiff_sorted($v1_4)), $kempty), "setdiff_sorted - broadcast dims - 0(*k)-1234";
  ok tapprox(scalar($kempty->setdiff_sorted($empty)), $kempty), "setdiff_sorted - broadcast dims - 0(*k)-0";
}
test_v_broadcast_dimensions();

done_testing;
