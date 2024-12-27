use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use Test::Exception;
use Test::PDL;
use PDL::MatrixOps;

sub check_inplace {
  my ($in, $cb, $expected, $label) = @_;
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  my @expected_dims = $expected->dims;
  for my $inplace (0, 1) {
    my $in_copy = $in->copy;
    my $got;
    $inplace
      ? lives_ok { $cb->($in_copy->inplace); $got = $in_copy->copy } "$label inplace=$inplace runs"
      : lives_ok { $got = $cb->($in_copy) } "$label inplace=$inplace runs";
    is_pdl $got, $expected, "$label inplace=$inplace";
  }
}

{
### Check LU decomposition of a simple matrix
my $pa = pdl([1,2,3],[4,5,6],[7,1,1]);
my ($lu, $perm, $par);
lives_ok { ($lu,$perm,$par) = lu_decomp($pa); } "lu_decomp 3x3 ran OK";
is($par, -1, "lu_decomp 3x3 correct parity");
is_pdl $perm, pdl(2,1,0), "lu_decomp 3x3 correct permutation";
my $l = $lu->copy;
$l->diagonal(0,1) .= 1;
$l->slice("2,1")   .= 0;
$l->slice("1:2,0") .= 0;
my $u = $lu->copy;
$u->slice("1,2")   .= 0;
$u->slice("0,1:2") .= 0;
is_pdl matmult($l,$u)->slice(":,-1:0"), $pa, "LU = A (after depermutation)";
}

{
### Check LU decomposition of an OK singular matrix
my $pb = pdl([1,2,3],[4,5,6],[7,8,9]);
my ($lu,$perm,$par) = lu_decomp($pb);
is_pdl $lu, pdl('7 8 9; 0.142857 0.857142 1.714285; 0.571428 0.5 0'), "lu_decomp singular matrix";
}

{
### Check inversion -- this also checks lu_backsub
my $pa = pdl([1,2,3],[4,5,6],[7,1,1]);
my $opt ={s=>1,lu=>\my @a};
my $inv_expected = pdl '0.055555556 -0.055555556 0.16666667; -2.1111111 1.1111111 -0.33333333; 1.7222222 -0.72222222 0.16666667';
check_inplace($pa, sub { inv($_[0], $opt) }, $inv_expected, "inv 3x3");
isa_ok $opt->{lu}[0], 'PDL', "inverse: lu_decomp first entry is an ndarray";
is_pdl matmult($inv_expected,$pa),identity(3),"matrix mult by its inverse gives identity matrix";
}

{
### Check inv() with added broadcast dims (simple check)
my $C22 = pdl([5,5],[5,7.5]);
my $inv_expected = pdl([0.6, -0.4], [-0.4, 0.4]);
check_inplace($C22, sub { $_[0]->inv }, $inv_expected, "inv 2x2");
check_inplace($C22->dummy(2,2), sub { $_[0]->inv }, $inv_expected->dummy(2,2), "inv 2x2 extra dim");
}

{
### Check inv() for matrices with added broadcast dims (bug #3172882 on sf.net)
my $a334 = pdl <<'EOF';
[
 [
  [ 1  0  4]
  [-1 -1 -3]
  [ 0  1  0]
 ]
 [
  [ 4 -4 -5]
  [ 1 -5 -3]
  [-1 -2  0]
 ]
 [
  [-2  2 -5]
  [-1  1 -3]
  [-4  3 -4]
 ]
 [
  [-1  4 -4]
  [ 2  1  3]
  [-3 -4 -3]
 ]
]
EOF
my $a334inv;
lives_ok { $a334inv = $a334->inv } "3x3x4 inv ran OK";
is_pdl matmult($a334,$a334inv),identity(3)->dummy(2,4), "3x3x4 inv";
}

{
my $idc = identity(zeroes(cdouble, 2, 2));
is $idc->type, 'cdouble';
}

{
# bug in inv for native-complex - GH#403
my $p = pdl [[ 1+i(), 0], [0, 2+2*i() ] ];
my $p_inv;
lives_ok { $p_inv = $p->inv } "native-complex inv runs OK";
is_pdl matmult($p,$p_inv),identity(2)->cdouble, "native-complex inv";
}

{
### Check LU backsubstitution (bug #2023711 on sf.net)
my $pa = pdl([[1,2],[1,1]]); # asymmetric to see if need transpose
my ($lu,$perm,$par);
lives_ok { ($lu,$perm,$par) = lu_decomp($pa) } "lu_decomp 2x2 ran OK";
is $par, 1, "lu_decomp 2x2 correct parity";
is_pdl $perm, pdl(0,1), "lu_decomp 2x2 correct permutation";
my $bb = pdl([1,0], [3, 4]);
my $xx_expected = pdl '-1  1; 5 -1';
check_inplace($bb, sub { lu_backsub($lu,$perm,$_[0]) }, $xx_expected, "lu_backsub");
my $got = $pa x $xx_expected->transpose;
is_pdl $got, $bb->transpose, "A x actually == B";
}

{
my $A = identity(4) + ones(4, 4); $A->slice('2,0') .= 0;
my $B = sequence(1, 4);
my ($x) = simq($A->copy, $B->transpose, 0);
$x = $x->inplace->transpose;
is_pdl $A x $x, $B, 'simq right result';
}

{
### Check attempted inversion of a singular matrix
my $pb = pdl([1,2,3],[4,5,6],[7,8,9]);
my $b2;
lives_ok { $b2 = inv($pb,{s=>1}) } "inv of singular matrix should not barf if s=>1";
ok(!defined $b2, "inv of singular matrix undefined if s=>1");
}

{
### Check that det will save lu_decomp and reuse it
my $m1 = pdl [[1,2],[3,4]];  # det -2
my $opt1 = {lu=>undef};
is_pdl $m1->det($opt1), pdl(-2), "det([[1,2],[3,4]]";
is_pdl $opt1->{lu}[0]->index2d(0,0), pdl(3), "set lu";
my $m2 = pdl [[2,1],[4,3]];  # det 2
is_pdl $m2->det, pdl(2), "det([[2,1],[3,4]]";
is_pdl $m2->det($opt1), pdl(-2), "correctly used wrong lu";
}

{
### Check broadcasted determinant -- simultaneous recursive det of four 4x4's
my $pa = pdl([3,4,5,6],[6,7,8,9],[9,0,7,6],[4,3,2,0]); # det=48
my $pb = pdl([1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]); # det=1
my $c = pdl([0,1,0,0],[1,0,0,0],[0,0,1,0],[0,0,0,1]); # det=-1
my $d = pdl([1,2,3,4],[5,4,3,2],[0,0,3,0],[3,0,1,6]); # det=-216
my $e = ($pa->cat($pb)) -> cat( $c->cat($d) );
my $det = $e->determinant;
is_pdl $det, pdl([48,1],[-1,-216]), "broadcasted determinant";
}

{
my $m2=pdl[[-2,-2,-2],[-1,-1,-2],[0,0,-2]];
isa_ok $m2->det, 'PDL', 'det of singular always returns ndarray';
}

{
### Check identity and stretcher matrices...
is_pdl identity(2), pdl('1 0; 0 1'), "identity matrix";
is_pdl identity(pdl 2), pdl('1 0; 0 1'), "identity matrix with scalar ndarray";
is_pdl identity(zeroes 2, 3), pdl('1 0; 0 1'), "identity matrix with dimensioned ndarray";
is_pdl identity(zeroes 2, 3, 4)->shape, indx([2,2,4]), "identity matrix with multi-dimensioned ndarray";
is_pdl stretcher(pdl(2,3)), pdl('2 0;0 3'), "stretcher 2x2";
is_pdl stretcher(pdl('2 3;3 4')), pdl('[2 0;0 3][3 0;0 4]'), "stretcher 2x2x2";
is_pdl stretcher(ldouble('0.14142135623731 0.14142135623731')), ldouble('0.14142135623731 0;0 0.14142135623731'), "stretcher ldouble 2x2";
}

{
### Check eigens with symmetric
my $pa = pdl([3,4],[4,-3]);
### Check that eigens runs OK
my ($vec,$val);
lives_ok { ($vec,$val) = eigens $pa } "eigens runs OK";
is_pdl $vec, pdl('[-0.4472 0.8944; 0.8944 0.4472]'), {atol=>1e-4, test_name=>'vec'};
is_pdl $val, pdl('[-5 5]'), {atol=>1e-4, test_name=>'val'};
}

{
### Check computations on larger symmetric matrix with known eigenvalue sum.
my $m = pdl(
   [ 1.638,  2.153,  1.482,  1.695, -0.557, -2.443,  -0.71,  1.983],
   [ 2.153,  3.596,  2.461,  2.436, -0.591, -3.711, -0.493,  2.434],
   [ 1.482,  2.461,    2.5,  2.834, -0.665, -2.621,  0.248,  1.738],
   [ 1.695,  2.436,  2.834,  4.704, -0.629, -2.913,  0.576,  2.471],
   [-0.557, -0.591, -0.665, -0.629,     19,  0.896,  8.622, -0.254],
   [-2.443, -3.711, -2.621, -2.913,  0.896,  5.856,  1.357, -2.915],
   [ -0.71, -0.493,  0.248,  0.576,  8.622,  1.357,   20.8, -0.622],
   [ 1.983,  2.434,  1.738,  2.471, -0.254, -2.915, -0.622,  3.214]);
my ($vec,$val) = eigens($m);
is_pdl sum($val), pdl(61.308), {atol=>1e-3, test_name=>"eigens sum for 8x8 correct answer"};
}

{
my ($vec, $val) = eigens_sym(pdl '
1 0.97129719 0.84976463; 0.97129719 1 0.85718032; 0.84976463 0.85718032 1
');
is_pdl $vec, pdl('
-0.69643754 -0.4153763 0.58518141; 0.71722723 -0.3760106 0.58668657; -0.023661283 0.82829859 0.55978709
'), {atol=>1e-4};
is_pdl $val, pdl '0.0285787023603916 0.184737309813499 2.78668427467346';
}

{
my ($vec, $val) = eigens_sym(pdl '
6.16 6.92 6.48; 6.92 8.24 7.56; 6.48 7.56 9.44
');
is_pdl $vec, pdl('
0.75308498 -0.41356731 0.51168848; -0.6578457 -0.46138793 0.59528162; 0.010102134 0.78490971 0.6195278
'), {atol=>1e-4};
is_pdl $val, pdl '0.202065318822861 1.58175909519196 22.056173324585';
}

{
my ($vec, $val) = eigens_sym(pdl '
1 0.39340591 0.020299473 0.0708649 -0.2113158;
0.39340591 1 -0.0068287551 0.041056049 -0.16445125;
0.020299473 -0.0068287551 1 -0.12104955 -0.27936218;
0.0708649 0.041056049 -0.12104955 1 -0.19270414;
-0.2113158 -0.16445125 -0.27936218 -0.19270414 1
');
is_pdl $vec, pdl('
0.18062011 -0.7073149 -0.27969719 -0.23767039 0.57651043;
0.062941168 0.6977992 -0.36701876 -0.29014418 0.53872838;
0.56181779 0.078088453 -0.069022759 0.79253795 0.21303147;
0.43178295 0.081451098 0.81198071 -0.30588545 0.23248791;
0.67921943 0.0070585731 -0.35069916 -0.37100785 -0.52723279
'), {atol=>1e-4};
is_pdl $val, pdl '0.574989080429077 0.60359400510788 1.05055177211761 1.17390930652618 1.59695565700531';
}

{
#Check an asymmetric matrix:
my $pa = pdl ([4,-1], [2,1]);
my ($vec,$val) = eigens $pa;
is_pdl $vec, cdouble('0.707106781186548 0.447213595499958;
  0.707106781186548 0.894427190999916');
is_pdl $val, cdouble(3,2);
}

{
#The below matrix has complex eigenvalues
my ($rvec, $val) = eigens(pdl([1,1],[-1,1]));
is_pdl $rvec, pdl('[0.707i -0.707i; 0.707 0.707]'), {atol=>1e-3};
is_pdl $val, pdl('[1-i 1+i]'), {atol=>1e-3};
}

throws_ok { eigens(pdl '243 -54 0; 126 72 10; 144 -72 135') } qr/hqr2 function/;

{
#asymmetric case with complex eigenvectors
my ($rvec, $val) = eigens(my $A = pdl '1 0 0 0; 0 1 0 0; 0 0 0 -1; 0 0 1 0');
is_pdl $val, pdl('-i i 1 1');
for my $i (0..3) {
  my ($vals, $vecs) = ($val->slice($i), $rvec->slice($i));
  is_pdl $vals * $vecs, $A x $vecs;
}
}

{
#check singular value decomposition for MxN matrices (M=#rows, N=#columns):

my $svd_in = pdl([3,1,2,-1],[-1,3,0,2],[-2,3,0,0],[1,3,-1,2]);

{
#2x2;
my $this_svd_in = $svd_in->slice("0:1","0:1");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
is_pdl $this_svd_in, ($u x $ess x $v->transpose), "svd 2x2";
}

{
#3x3;
my $this_svd_in = $svd_in->slice("0:2","0:2");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
is_pdl $this_svd_in, $u x $ess x $v->transpose, "svd 3x3";
}

{
#4x4;
my $this_svd_in = $svd_in;
my ($u,$s,$v) = svd($this_svd_in);
my $ess =zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
is_pdl $this_svd_in,($u x $ess x $v->transpose),"svd 4x4";
}

{
#3x2
my $this_svd_in = $svd_in->slice("0:1","0:2");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0,1); #generic diagonal
is_pdl $this_svd_in, $u x $ess x $v->transpose, "svd 3x2";
}

{
#2x3
my $this_svd_in = $svd_in->slice("0:2","0:1");
my ($u,$s,$v) = svd($this_svd_in->transpose);
my $ess = zeroes($this_svd_in->dim(1),$this_svd_in->dim(1));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0..$this_svd_in->dim(1)-1); #generic diagonal
is_pdl $this_svd_in, $v x $ess x $u->transpose, "svd 2x3";
}

}

{
# test inspired by Luis Mochan
my $A = sequence(2, 2) + 1;
my $A1 = $A->slice(',1:0'); # interchange two rows
my $B = pdl(1,1);
my $x_expected = pdl([[-1, 1]]);
check_inplace($B, sub { lu_backsub($A->lu_decomp, $_[0]) }, $x_expected, "lu_backsub dims");
check_inplace($B, sub { lu_backsub($A1->lu_decomp, $_[0]) }, $x_expected, "lu_backsub dims 2");
my $got = $A x $x_expected->transpose;
is_pdl $got,$B->transpose, "A x actually == B";
}

{
squaretotri(my $x=sequence(3,3), my $y=zeroes(6));
is $y.'', "[0 3 4 6 7 8]", 'squaretotri with output arg given';
eval {squaretotri($x, zeroes(7))};
like $@, qr/dim has size 7/;
$y = squaretotri($x);
is $y.'', "[0 3 4 6 7 8]", 'squaretotri with no output arg given';
is tritosquare($y).'', '
[
 [0 0 0]
 [3 4 0]
 [6 7 8]
]
', 'tritosquare';
$y = squaretotri(sequence(3,3,2));
is $y.'', "
[
 [ 0  3  4  6  7  8]
 [ 9 12 13 15 16 17]
]
", 'squaretotri broadcasts right';
}

{
my $A = pdl '[1 2 3; 4 5 6; 7 8 9]';
my $up = pdl '[1 2 3; 0 5 6; 0 0 9]';
my $lo = pdl '[1 0 0; 4 5 0; 7 8 9]';
is_pdl $A->tricpy(0), $up, 'upper triangle #1';
tricpy($A, 0, my $got = null);
is_pdl $got, $up, 'upper triangle #2';
is_pdl $A->tricpy, $up, 'upper triangle #3';
is_pdl $A->tricpy(1), $lo, 'lower triangle #1';
tricpy($A, 1, $got = null);
is_pdl $got, $lo, 'lower triangle #2';
is_pdl $A->mstack($up), pdl('[1 2 3; 4 5 6; 7 8 9; 1 2 3; 0 5 6; 0 0 9]');
is_pdl sequence(2,3)->augment(sequence(3,3)+10), pdl('[0 1 10 11 12; 2 3 13 14 15; 4 5 16 17 18]');
my $B = pdl('[i 2+4i 3+5i; 0 3i 7+9i]');
is_pdl $B->t, pdl('[i 0; 2+4i 3i; 3+5i 7+9i]');
is_pdl $B->t(1), pdl('[-i 0; 2-4i -3i; 3-5i 7-9i]');
is_pdl sequence(3)->t, pdl('[0; 1; 2]');
is_pdl pdl(3)->t->shape, indx([1,1]);
}

done_testing;
