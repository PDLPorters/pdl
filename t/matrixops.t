use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use Test::Exception;
use Config;
use PDL::MatrixOps;

sub tapprox {
	my($pa,$pb,$tol) = @_;
	$tol //= 1e-14;
	all approx $pa, $pb, $tol;
}

my $tol = 1e-6;

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
    fail("got non-PDL ".explain($got)." back"), next if !UNIVERSAL::isa($got, 'PDL');
    my @got_dims = $got->dims;
    is_deeply \@got_dims, \@expected_dims, "got and expected same shape inplace=$inplace"
      or diag 'got: ', explain \@got_dims, 'expected: ', explain \@expected_dims;
    ok tapprox($got, $expected, $tol), "$label inplace=$inplace"
      or diag "got:$got\nexpected:$expected";
  }
}

{
### Check LU decomposition of a simple matrix
my $pa = pdl([1,2,3],[4,5,6],[7,1,1]);
my ($lu, $perm, $par);
lives_ok { ($lu,$perm,$par) = lu_decomp($pa); } "lu_decomp 3x3 ran OK";
is($par, -1, "lu_decomp 3x3 correct parity");
ok(all($perm == pdl(2,1,0)), "lu_decomp 3x3 correct permutation");
my $l = $lu->copy;
my $ldiag;
($ldiag = $l->diagonal(0,1)) .= 1;
my $tmp;
($tmp = $l->slice("2,1"))   .= 0;
($tmp = $l->slice("1:2,0")) .= 0;
my $u = $lu->copy;
($tmp = $u->slice("1,2"))   .= 0;
($tmp = $u->slice("0,1:2")) .= 0;
ok(tapprox($pa,matmult($l,$u)->slice(":,-1:0"),$tol), "LU = A (after depermutation)");
}

{
### Check LU decomposition of an OK singular matrix
my $pb = pdl([1,2,3],[4,5,6],[7,8,9]);
my ($lu,$perm,$par) = lu_decomp($pb);
ok(defined $lu, "lu_decomp singular matrix defined");
ok($lu->flat->abs->at(-1) < $tol, "lu_decomp singular matrix small value");
}

{
### Check inversion -- this also checks lu_backsub
my $pa = pdl([1,2,3],[4,5,6],[7,1,1]);
my $opt ={s=>1,lu=>\my @a};
my $inv_expected = pdl <<'EOF';
[
 [ 0.055555556 -0.055555556   0.16666667]
 [  -2.1111111    1.1111111  -0.33333333]
 [   1.7222222  -0.72222222   0.16666667]
]
EOF
check_inplace($pa, sub { inv($_[0], $opt) }, $inv_expected, "inv 3x3");
ok(ref ($opt->{lu}->[0]) eq 'PDL',"inverse: lu_decomp first entry is an ndarray");
ok(tapprox(matmult($inv_expected,$pa),identity(3),$tol),"matrix mult by its inverse gives identity matrix");
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
ok(tapprox(matmult($a334,$a334inv),identity(3)->dummy(2,4)), "3x3x4 inv gave correct answer");
}

{
### Check LU backsubstitution (bug #2023711 on sf.net)
my $pa = pdl([[1,2],[1,1]]); # asymmetric to see if need transpose
my ($lu,$perm,$par);
lives_ok { ($lu,$perm,$par) = lu_decomp($pa) } "lu_decomp 2x2 ran OK";
ok($par==1, "lu_decomp 2x2 correct parity");
ok(all($perm == pdl(0,1)), "lu_decomp 2x2 correct permutation");
my $bb = pdl([1,0], [3, 4]);
my $xx_expected = pdl <<'EOF';
[
 [-1  1]
 [ 5 -1]
]
EOF
check_inplace($bb, sub { lu_backsub($lu,$perm,$_[0]) }, $xx_expected, "lu_backsub");
my $got = $pa x $xx_expected->transpose;
ok(tapprox($got,$bb->transpose,$tol), "A x actually == B") or diag "got: $got";
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
ok($m1->det($opt1) == -2, "det([[1,2],[3,4]]");
ok($opt1->{lu}[0]->index2d(0,0) == 3, "set lu");
my $m2 = pdl [[2,1],[4,3]];  # det 2
ok($m2->det == 2, "det([[2,1],[3,4]]");
ok($m2->det($opt1) == -2, "correctly used wrong lu");
}

{
### Check broadcasted determinant -- simultaneous recursive det of four 4x4's
my $pa = pdl([3,4,5,6],[6,7,8,9],[9,0,7,6],[4,3,2,0]); # det=48
my $pb = pdl([1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]); # det=1
my $c = pdl([0,1,0,0],[1,0,0,0],[0,0,1,0],[0,0,0,1]); # det=-1
my $d = pdl([1,2,3,4],[5,4,3,2],[0,0,3,0],[3,0,1,6]); # det=-216
my $e = ($pa->cat($pb)) -> cat( $c->cat($d) );
my $det = $e->determinant;
ok(all($det == pdl([48,1],[-1,-216])), "broadcasted determinant");
}

{
### Check identity and stretcher matrices...
ok((identity(2)->flat == pdl(1,0,0,1))->all, "identity matrix");
ok((identity(pdl 2)->flat == pdl(1,0,0,1))->all, "identity matrix with scalar ndarray");
ok((identity(zeroes 2, 3)->flat == pdl(1,0,0,1))->all, "identity matrix with dimensioned ndarray");
my @deep_identity_dims = identity(zeroes 2, 3, 4)->dims;
is_deeply \@deep_identity_dims, [2, 2, 4], "identity matrix with multi-dimensioned ndarray" or diag 'got: ', explain \@deep_identity_dims;
ok((stretcher(pdl(2,3))->flat == pdl(2,0,0,3))->all, "stretcher 2x2");
ok((stretcher(pdl([2,3],[3,4]))->flat == pdl(2,0,0,3,3,0,0,4))->all, "stretcher 2x2x2");
}

{
### Check eigens
my $pa = pdl([3,4],[4,-3]);
### Check that eigens runs OK
my ($vec,$val);
lives_ok { ($vec,$val) = eigens $pa } "eigens runs OK";
### Check that it really returns eigenvectors
my $c = float(($pa x $vec) / $vec);
#print "c is $c\n";
ok(all($c->slice(":,0") == $c->slice(":,1")),"eigens really returns eigenvectors");
### Check that the eigenvalues are correct for this matrix
ok((float($val->slice("0")) == - float($val->slice("1")) and
	float($val->slice("0") * $val->slice("1")) == float(-25)),"eigenvalues are correct");
}

{
### Check computations on larger matrix with known eigenvalue sum.
my $m = pdl(
   [ 1.638,  2.153,  1.482,  1.695, -0.557, -2.443,  -0.71,  1.983],
   [ 2.153,  3.596,  2.461,  2.436, -0.591, -3.711, -0.493,  2.434],
   [ 1.482,  2.461,    2.5,  2.834, -0.665, -2.621,  0.248,  1.738],
   [ 1.695,  2.436,  2.834,  4.704, -0.629, -2.913,  0.576,  2.471],
   [-0.557, -0.591, -0.665, -0.629,     19,  0.896,  8.622, -0.254],
   [-2.443, -3.711, -2.621, -2.913,  0.896,  5.856,  1.357, -2.915],
   [ -0.71, -0.493,  0.248,  0.576,  8.622,  1.357,   20.8, -0.622],
   [ 1.983,  2.434,  1.738,  2.471, -0.254, -2.915, -0.622,  3.214]);
{
my $esum=0;
my ($vec,$val);
eval {
    ($vec,$val) = eigens($m);
    $esum=sprintf "%.3f", sum($val); #signature of eigenvalues
};
#print STDERR "eigensum for the 8x8: $esum\n";
ok($esum == 61.308,"eigens sum for 8x8 correct answer");
}

{
my $esum=0;
lives_ok {
    $esum = sprintf "%.3f", sum scalar eigens_sym($m);
} "eigens_sym for 8x8 ran OK";
is($esum, 61.308, 'eigens_sym sum for 8x8 correct answer');
}

{
if(0){ #fails because of bad eigenvectors
#Check an assymmetric matrix:
my $pa = pdl ([4,-1], [2,1]);
my $esum;
my ($vec,$val);
lives_ok {
    ($vec,$val) = eigens $pa;
    $esum=sprintf "%.3f", sum($val);
};
ok($esum == 5);
}
}

}

{

if(0){ #eigens for asymmetric matrices disbled
#The below matrix has complex eigenvalues
my $should_be_nan = eval { sum(scalar eigens(pdl([1,1],[-1,1]))) };
ok( ! ($should_be_nan == $should_be_nan)); #only NaN is not equal to itself
}

#check singular value decomposition for MxN matrices (M=#rows, N=#columns):

my $svd_in = pdl([3,1,2,-1],[-1,3,0,2],[-2,3,0,0],[1,3,-1,2]);

{
#2x2;
my $this_svd_in = $svd_in->slice("0:1","0:1");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all($this_svd_in==($u x $ess x $v->transpose)), "svd 2x2");
}

{
#3x3;
my $this_svd_in = $svd_in->slice("0:2","0:2");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all(approx($this_svd_in,$u x $ess x $v->transpose, 1e-8)), "svd 3x3");
}

{
#4x4;
my $this_svd_in = $svd_in;
my ($u,$s,$v) = svd($this_svd_in);
my $ess =zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all(approx($this_svd_in,($u x $ess x $v->transpose),1e-8)),"svd 4x4");
}

{
#3x2
my $this_svd_in = $svd_in->slice("0:1","0:2");
my ($u,$s,$v) = svd($this_svd_in);
my $ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0,1); #generic diagonal
ok(all(approx($this_svd_in, $u x $ess x $v->transpose,1e-8)), "svd 3x2");
}

{
#2x3
my $this_svd_in = $svd_in->slice("0:2","0:1");
my ($u,$s,$v) = svd($this_svd_in->transpose);
my $ess = zeroes($this_svd_in->dim(1),$this_svd_in->dim(1));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0..$this_svd_in->dim(1)-1); #generic diagonal
ok(all(approx($this_svd_in, $v x $ess x $u->transpose,1e-8)), "svd 2x3");
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
ok(tapprox($got,$B->transpose,$tol), "A x actually == B") or diag "got: $got";
}

done_testing;
