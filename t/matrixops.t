use PDL::LiteF;
use Test::More tests => 39;
use Config;

sub near {
	my($a,$b,$tol) = @_;
	$tol = 1e-14 unless defined $tol;
	my $dist = abs($a - $b);
	print STDERR "Max dist: ".$dist->max."\n" if any ($dist > $tol);
	return ($dist <= $tol)->all;
}

my $tol = 1e-14;

eval 'use PDL::MatrixOps;';
ok(!$@, "use PDL::MatrixOps");


### Check LU decomposition of a simple matrix

$a = pdl([1,2,3],[4,5,6],[7,1,1]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(!$@, "lu_decomp 3x3 ran OK");
ok($par==-1, "lu_decomp 3x3 correct parity");
ok(all($perm == pdl(2,1,0)), "lu_decomp 3x3 correct permutation");

$l = $lu->copy;
my $ldiag;
($ldiag = $l->diagonal(0,1)) .= 1;
my $tmp;
($tmp = $l->slice("2,1"))   .= 0;
($tmp = $l->slice("1:2,0")) .= 0;

$u = $lu->copy;
($tmp = $u->slice("1,2"))   .= 0;
($tmp = $u->slice("0,1:2")) .= 0;

ok(near($a,matmult($l,$u)->slice(":,-1:0"),$tol),"LU = A (after depermutation)");

### Check LU decomposition of an OK singular matrix

$b = pdl([1,2,3],[4,5,6],[7,8,9]);
($lu,$perm,$par) = lu_decomp($b);

ok(defined $lu, "lu_decomp singular matrix defined");
ok($lu->flat->abs->at(-1) < $tol, "lu_decomp singular matrix small value");

### Check inversion -- this also checks lu_backsub

$a1 = inv($a,$opt={s=>1,lu=>\@a});
$identity = zeroes(3,3); ($tmp = $identity->diagonal(0,1))++;

ok(defined $a1, "3x3 inverse: defined");
ok(ref ($opt->{lu}->[0]) eq 'PDL',"inverse: lu_decomp first entry is a piddle");
ok(near(matmult($a1,$a),$identity,$tol),"matrix mult by its inverse gives identity matrix");

### Check inv() with added thread dims (simple check)
my $C22 = pdl([5,5],[5,7.5]);
my $C22inv = eval { $C22->inv };
ok(!$@, "2x2 inv ran OK");
ok(near($C22inv,pdl([0.6, -0.4], [-0.4, 0.4])), "2x2 inv gave correct answer");
my $C222 = $C22->dummy(2,2);
my $C222inv = eval { $C222->inv };
ok(!$@, "2x2 w/ dummy dims ran OK");
ok(near($C222inv,pdl([0.6, -0.4], [-0.4, 0.4])->dummy(2,2)), "2x2 w/ dummy dims correct answer");

### Check inv() for matrices with added thread dims (bug #3172882 on sf.net)
$a94 = pdl( [  1,  0,  4, -1, -1, -3,  0,  1,  0 ],
            [  4, -4, -5,  1, -5, -3, -1, -2,  0 ],
            [ -2,  2, -5, -1,  1, -3, -4,  3, -4 ],
            [ -1,  4, -4,  2,  1,  3, -3, -4, -3 ],
         );
$a334 = $a94->reshape(3,3,4);
eval '$a334inv = $a334->inv';

ok(!$@, "3x3x4 inv ran OK");
ok(near(matmult($a334,$a334inv),$identity->dummy(2,4)),"3x3x4 inv gave correct answer");

undef $a94;       # clean up variables
undef $a334;      # clean up variables
undef $a334inv;   # clean up variables



### Check LU backsubstitution (bug #2023711 on sf.net)


$a = pdl([[2,1],[1,2]]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(!$@, "lu_decomp 2x2 ran OK");
ok($par==1, "lu_decomp 2x2 correct parity");
ok(all($perm == pdl(0,1)), "lu_decomp 2x2 correct permutation");

$bb = pdl([1,0]);
eval '$xx = lu_backsub($lu,$perm,$bb)';
ok(!$@, "lu_backsub ran OK");
my $xx_shape = pdl($xx->dims);
my $bb_shape = pdl($bb->dims);
ok(all($xx_shape == $bb_shape), "lu_backsub solution and input have same shape");
ok(near($xx,pdl([2/3, -1/3]),$tol), "lu_backsub LU=A (after depermutation)");


### Check attempted inversion of a singular matrix
$b2=undef; # avoid warning from compiler
eval '$b2 = inv($b,{s=>1})';
ok(!$@, "inv of singular matrix should not barf if s=>1");
ok(!defined $b2, "inv of singular matrix undefined if s=>1");


### Check threaded determinant -- simultaneous recursive det of four 4x4's
$a = pdl([3,4,5,6],[6,7,8,9],[9,0,7,6],[4,3,2,0]); # det=48
$b = pdl([1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]); # det=1
$c = pdl([0,1,0,0],[1,0,0,0],[0,0,1,0],[0,0,0,1]); # det=-1
$d = pdl([1,2,3,4],[5,4,3,2],[0,0,3,0],[3,0,1,6]); # det=-216
$e = ($a->cat($b)) -> cat( $c->cat($d) );
$det = $e->determinant;
ok(all($det == pdl([48,1],[-1,-216])),"threaded determinant");

### Check identity and stretcher matrices...
ok((identity(2)->flat == pdl(1,0,0,1))->all, "identity matrix");

ok((stretcher(pdl(2,3))->flat == pdl(2,0,0,3))->all, "stretcher 2x2");

ok((stretcher(pdl([2,3],[3,4]))->flat == pdl(2,0,0,3,3,0,0,4))->all,"stretcher 2x2x2");

### Check eigens
$a = pdl([3,4],[4,-3]);

### Check that eigens runs OK
eval {($vec,$val) = eigens $a};
ok(!$@,"eigens runs OK");

### Check that it really returns eigenvectors
$c = float(($a x $vec) / $vec);
#print "c is $c\n";
ok(all($c->slice(":,0") == $c->slice(":,1")),"eigens really returns eigenvectors");

### Check that the eigenvalues are correct for this matrix
ok((float($val->slice("0")) == - float($val->slice("1")) and
	float($val->slice("0") * $val->slice("1")) == float(-25)),"eigenvalues are correct");

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

my $esum=0;
eval {
    ($vec,$val) = eigens($m);
    $esum=sprintf "%.3f", sum($val); #signature of eigenvalues
};
#print STDERR "eigensum for the 8x8: $esum\n";
ok($esum == 61.308,"eigens sum for 8x8 correct answer");

if(0){ #fails because of bad eigenvectors
#Check an assymmetric matrix:
$a = pdl ([4,-1], [2,1]);
eval {
    ($vec,$val) = eigens $a;
    $esum=sprintf "%.3f", sum($val);
};
ok(!$@);
ok($esum == 5);
}

$esum=0;
eval {
    $esum = sprintf "%.3f", sum scalar eigens_sym($m);
};
ok(!$@, "eigens_sym for 8x8 ran OK");
ok($esum == 61.308, "eigens_sym sum for 8x8 correct answer");

if(0){ #eigens for asymmetric matrices disbled
#The below matrix has complex eigenvalues
my $should_be_nan = eval { sum(scalar eigens(pdl([1,1],[-1,1]))) };
ok( ! ($should_be_nan == $should_be_nan)); #only NaN is not equal to itself
}

#check singular value decomposition for MxN matrices (M=#rows, N=#columns):

my ($svd_in,$this_svd_in,$u,$s,$vT,$ess);

$svd_in = pdl([3,1,2,-1],[-1,3,0,2],[-2,3,0,0],[1,3,-1,2]);

#2x2;
$this_svd_in = $svd_in->slice("0:1","0:1");
($u,$s,$v) = svd($this_svd_in);
$ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all($this_svd_in==($u x $ess x $v->transpose)), "svd 2x2");

#3x3;
$this_svd_in = $svd_in->slice("0:2","0:2");
($u,$s,$v) = svd($this_svd_in);
$ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all(approx($this_svd_in,$u x $ess x $v->transpose, 1e-8)), "svd 3x3");

#4x4;
$this_svd_in = $svd_in;
($u,$s,$v) = svd($this_svd_in);
$ess =zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->diagonal(0,1).=$s;
ok(all(approx($this_svd_in,($u x $ess x $v->transpose),1e-8)),"svd 4x4");

#3x2
$this_svd_in = $svd_in->slice("0:1","0:2");
($u,$s,$v) = svd($this_svd_in);
$ess = zeroes($this_svd_in->dim(0),$this_svd_in->dim(0));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0,1); #generic diagonal
ok(all(approx($this_svd_in, $u x $ess x $v->transpose,1e-8)), "svd 3x2");

#2x3
$this_svd_in = $svd_in->slice("0:2","0:1");
($u,$s,$v) = svd($this_svd_in->transpose);
$ess = zeroes($this_svd_in->dim(1),$this_svd_in->dim(1));
$ess->slice("$_","$_").=$s->slice("$_") foreach (0..$this_svd_in->dim(1)-1); #generic diagonal
ok(all(approx($this_svd_in, $v x $ess x $u->transpose,1e-8)), "svd 2x3");
