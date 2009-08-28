use PDL::LiteF;
use Test;
use Config;
# sub ok {
# 	my $no = shift;
# 	my $result = shift;
# 	print "not " unless $result;
# 	print "ok $no\n";
# }

sub near {
	my($a,$b,$tol) = @_;
	$tol = 1e-14 unless defined $tol;
	my $dist = abs($a - $b);
	print STDERR "Max dist: ".$dist->max."\n" if any ($dist > $tol);
	return ($dist <= $tol)->all;
}

BEGIN { plan tests => 28,
}

my $tol = 1e-14;

eval 'use PDL::MatrixOps;';
ok(!$@);


### Check LU decomposition of a simple matrix

$a = pdl([1,2,3],[4,5,6],[7,1,1]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(!$@);                                 # ran OK
ok($par==-1);                            # parity is right
ok(all($perm == pdl(2,1,0)));            # permutation is right

$l = $lu->copy; 
my $ldiag;
($ldiag = $l->diagonal(0,1)) .= 1; 
$l->slice("1:2,0") .= $l->slice("2,1") .= 0;

$u = $lu->copy; 
$u->slice("0,1:2") .= $u->slice("1,2") .= 0;

ok(near($a,matmult($l,$u)->slice(":,-1:0"),$tol)); # LU = A (after depermutation)

### Check LU decomposition of an OK singular matrix

$b = pdl([1,2,3],[4,5,6],[7,8,9]);
($lu,$perm,$par) = lu_decomp($b);

ok(defined $lu);
ok($lu->flat->abs->at(-1) < $tol);

### Check inversion -- this also checks lu_backsub

$a1 = inv($a,$opt={s=>1,lu=>\@a});
$identity = zeroes(3,3); $identity->diagonal(0,1)++;

ok(defined $a1);
ok(ref ($opt->{lu}->[0]) eq 'PDL');
ok(near(matmult($a1,$a),$identity,$tol));


### Check LU backsubstitution (bug #2023711 on sf.net)

$a = pdl([[2,1],[1,2]]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(!$@);                                 # ran OK
ok($par==1);                             # parity is right
ok(all($perm == pdl(0,1)));              # permutation is right

$bb = pdl([1,0]);
eval '$xx = lu_backsub($lu,$perm,$bb)';
ok(!$@);                                 # ran OK
my $xx_shape = pdl($xx->dims);
my $bb_shape = pdl($bb->dims);
ok(all($xx_shape == $bb_shape));        # check that soln and input have same shape
ok(near($xx,pdl([2/3, -1/3]),$tol));     # LU = A (after depermutation)


### Check attempted inversion of a singular matrix
$b2=undef; # avoid warning from compiler
eval '$b2 = inv($b,{s=>1})';
ok(!$@);
ok(!defined $b2);


### Check threaded determinant -- simultaneous recursive det of four 4x4's
$a = pdl([3,4,5,6],[6,7,8,9],[9,0,7,6],[4,3,2,0]); # det=48
$b = pdl([1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]); # det=1
$c = pdl([0,1,0,0],[1,0,0,0],[0,0,1,0],[0,0,0,1]); # det=-1
$d = pdl([1,2,3,4],[5,4,3,2],[0,0,3,0],[3,0,1,6]); # det=-216
$e = ($a->cat($b)) -> cat( $c->cat($d) );
$det = $e->determinant;
ok(all($det == pdl([48,1],[-1,-216])));

### Check identity and stretcher matrices...
ok((identity(2)->flat == pdl(1,0,0,1))->all);

ok((stretcher(pdl(2,3))->flat == pdl(2,0,0,3))->all);

ok((stretcher(pdl([2,3],[3,4]))->flat == pdl(2,0,0,3,3,0,0,4))->all);

### Check eigens
$a = pdl([3,4],[4,-3]);

### Check that eigens runs OK
eval {($vec,$val) = eigens $a};
ok(!$@);

### Check that it really returns eigenvectors
$c = float(($a x $vec) / $vec); 
ok(all($c->slice(":,0") == $c->slice(":,1")));

### Check that the eigenvalues are correct for this matrix
ok((float($val->slice("0")) == - float($val->slice("1")) and 
	float($val->slice("0") * $val->slice("1")) == float(-25)));

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
ok($esum == 61.308);

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
ok(!$@);
ok($esum == 61.308);

if(0){ #eigens for asymmetric matrices disbled
#The below matrix has complex eigenvalues
my $should_be_nan = eval { sum(scalar eigens(pdl([1,1],[-1,1]))) };
ok( ! ($should_be_nan == $should_be_nan)); #only NaN is not equal to itself
}
