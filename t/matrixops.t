use PDL::LiteF;
use Test;
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

BEGIN { plan tests => 19,
	  todo => $^O =~ /win32/i ? [12] : []
}

my $tol = ($^O =~ /win32/i) ? 1e-6 : 1e-14;

eval 'use PDL::MatrixOps;';
ok(!$@);


### Check LU decomposition of a simple matrix

$a = pdl([1,2,3],[4,5,6],[7,1,1]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(!$@);                                 # ran OK
ok($par==-1);                            # parity is right
ok(all($perm == pdl(2,1,0)));            # permutation is right

$l = $lu->copy; 
$l->diagonal(0,1) .= 1; 
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
