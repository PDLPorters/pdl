use PDL::LiteF;
sub ok {
	my $no = shift;
	my $result = shift;
	print "not " unless $result;
	print "ok $no\n";
}

sub near {
	my($a,$b) = @_;
	((( abs($a - $b) > 1e-14 ) -> sum ) == 0 );
}

print "1..16\n";
eval 'use PDL::MatrixOps;';
ok(1,!$@);


### Check LU decomposition of a simple matrix

$a = pdl([1,2,3],[4,5,6],[7,1,1]);
eval '($lu,$perm,$par) = lu_decomp($a)';

ok(2,!$@);                                 # ran OK
ok(3,$par==-1);                            # parity is right
ok(4,all($perm == pdl(2,1,0)));            # permutation is right

$l = $lu->copy; 
$l->diagonal(0,1) .= 1; 
$l->slice("1:2,0") .= $l->slice("2,1") .= 0;

$u = $lu->copy; 
$u->slice("0,1:2") .= $u->slice("1,2") .= 0;

ok(5,near($a,matmult($l,$u)->slice(":,-1:0"))); # LU = A (after depermutation)

### Check LU decomposition of an OK singular matrix

$b = pdl([1,2,3],[4,5,6],[7,8,9]);
($lu,$perm,$par) = lu_decomp($b);

ok(6,defined $lu);
ok(7,$lu->flat->at(-1)==0);

### Check inversion -- this also checks lu_backsub

$a1 = inv($a,$opt={s=>1,lu=>\@a});
$identity = zeroes(3,3); $identity->diagonal(0,1)++;

ok(8,defined $a1);
ok(9,ref ($opt->{lu}->[0]) eq 'PDL');
ok(10,near(matmult($a1,$a),$identity));

### Check attempted inversion of a singular matrix
$b2=undef; # avoid warning from compiler
eval '$b2 = inv($b,{s=>1})';
ok(11,!$@);
ok(12,!defined $b2);


### Check threaded determinant -- simultaneous recursive det of four 4x4's
$a = pdl([3,4,5,6],[6,7,8,9],[9,0,7,6],[4,3,2,0]); # det=48
$b = pdl([1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]); # det=1
$c = pdl([0,1,0,0],[1,0,0,0],[0,0,1,0],[0,0,0,1]); # det=-1
$d = pdl([1,2,3,4],[5,4,3,2],[0,0,3,0],[3,0,1,6]); # det=-216
$e = ($a->cat($b)) -> cat( $c->cat($d) );
$det = $e->determinant;
ok(13,all($det == pdl([48,1],[-1,-216])));

### Check identity and stretcher matrices...
ok(14, (identity(2)->flat == pdl(1,0,0,1))->all);

ok(15, (stretcher(pdl(2,3))->flat == pdl(2,0,0,3))->all);

ok(16, (stretcher(pdl([2,3],[3,4]))->flat == pdl(2,0,0,3,3,0,0,4))->all);

