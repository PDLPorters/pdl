use PDL::LiteF;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless($result);
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..5\n";

$a = pdl [[ 1,  2,  3,  0],
      [ 1, -1,  2,  7],
      [ 1,  0,  0,  1]];

$b = pdl [[1, 1],
     [0, 2],
     [0, 2],
     [1, 1]];

$c = pdl [[ 1, 11],
      [ 8, 10],
      [ 2,  2]];

$res = $a x $b;

ok(1,tapprox($c,$res));

$eq = float [[1,1,1,1]];  # a 4,1-matrix ( 1 1 1 1 )

# Check collapse: output should be a 1x2...
ok(2,tapprox($eq x $b  , pdl([[2,6]]) )); # ([4x1] x [2x4] -> [1x2])

# Check dimensional exception: mismatched dims should throw an error
eval '$z = $b x $eq';  # [2x4] x [4x1] --> error (2 != 1)
ok(3, $@ ne "");

# Check automatic scalar multiplication
eval '$z = $b x 2;';

ok(4, !$@ && tapprox($z,$b * 2));

eval '$z = pdl(3) x $b;';
ok(5, !$@ && tapprox($z,$b * 3));