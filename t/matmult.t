use PDL::LiteF;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..2\n";

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

print $c;
print $res;

ok(1,tapprox($c,$res));

$equiv = float [[1,1,1,1]];  # a 4,1-matrix ( 1 1 1 1 )
print $equiv;
ok(2,tapprox($equiv x $b,float(1) x $b)); # check promotion
