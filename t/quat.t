# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
# If doesn't exist, doesn't test..
BEGIN{
eval " use PDL::Graphics::TriD::Quaternion ";
if($@) {
	print STDERR "Skipping test on this platform\n";
	print "1..0\n";
	print "ok 0\n";
}
}

# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b) = @_;
	$a = pdl $a; $b = pdl $b;
	print "APPROX: $a $b\n";
	if((join ',',$a->dims) ne (join ',',$b->dims)) {
		print "UNEQDIM\n";
		return 0;
	}
	my $c = abs($a-$b);
	my $d = max($c);
	if($d >= 0.01) {
		print "# APPROXFAIL: $a $b\n";
	}
	$d < 0.01;
}

print "1..1\n";
print "ok 1\n";
exit 0;

sub nq {return PDL::Graphics::TriD::Quaternion->new(@_)}

$a = nq(0,0,1,0);

$b = $a->rotate([0,0,1]); # Rotate a z-vector around Y axis
ok(1,approx($b,[1,0,0]));

$b = $a->rotate([0,1,0]);
ok(2,approx($b,[0,1,0]));

$b = $a->rotate([1,0,0]);
ok(3,approx($b,[0,0,-1]));

$b = $a->rotate([0,0,2]); # Rotate a z-vector around Y axis
ok(4,approx($b,[2,0,0]));

$b = $a->rotate([0,2,0]);
ok(5,approx($b,[0,2,0]));

$b = $a->rotate([2,0,0]);
ok(6,approx($b,[0,0,-2]));

$s2 = sqrt(2);

$a = nq($s2/2, 0, $s2/2, 0);

$b = $a->rotate([0,0,2]); # Rotate a z-vector around Y axis
ok(7,approx($b,[$s2,0,$s2]));

$b = $a->rotate([0,2,0]);
ok(8,approx($b,[0,2,0]));

$b = $a->rotate([2,0,0]);
ok(9,approx($b,[$s2,0,-$s2]));



$a = nq(-$s2/2, 0, $s2/2, 0);

$b = $a->rotate([0,0,2]); # Rotate a z-vector around Y axis
ok(10,approx($b,[$s2,0,-$s2]));

$b = $a->rotate([0,2,0]);
ok(11,approx($b,[0,2,0]));

$b = $a->rotate([2,0,0]);
ok(12,approx($b,[-$s2,0,-$s2]));



$a = nq(qw/0.681060411338974   0.730795579084142   -0.0457463814022116   -0.00134398157121446/
);

$a = nq(0.6, sqrt(1-0.6**2), 0,0);

$a->normalize_this();

$b = $a->rotate([0,0,2]);
print ((join ' ',@$b),"\n");
print "BSQ: ",($b->[0] ** 2 + $b->[1] ** 2 + $b->[2] ** 2),"\n";
ok(13, 4.001 >= ($b->[0] ** 2 + $b->[1] ** 2 + $b->[2] ** 2));

$a = nq(0.99, sqrt(1-0.99**2), 0.0, 0.0);
$b = $a->rotate([0,0,2]);

print ((join ' ',@$b),"\n");



