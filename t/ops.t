use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
	return $d < 0.01;
}

print "1..25\n";

# $a0 = zeroes 3,5;
# $b0 = xvals $a0;

$a = xvals zeroes 3,5;

$b = yvals zeroes 3,5;

$c = $a + $b;

ok(1,$c->at(2,2) == 4);
ok(2,$c->at(2,3) == 5);
eval '$c->at(3,3)';
ok(3,$@ =~ /Position out of range/);

$d = pdl 5,6;

$e = $d - 1;
ok(4,$e->at(0) == 4);
ok(5,$e->at(1) == 5);
$f = 1 - $d;
ok(6,$f->at(0) == -4);
ok(7,$f->at(1) == -5);

# Now, test one operator from each group
# biop1 tested already

$a = pdl 0,1,2;
$b = pdl 1.5;

$c = $a > $b;

ok(8,$c->at(1) == 0);
ok(9,$c->at(2) == 1);

$a = byte pdl 0,1,3;
$c = $a << 2;

ok(10,$c->at(0) == 0);
ok(11,$c->at(1) == 4);
ok(12,$c->at(2) == 12);


$a = pdl 16,64,9;
$b = sqrt($a);

ok(13,approx($b,(pdl 4,8,3)));

# See that a is unchanged.

ok(14,$a->at(0) == 16);

$a = pdl 1,0;
$b = ! $a;
ok(15,$b->at(0) == 0);
ok(16,$b->at(1) == 1);

$a = pdl 12,13,14,15,16,17;
$b = $a % 3;

ok(17,$b->at(0) == 0);
ok(18,$b->at(1) == 1);
ok(19,$b->at(3) == 0);

# Might as well test this also

ok(20,approx((pdl 2,3),(pdl 2,3)));
ok(21,!approx((pdl 2,3),(pdl 2,4)));

# Simple function tests

$a = pdl(2,3);
ok(22, approx(exp($a), pdl(7.3891,20.0855)));
ok(23, approx(sqrt($a), pdl(1.4142, 1.7321)));

# And and Or

ok(24, approx(pdl(1,0,1) & pdl(1,1,0), pdl(1,0,0)));
ok(25, approx(pdl(1,0,1) | pdl(1,1,0), pdl(1,1,1)));




