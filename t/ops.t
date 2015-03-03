use Test::More tests => 53;
use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
	return $d < 0.01;
}

# $a0 = zeroes 3,5;
# $b0 = xvals $a0;

$a = xvals zeroes 3,5;

$b = yvals zeroes 3,5;

$c = $a + $b;

ok($c->at(2,2) == 4);
ok($c->at(2,3) == 5);
eval '$c->at(3,3)';
ok($@ =~ /Position out of range/);

$d = pdl 5,6;

$e = $d - 1;
ok($e->at(0) == 4);
ok($e->at(1) == 5);
$f = 1 - $d;
ok($f->at(0) == -4);
ok($f->at(1) == -5);

# Now, test one operator from each group
# biop1 tested already

$a = pdl 0,1,2;
$b = pdl 1.5;

$c = $a > $b;

ok($c->at(1) == 0);
ok($c->at(2) == 1);

$a = byte pdl 0,1,3;
$c = $a << 2;

ok($c->at(0) == 0);
ok($c->at(1) == 4);
ok($c->at(2) == 12);


$a = pdl 16,64,9;
$b = sqrt($a);

ok(tapprox($b,(pdl 4,8,3)));

# See that a is unchanged.

ok($a->at(0) == 16);

$a = pdl 1,0;
$b = ! $a;
ok($b->at(0) == 0);
ok($b->at(1) == 1);

$a = pdl 12,13,14,15,16,17;
$b = $a % 3;

ok($b->at(0) == 0);
ok($b->at(1) == 1);
ok($b->at(3) == 0);
# [ More modulus testing farther down! ]

# Might as well test this also

ok(tapprox((pdl 2,3),(pdl 2,3)));
ok(!tapprox((pdl 2,3),(pdl 2,4)));

# Simple function tests

$a = pdl(2,3);
ok(tapprox(exp($a), pdl(7.3891,20.0855)));
ok(tapprox(sqrt($a), pdl(1.4142, 1.7321)));

# And and Or

ok(tapprox(pdl(1,0,1) & pdl(1,1,0), pdl(1,0,0)));
ok(tapprox(pdl(1,0,1) | pdl(1,1,0), pdl(1,1,1)));

# atan2
ok (tapprox(atan2(pdl(1,1), pdl(1,1)), ones(2) * atan2(1,1)));

$a = sequence (3,4);
$b = sequence (3,4) + 1;

ok (tapprox($a->or2($b,0), $a | $b));
ok (tapprox($a->and2($b,0), $a & $b));
ok (tapprox($b->minus($a,0), $b - $a));
ok (tapprox($b - $a, ones(3,4)));

# inplace tests

$a = pdl 1;
$sq2 = sqrt 2; # perl sqrt
$a->inplace->plus(1,0);  # trailing 0 is ugly swap-flag
ok(tapprox $a, pdl 2);
$warning_shutup = $warning_shutup = sqrt $a->inplace;
ok(tapprox $a, pdl($sq2));
$a = pdl 4;
ok(tapprox 2, sqrt($a->inplace));

# log10 now uses C library
# check using scalars and piddles
$a = log10(110);
$b = log(110) / log(10);
note "a: $a  [ref(\$a)='", ref($a),"']\n";
note "b: $b\n";
ok(abs($a-$b) < 1.0e-5 );
$a = log10(pdl(110,23));
$b = log(pdl(110,23)) / log(10);
note "a: $a\n";
note "b: $b\n";
ok(tapprox $a, $b );

# check inplace
ok(tapprox pdl(110,23)->inplace->log10(), $b );
$data = ones 5;
$data &= 0;
ok(all $data == 0);
$data |= 1;
ok(all $data == 1);

ok(all $data eq $data); # check eq operator


#### Modulus checks ####

#test signed modulus on small numbers
# short/long/indx/longlong/float/double neg/0/pos % neg/0/pos
$a = pdl(-7..7);
$b = pdl(-3,0,3)->transpose;
$c = cat(pdl("-1 0 -2 " x 5),zeroes(15),pdl("2 0 1 " x 5));
ok all(short($a) % short($b) == short($c));
ok all(long($a) % long($b) ==  long($c));
ok all(indx($a) % indx($b) == indx($c));
ok all(longlong($a) % longlong($b) == longlong($c));
ok all(float($a) % float($b) == float($c));
ok all(double($a) % double($b) == double($c));

#test unsigned modulus
# byte/ushort 0/pos % 0/pos
$a = xvals(15);
$b = pdl(0,3)->transpose;
$c = cat(zeroes(15),pdl("0 1 2 " x 5));
ok all(byte($a) % byte($b)==byte($c));
ok all(ushort($a) % ushort($b)==ushort($c));

#and for big numbers (bigger than INT_MAX=2147483647)
#basically this is exercising the (typecast)(X)/(N) in the macros
$INT_MAX=2147483647;

ok long($INT_MAX)%1 == 0;
ok indx($INT_MAX*4)%2 == 0;
ok longlong($INT_MAX*4)%2 == 0;
#skip float intentionally here, since float($INT_MAX)!=$INT_MAX
ok double($INT_MAX*4)%2 == 0;

#and do the same for byte (unsigned char) and ushort
$BYTE_MAX = 255;
$USHORT_MAX = 65535;

ok byte($BYTE_MAX)%1 == 0;
ok ushort($USHORT_MAX)%1 == 0;
