use Test::More tests => 64;
use PDL::LiteF;
use PDL::NiceSlice;
use Config;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
use Test::Exception;

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

approx(pdl(0), pdl(0), 0.01); # set eps

# $a0 = zeroes cdouble,3,5;
# $b0 = xvals $a0;
{
my $pa = xvals (cdouble,3,5) + 10 -2*  xvals (3,5)*ci;

my $pb = yvals (cdouble,3,5)+ 10 -2* yvals (3,5)*ci;

my $pc = $pa + $pb;

ok($pc->double->at(2,2) == 4+6*ci, 'pdl addition 1');
ok($pc(2,3) == cdouble (5 +ci*4), 'pdl addition 2');
#throws_ok {$pc(2,3) > (5 +ci*4); } 'pdl comparison 2';
throws_ok {
	$pc->at(3,3);
} qr/Position out of range/, 'invalid position';
}

{
my $pd = cdouble 5,6;

my $pe = $pd - 1;
ok($pe->at(0) eq '4+0i', 'pdl - scalar 1');
ok($pe->at(1) == 5, 'pdl - scalar 2');
my $pf = 1 - $pd;
ok($pf->at(0) == -4, 'scalar - pdl 1');
ok($pf->at(1) == -5, 'scalar - pdl 2');
}

# Now, test one operator from each group
# biop1 tested already
{
my $pa = pdl 0,1,2;
my $pb = pdl 1.5;

my $pc = $pa > $pb;

ok($pc->at(1) == 0, '0 not > 1.5');
ok($pc->at(2) == 1, '2 is > 1.5');
}

{
my $pa = byte pdl 0,1,3;
my $pc = $pa << 2;

ok($pc->at(0) == 0, '0 left bitshift 2 is 0');
ok($pc->at(0) != 1, '0 left bitshift 2 is 0');
ok($pc->at(1) == 4, '1 left bitshift 2 is 4');
ok($pc->at(2) == 12,'3 left bitshift 2 is 12');
}

{
my $pa = cdouble pdl 16,64,9,-1;
my $pb = sqrt($pa);

ok(ci()**2 == -1,'i squared = -1');
ok(all( approx($pb,(cdouble 4,8,3,ci()))),'sqrt of pdl(16,64,9,-1)');

# See that a is unchanged.

ok($pa->at(0) == 16, 'sqrt orig value ok');
}

{
my $pa = pdl 1,0;
my $pb = ! $pa;
ok($pb->at(0) == 0, 'elementwise not 1');
ok($pb->at(1) == 1, 'elementwise not 2');
}

{
my $pa = pdl 12,13,14,15,16,17;
my $pb = $pa % 3;

ok($pb->at(0) == 0, 'simple modulus 1');
ok($pb->at(1) == 1, 'simple modulus 2');
ok($pb->at(3) == 0, 'simple modulus 3');
# [ More modulus testing farther down! ]
}

{
# Might as well test this also
ok(all( approx((pdl 2,3),(pdl 2,3))),'approx equality 1');
ok(!all( approx((pdl 2,3),(pdl 2,4))),'approx equality 2');
}

{
# Simple function tests
my $pa = pdl(2,3);
ok(all( approx(exp($pa), pdl(7.3891,20.0855))), 'exponential');
ok(all( approx(sqrt($pa), pdl(1.4142, 1.7321))), 'sqrt makes decimal');
}

{
# And and Or

ok(all( approx(pdl(1,0,1) & pdl(1,1,0), pdl(1,0,0))), 'elementwise and');
ok(all( approx(pdl(1,0,1) | pdl(1,1,0), pdl(1,1,1))), 'elementwise or');
}

{
# atan2
ok (all( approx(atan2(pdl(1,1), pdl(1,1)), ones(2) * atan2(1,1))), 'atan2');
}

{
my $pa = sequence (3,4);
my $pb = sequence (3,4) + 1;

ok (all( approx($pa->or2($pb,0), $pa | $pb)), 'or2');
ok (all( approx($pa->and2($pb,0), $pa & $pb)), 'and2');
ok (all( approx($pb->minus($pa,0), $pb - $pa)), 'explicit minus call');
ok (all( approx($pb - $pa, ones(3,4))), 'pdl subtraction');
}

# inplace tests

{
my $pa = cdouble 1;
my $sq2 = sqrt 2; # perl sqrt

$pa->inplace->plus(1,0);  # trailing 0 is ugly swap-flag
ok(all( approx($pa, pdl 2)), 'inplace plus');
my $warning_shutup;
$warning_shutup = $warning_shutup = sqrt $pa->inplace;
ok(all( approx( $pa, pdl($sq2))), 'inplace pdl sqrt vs perl scalar sqrt');
my $pb = pdl 4;
ok(all( approx( 2, sqrt($pb->inplace))),'perl scalar vs inplace pdl sqrt');
}

{
# log10 now uses C library
# check using scalars and piddles
{
my $pa = 20+10*ci;
my $pb = log ($pa);
note "a: $pa  [ref(\$pa)='", ref($pa),"']\n";
note "b: $pb\n";
ok(exp($pb)-$pa < 1.0e-5 ,'exp of log of complex scalar');
}

{
my $pa = log10(pdl(110,23));
my $pb = log(pdl(110,23)) / log(10);
note "a: $pa\n";
note "b: $pb\n";
ok(all( approx( $pa, $pb)), 'log10 pdl');

# check inplace
ok(all( approx( cdouble(110,23)->inplace->log()/log(10), $pb)), 'inplace pdl log10');
}

}

{
my $data = ones cdouble,5;
$data+=ci();
$data &= 0;
ok(all($data == 0), 'and assign');
$data |= 1;
ok(all($data == 1), 'or assign');

ok(all($data eq $data), 'eq'); # check eq operator
}

SKIP:
{
  skip("ipow skipped without 64bit support", 1) if $Config{ivsize} < 8;
  # check ipow routine
  my $xdata = indx(0xeb * ones(8));
  my $n = sequence(indx,8);
  my $exact = indx(1,235,55225,12977875,3049800625,716703146875,168425239515625,39579931286171875);
  ok(all($exact - ipow($xdata,$n) == indx(0)), 'ipow');
}

#### Modulus checks ####

{
#test signed modulus on small numbers
# short/long/indx/longlong/float/double neg/0/pos % neg/0/pos
my $pa = pdl(-7..7);
my $pb = pdl(-3,0,3)->transpose;
my $pc = cat(pdl("-1 0 -2 " x 5),zeroes(15),pdl("2 0 1 " x 5));
ok all(short($pa) % short($pb) == short($pc)),'short modulus';
ok all(long($pa) % long($pb) ==  long($pc)), 'long modulus';
ok all(indx($pa) % indx($pb) == indx($pc)), 'indx modulus';
ok all(longlong($pa) % longlong($pb) == longlong($pc)), 'longlong modulus';
ok all(float($pa) % float($pb) == float($pc)), 'float modulus';
ok all(double($pa) % double($pb) == double($pc)), 'double modulus';
}

{
#test unsigned modulus
# byte/ushort 0/pos % 0/pos
my $pa = xvals(15);
my $pb = pdl(0,3)->transpose;
my $pc = cat(zeroes(15),pdl("0 1 2 " x 5));
ok all(byte($pa) % byte($pb)==byte($pc)), 'byte modulus';
ok all(ushort($pa) % ushort($pb)==ushort($pc)), 'ushort modulus';
}

#and for big numbers (bigger than INT_MAX=2147483647)
#basically this is exercising the (typecast)(X)/(N) in the macros
my $INT_MAX = 2147483647;

TODO: {
local $TODO = undef;
$TODO = 'Marking TODO for big modulus for 2.008 release';
require Config;
diag "\$Config{ivsize} = $Config::Config{ivsize}";
diag "\$INT_MAX = $INT_MAX = @{[ sprintf '%x', $INT_MAX ]}";
cmp_ok long($INT_MAX)%1      , '==', 0, "big long modulus: $INT_MAX % 1";
cmp_ok indx($INT_MAX*4)%2    , '==', 0, "big indx modulus: @{[$INT_MAX*4]} % 2";
cmp_ok longlong($INT_MAX*4)%2, '==', 0, "big longlong modulus: @{[$INT_MAX*4]} % 2";
#skip float intentionally here, since float($INT_MAX)!=$INT_MAX
cmp_ok double($INT_MAX*4)%2  , '==', 0, "big double modulus: @{[$INT_MAX*4]} % 2";
}

{
#and do the same for byte (unsigned char) and ushort
my $BYTE_MAX = 255;
my $USHORT_MAX = 65535;

ok byte($BYTE_MAX)%1 == 0, 'big byte modulus';
ok ushort($USHORT_MAX)%1 == 0, 'big ushort modulus';
}

SKIP:
{
  skip("your perl hasn't 64bit int support", 6) if $Config{ivsize} < 8;
  # SF bug #343 longlong constructor and display lose digits due to implicit double precision conversions
  cmp_ok longlong(10555000100001145) - longlong(10555000100001144),      '==', 1, "longlong precision/1";
  cmp_ok longlong(9000000000000000002) - longlong(9000000000000000001),  '==', 1, "longlong precision/2";
  cmp_ok longlong(-8999999999999999998) + longlong(8999999999999999999), '==', 1, "longlong precision/3";
  cmp_ok longlong(1000000000000000001) - longlong(1000000000000000000),  '==', 1, "longlong precision/4";
  cmp_ok longlong(9223372036854775807) - longlong(9223372036854775806),  '==', 1, "longlong precision/5";
  cmp_ok longlong(9223372036854775807) + longlong(-9223372036854775808), '==',-1, "longlong precision/6";
}

is(~pdl(1,2,3)              ."", '[-2 -3 -4]', 'bitwise negation');
is((pdl(1,2,3) ^ pdl(4,5,6))."", '[5 7 5]'   , 'bitwise xor'     );
