use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Config;
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
use Test::Exception;
require PDL::Core::Dev;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $can_complex_power = PDL::Core::Dev::got_complex_version('pow', 2)
  && PDL::Core::Dev::got_complex_version('exp', 1);

approx(pdl(0), pdl(0), 0.01); # set eps

{
my $pa = xvals zeroes 3,5;
my $pb = yvals zeroes 3,5;
my $pc = $pa + $pb;
ok($pc->at(2,2) == 4, 'pdl addition 1');
ok($pc->at(2,3) == 5, 'pdl addition 2');
throws_ok { $pc->at(3,3); } qr/Position\s*\d+\s*out of range/, 'invalid position';
}

{
my $pd = pdl 5,6;
my $pe = $pd - 1;
ok($pe->at(0) == 4, 'pdl - scalar 1');
ok($pe->at(1) == 5, 'pdl - scalar 2');
my $pf = 1 - $pd;
ok($pf->at(0) == -4, 'scalar - pdl 1');
ok($pf->at(1) == -5, 'scalar - pdl 2');
}

# complex versions of above
{
my @w;
local $SIG{__WARN__} = sub { push @w, @_ };
my $pa = xvals(cdouble, 3, 5)+10 - 2*xvals(3, 5)*i;
my $pb = yvals(cdouble, 3, 5)+10 - 2*yvals(3, 5)*i;
my $pc = $pa + $pb;
ok(approx(cdouble(25 - 10*i) - cdouble(25 - 10*i), 0), 'pdl complex subtraction');
ok(approx($pc->double->at(2,2), 24), 'pdl complex addition 1');
is $pc->at(2,3), '25-10i', 'at stringifies complex';
ok(approx($pc->slice([2], [3]), cdouble(25 - 10*i)), 'pdl complex addition 2');
throws_ok { $pc->at(3,3); } qr/Position\s*\d+\s*out of range/, 'invalid position';
is_deeply \@w, [], 'no warnings' or diag explain \@w;
}

{
my @w;
local $SIG{__WARN__} = sub { push @w, @_ };
my $pd = cdouble 5,6;
my $pe = $pd - 1;
is($pe->at(0), '4', 'pdl - scalar 1');
is($pe->at(1), '5', 'pdl - scalar 2');
my $pf = 1 - $pd;
my $got = $pf->at(0);
is($got, '-4', 'scalar - pdl 1') or diag explain $got;
is($pf->at(1), '-5', 'scalar - pdl 2');
is_deeply \@w, [], 'no warnings' or diag explain \@w;
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
ok($pc->at(1) == 4, '1 left bitshift 2 is 4');
ok($pc->at(2) == 12,'3 left bitshift 2 is 12');
}

{
my $pa = pdl 16,64,9;
my $pb = sqrt($pa);
ok(all( approx($pb,(pdl 4,8,3))),'sqrt of pdl(16,64,9)');
# See that a is unchanged.
ok($pa->at(0) == 16, 'sqrt orig value ok');
# complex version
$pa = cdouble pdl 16,64,9,-1;
if ($can_complex_power) {
  $pb = sqrt($pa);
  my $got = i()**2;
  ok(approx($got, -1),'i squared = -1') or diag "got=$got";
  ok(all( approx($pb,(cdouble 4,8,3,i()))),'sqrt of pdl(16,64,9,-1)');
}
is $pa->at(0), '16', 'sqrt orig value ok';
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
ok (all( approx($pa->or2($pb), $pa | $pb)), 'or2');
ok (all( approx($pa->and2($pb), $pa & $pb)), 'and2');
ok (all( approx($pb->minus($pa), $pb - $pa)), 'explicit minus call');
ok (all( approx($pb - $pa, ones(3,4))), 'pdl subtraction');
}

# inplace tests

{
my $pa = pdl 1;
my $sq2 = sqrt 2; # perl sqrt
$pa->inplace->plus(1);
ok(all( approx($pa, pdl 2)), 'inplace plus');
my $warning_shutup = sqrt $pa->inplace;
ok(all( approx( $pa, pdl($sq2))), 'inplace pdl sqrt vs perl scalar sqrt');
my $pb = pdl 4;
ok(all( approx( 2, sqrt($pb->inplace))),'perl scalar vs inplace pdl sqrt');
}

{
# log10 now uses C library
# check using scalars and ndarrays
{
my $pa = log10(110);
my $pb = log(110) / log(10);
note "a: $pa  [ref(\$pa)='", ref($pa),"']\n";
note "b: $pb\n";
ok(abs($pa-$pb) < 1.0e-5, 'log10 scalar');
if ($can_complex_power) {
  $pa = 20+10*i;
  $pb = log($pa);
  my $got = exp($pb);
  ok(abs($got-$pa) < 1.0e-4,'exp of log of complex scalar') or diag "pb=$pb, got=$got, expected=$pa";
}
my $y = sequence(5,4)+2;  # Create PDL
is log(float($y))->type, 'float';
}

{
my $pa = log10(pdl(110,23));
my $pb = log(pdl(110,23)) / log(10);
note "a: $pa\n";
note "b: $pb\n";
ok(all( approx( $pa, $pb)), 'log10 pdl');
# check inplace
ok(all( approx( pdl(110,23)->inplace->log10(), $pb)), 'inplace pdl log10');
if ($can_complex_power) {
  ok(all( approx( cdouble(110,23)->inplace->log()/log(10), $pb)), 'complex inplace pdl log10');
}
}

}

{
my $data = ones 5;
$data &= 0;
ok(all($data == 0), 'and assign');
$data |= 1;
ok(all($data == 1), 'or assign');
ok(all($data eq $data), 'eq'); # check eq operator
$data = ones cdouble, 5;
$data+=i();
$data &= 0;
ok(all($data == 0), 'and assign complex');
}

# check ipow routine
my $xdata = longlong(0xeb * ones(8));
my $n = sequence(longlong,8);
is $n->type, 'longlong', 'sequence with specified type has that type';
my $exact = longlong(1,235,55225,12977875,3049800625,716703146875,168425239515625,39579931286171875);
my $got = ipow($xdata,$n);
ok(all($exact - $got == longlong(0)), 'ipow') or diag "got=$got\nexpected=$exact";

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

# Check badflag propagation with .= (Ops::assgn) sf.net bug 3543056
$a = sequence(10);
$b = sequence(5);
$b->inplace->setvaltobad(3);
$a->slice('0:4') .= $b;
$a->badflag(1);
$a->check_badflag();
ok($a->badflag == 1 && $a->nbad == 1, 'badflag propagation with .=');

done_testing;
