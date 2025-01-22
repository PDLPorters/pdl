use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use Config;
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
use Test::Exception;
use Test::PDL;
require PDL::Core::Dev;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $can_complex_power = PDL::Core::Dev::got_complex_version('pow', 2)
  && PDL::Core::Dev::got_complex_version('exp', 1);

{
my ($pa, $pb, $pc) = (xvals(3,5), yvals(3,5));
for (
  [$pa,$pb,\$pc, sub { ${$_[2]} = $_[0] - $_[1] }, [1,-1]],
  [$pa,$pb,\$pc, sub { ${$_[2]} = PDL::minus($_[0], $_[1]) }, [1,-1]],
  [$pa,$pb,\$pc, sub { ${$_[2]} = PDL::minus($_[0], $_[1], 0) }, [1,-1]],
  [$pa,$pb,\$pc, sub { ${$_[2]} = PDL::minus($_[0], $_[1], 1) }, [-1,1]],
  [$pa,$pb,\$pc, sub { PDL::minus($_[0], $_[1], ${$_[2]}, 0) }, [1,-1]],
  [$pa,$pb,\$pc, sub { PDL::minus($_[0], $_[1], ${$_[2]}, 1) }, [-1,1]],
) {
  my ($in1, $in2, $outref, $sub, $exp) = @$_;
  $sub->($in1, $in2, $outref);
  ok($$outref->at(2,1) == $exp->[0], 'pdl subtraction 1');
  ok($$outref->at(2,3) == $exp->[1], 'pdl subtraction 2');
  throws_ok { $$outref->at(3,3); } qr/Position.*out of range/, 'invalid position';
}
}

is_pdl PDL::power(10,2), double(100), 'floating point op defaults to double';

{
my $pd = pdl 5,6;
is_pdl $pd - 1, pdl(4,5);
is_pdl 1 - $pd, pdl(-4,-5);
}

# complex versions of above
{
my @w;
local $SIG{__WARN__} = sub { push @w, @_ };
my $pa = xvals(cdouble, 3, 5)+10 - 2*xvals(3, 5)*i;
my $pb = yvals(cdouble, 3, 5)+10 - 2*yvals(3, 5)*i;
my $pc = $pa + $pb;
is_pdl cdouble(25 - 10*i) - cdouble(25 - 10*i), cdouble(0), 'pdl complex subtraction';
ok(approx($pc->double->at(2,2), 24), 'pdl complex addition 1');
is $pc->at(2,3), '25-10i', 'at stringifies complex';
ok(approx($pc->slice([2], [3]), cdouble(25 - 10*i)), 'pdl complex addition 2');
throws_ok { $pc->at(3,3); } qr/Position.*out of range/, 'invalid position';
is_deeply \@w, [], 'no warnings' or diag explain \@w;
}

{
my @w;
local $SIG{__WARN__} = sub { push @w, @_ };
my $pd = cdouble 5,6;
is_pdl $pd - 1, cdouble(4,5);
is_pdl 1 - $pd, cdouble(-4,-5);
is_deeply \@w, [], 'no warnings' or diag explain \@w;
}

# Now, test one operator from each group
# biop1 tested already
is_pdl pdl(0,1,2) > pdl(1.5), pdl(0,0,1);

{
my $pa = byte 0,1,3;
my $pc = $pa << 2;
is_pdl $pc, byte(0,4,12), 'left bitshift 2';
}

{
my $pa = pdl(16,64,9);
my $pb = sqrt($pa);
is_pdl $pb, pdl(4,8,3), 'sqrt of pdl(16,64,9)';
is_pdl $pa, pdl(16,64,9), 'a is unchanged';
# complex version
if ($can_complex_power) {
  $pa = cdouble 16,64,9,-1;
  $pb = sqrt($pa);
  is_pdl $pb,cdouble(4,8,3,i()), 'sqrt of pdl(16,64,9,-1)';
  is_pdl $pa, cdouble(16,64,9,-1), 'sqrt orig value ok';
  is_pdl i()**2, cdouble(-1), 'i squared = -1';
}
}

{
  is_pdl(r2C(long(1)), cdouble(1), "r2C of long");
  is_pdl(r2C(longlong(1)), cdouble(1), "r2C of longlong");
  is_pdl(r2C(float(1)), cfloat(1), "r2C of float");
  is_pdl(r2C(double(1)), cdouble(1), "r2C of double");
  is_pdl(r2C(ldouble(1)), cldouble(1), "r2C of ldouble");
  is_pdl(r2C(cfloat(1)), cfloat(1), "r2C of cfloat");
  is_pdl(r2C(cdouble(1)), cdouble(1), "r2C of cdouble");
  is_pdl(r2C(cldouble(1)), cldouble(1), "r2C of cldouble");
}

is_pdl !pdl(1,0), pdl(0,1);

is_pdl pdl(12,13,14,15,16,17) % 3, pdl(0,1,2,0,1,2);

# Might as well test this also
ok(all( approx((pdl 2,3),(pdl 2,3))),'approx equality 1');
ok(!all( approx((pdl 2,3),(pdl 2,4))),'approx equality 2');

{
# Simple function tests
my $pa = pdl(2,3);
is_pdl exp($pa), pdl(7.3891,20.0855), {atol=>1e-2, test_name=>'exp'};
is_pdl sqrt($pa), pdl(1.4142, 1.7321), {atol=>1e-2, test_name=>'sqrt'};
}

{
my @w;
local $SIG{__WARN__} = sub { push @w, @_ };
# And and Or
is_pdl pdl(1,0,1) & pdl(1,1,0), longlong(1,0,0), 'elementwise and';
is_pdl pdl(1,0,1) | pdl(1,1,0), longlong(1,1,1), 'elementwise or';
is_deeply \@w, [], 'no warnings' or diag explain \@w;
}

# atan2
is_pdl atan2(pdl(1,1), pdl(1,1)), ones(2) * atan2(1,1), 'atan2';
is_pdl PDL::atan2(pdl(1,1), pdl(1,1)), ones(2) * PDL::atan2(1,1), 'atan2';

{
my $pa = sequence (3,4);
my $pb = sequence (3,4) + 1;
is_pdl $pa->or2($pb), $pa | $pb, 'or2';
is_pdl $pa->and2($pb), $pa & $pb, 'and2';
is_pdl $pb->minus($pa), $pb - $pa, 'explicit minus call';
is_pdl $pb - $pa, ones(3,4), 'pdl subtraction';
}

# inplace tests

{
my $pa = pdl 1;
my $sq2 = sqrt 2; # perl sqrt
$pa->inplace->plus(1);
is_pdl $pa, pdl(2), 'inplace plus';
my $warning_shutup = sqrt $pa->inplace;
is_pdl $pa, pdl($sq2), 'inplace pdl sqrt vs perl scalar sqrt';
my $pb = pdl 4;
is_pdl sqrt($pb->inplace), pdl(2), 'perl scalar vs inplace pdl sqrt';
$pa .= 1;
eval {(my $tmp = $pa->inplace) += 1};
is $@, '', 'inplace += worked';
is_pdl $pa, pdl(2), 'inplace += right value after';
}

eval { my $res = pdl(3) + undef };
like $@, qr/given undef/, 'error on overloaded op with undef arg';
eval { (my $t = pdl(3)) += undef };
like $@, qr/given undef/, 'error on overloaded op= with undef arg';

{
# log10 now uses C library
# check using scalars and ndarrays
{
my $pa = log10(110);
my $pb = log(110) / log(10);
ok(abs($pa-$pb) < 1.0e-5, 'log10 scalar');
if ($can_complex_power) {
  $pa = 20+10*i;
  $pb = log($pa);
  my $got = exp($pb);
  is_pdl $got, $pa, {atol=>1.0e-4, test_name=>'exp of log of complex scalar'};
}
my $y = sequence(5,4)+2;  # Create PDL
is log(float($y))->type, 'float';
}

{
my $pa = log10(pdl(110,23));
my $pb = log(pdl(110,23)) / log(10);
is_pdl $pa, $pb, 'log10 pdl';
log10(pdl(110,23), my $pc=null);
is_pdl $pc, $pb, '2-arg log10 pdl';
# check inplace
is_pdl pdl(110,23)->inplace->log10(), $pb, 'inplace pdl log10';
if ($can_complex_power) {
  is_pdl cdouble(110,23)->inplace->log/log(10), cdouble($pb), 'complex inplace pdl log10';
}
}
}

{
my $data = ones 5;
$data &= 0;
is_pdl $data, zeroes(5), 'and assign';
$data |= 1;
is_pdl $data, ones(5), 'or assign';
is_pdl $data eq $data, ones(5), 'eq';
$data = ones cdouble, 5;
$data+=i();
$data &= 0;
is_pdl $data, zeroes(cdouble, 5), 'and assign complex';
}

#### Modulus checks ####

{
#test signed modulus on small numbers
# short/long/indx/longlong/float/double neg/0/pos % neg/0/pos
my $pa = pdl(-7..7);
my $pb = pdl(-3,0,3)->transpose;
my $pc = cat(pdl("-1 0 -2 " x 5),zeroes(15),pdl("2 0 1 " x 5));
is_pdl short($pa) % short($pb), short($pc),'short modulus';
is_pdl long($pa) % long($pb),  long($pc), 'long modulus';
is_pdl indx($pa) % indx($pb), indx($pc), 'indx modulus';
is_pdl longlong($pa) % longlong($pb), longlong($pc), 'longlong modulus'
  if $Config{ivsize} >= 8;
is_pdl float($pa) % float($pb), float($pc), 'float modulus';
is_pdl double($pa) % double($pb), double($pc), 'double modulus';
}

{
#test unsigned modulus
# byte/ushort 0/pos % 0/pos
my $pa = xvals(15);
my $pb = pdl(0,3)->transpose;
my $pc = cat(zeroes(15),pdl("0 1 2 " x 5));
is_pdl byte($pa) % byte($pb), byte($pc), 'byte modulus';
is_pdl ushort($pa) % ushort($pb), ushort($pc), 'ushort modulus';
}

{
#and do the same for byte (unsigned char) and ushort
my $BYTE_MAX = 255;
my $USHORT_MAX = 65535;

is_pdl byte($BYTE_MAX)%1, byte(0), 'big byte modulus';
is_pdl ushort($USHORT_MAX)%1, ushort(0), 'big ushort modulus';
}

SKIP:
{
  skip("your perl hasn't 64bit int support", 12) if $Config{ivsize} < 8;
  # SF bug #343 longlong constructor and display lose digits due to implicit double precision conversions
  cmp_ok longlong(10555000100001145) - longlong(10555000100001144),      '==', 1, "longlong precision/1";
  cmp_ok longlong(9000000000000000002) - longlong(9000000000000000001),  '==', 1, "longlong precision/2";
  cmp_ok longlong(-8999999999999999998) + longlong(8999999999999999999), '==', 1, "longlong precision/3";
  cmp_ok longlong(1000000000000000001) - longlong(1000000000000000000),  '==', 1, "longlong precision/4";
  cmp_ok longlong(9223372036854775807) - longlong(9223372036854775806),  '==', 1, "longlong precision/5";
  cmp_ok longlong(9223372036854775807) + longlong(-9223372036854775808), '==',-1, "longlong precision/6";
  # check ipow routine
  my $xdata = longlong(0xeb * ones(8));
  my $n = sequence(longlong,8);
  is $n->type, 'longlong', 'sequence with specified type has that type';
  my $exact = longlong(1,235,55225,12977875,3049800625,716703146875,168425239515625,39579931286171875);
  is_pdl ipow($xdata,$n), $exact, 'ipow';
  #and for big numbers (bigger than INT_MAX=2147483647)
  my $INT_MAX = 2147483647;
  cmp_ok long($INT_MAX)%1      , '==', 0, "big long modulus: $INT_MAX % 1";
  if ($Config{ptrsize} > 4) {
    cmp_ok indx($INT_MAX*4)%2    , '==', 0, "big indx modulus: @{[$INT_MAX*4]} % 2";
  }
  cmp_ok longlong($INT_MAX*4)%2, '==', 0, "big longlong modulus: @{[$INT_MAX*4]} % 2";
  cmp_ok ulonglong($INT_MAX*4)%2, '==', 0, "big ulonglong modulus: @{[$INT_MAX*4]} % 2";
  #skip float intentionally here, since float($INT_MAX)!=$INT_MAX
  cmp_ok double($INT_MAX*4)%2  , '==', 0, "big double modulus: @{[$INT_MAX*4]} % 2";

  my $u = pdl(ulonglong, [0,1]);
  my $compl = ~$u;
  is "$compl", '[18446744073709551615 18446744073709551614]', 'ULL get stringified right';
}

is_pdl ~pdl(1,2,3), longlong('[-2 -3 -4]'), 'bitwise negation';
is_pdl pdl(1,2,3) ^ pdl(4,5,6), longlong('[5 7 5]'), 'bitwise xor';

{
my $startgood = sequence(10);
$startgood->slice('0:4') .= pdl('0 1 2 BAD 4');
is_pdl $startgood, pdl('0 1 2 BAD 4 5 6 7 8 9'), 'now badflag true';
}

is_deeply [(zeroes(1,1,0) & zeroes(1,1,0))->dims], [1,1,0]; # used to segfault

done_testing;
