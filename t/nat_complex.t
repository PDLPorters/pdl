use strict;
use warnings;
use PDL::LiteF;
use PDL::Config;
use PDL::Core::Dev;
use PDL::Types qw(ppdefs ppdefs_complex ppdefs_all);

use Test::More;

sub tapprox {
        my($x,$y) = @_;
        my $c = abs($x-$y);
        my $d = max($c);
        $d < 0.0001;
}

is_deeply [ ppdefs() ], [qw(A B S U L K N P Q F D E)];
is_deeply [ ppdefs_complex() ], [qw(G C H)];
is_deeply [ ppdefs_all() ], [qw(A B S U L K N P Q F D E G C H)];

my $ref = pdl([[-2,1],[-3,1]]);
my $ref2 = squeeze(czip($ref->slice("0,"), $ref->slice("1,")));
my $x = i() -pdl (-2, -3);

is($x->type, 'cdouble', 'type promotion i - ndarray');
ok(tapprox($x->im,$ref->slice("1,:")), 'value from i - ndarray');
ok !$x->type->real, 'complex type not real';
ok double->real, 'real type is real';
ok !$x->sumover->type->real, 'sumover type=complex';

$x = cdouble(2,3);
$x-=i2C(3);
is type($x), 'cdouble', 'type promotion ndarray - i';
is $x->re->type, 'double', 'real real part';
my $y=cfloat($x);
is type($y), 'cfloat', 'type conversion to cfloat';
is $y->re->type, 'float', 'real real part';
ok(tapprox($x->im,$ref->slice("0,1")), 'value from ndarray - i') or diag 'got: ', $x->im;
is zeroes($_->[0], 2)->r2C->type, $_->[1], "r2C $_->[0] -> $_->[1]"
  for [byte, 'cdouble'], [long, 'cdouble'],
    [float, 'cfloat'], [cfloat, 'cfloat'],
    [double, 'cdouble'], [cdouble, 'cdouble'];
my $got_double = double(-1, 2);
my $got_r2C = $got_double->r2C;
is ''.$got_r2C->re, ''.$got_double, 're(r2C) identical to orig';

my $got = r2C(1);
is $got, 1, 'can give Perl numbers to r2C';
ok !$got->type->real, 'complex type';

$got = i2C(1);
is $got, i(), 'can give Perl numbers to i2C';
ok !$got->type->real, 'complex type';

ok !i(2, 3)->type->real, 'i(2, 3) returns complex type';

for (float, double, ldouble, cfloat, cdouble, cldouble) {
  my $got = pdl $_, '[0 BAD]';
  my $bv = $got->badvalue;
  my $obv = $got->orig_badvalue;
  is $got.'', '[0 BAD]', "$_ bad"
    or diag "bv=$bv, obv=$obv: ", explain [$bv, $obv];
  is $got->isbad.'', '[0 1]', "$_ isbad";
  # this captures a failure in IO/Flexraw/t/iotypes.t
  eval { ok $bv == $obv, 'can equality-check badvalue and orig_badvalue' };
  is $@, '', 'no error from ==' or diag explain [$bv, $obv];
}

{
# dataflow from complex to real
my $ar = $x->re;
$ar++;
ok(tapprox($x->re, -$ref->slice("0,")->squeeze + 1), 'complex to real dataflow') or diag "got=".$x->re, "expected=".(-$ref->slice("0,")->squeeze + 1);
my $ai = $x->im;
$x+=i;
my $expected = pdl(-2, -2);
ok(tapprox($x->im, $expected), 'dataflow after conversion')
  or diag "got=".$x->im, "\nexpected=$expected";
$ai++;
$expected++;
ok(tapprox($x->im, $expected), 'dataflow after change ->im')
  or diag "got=".$x->im, "\nexpected=$expected";
}

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = $ref2->copy;
my $a=abs($x);
my $p=carg($x)->double; # force to double to avoid glibc bug 18594

$y = czip($a*cos($p), $a*sin($p));
ok(tapprox($x-$y, 0.), 'check re/im and mag/ang equivalence')
  or diag "For ($x), got: ($y) from a=($a) p=($p) cos(p)=(", cos($p), ") sin(p)=(", sin($p), ")";

# Catan, Csinh, Ccosh, Catanh, Croots

my $cabs = sqrt($x->re**2+$x->im**2);

ok(abs($x)->type->real, 'Cabs type real');
ok(tapprox(abs $x, $cabs), 'Cabs value') or diag "got: (@{[abs $x]}), expected ($cabs)";
ok(tapprox(abs2 $x, $cabs**2), 'Cabs2 value') or diag "got: (@{[abs2 $x]}), expected (", $cabs**2, ")";
ok(carg($x)->type->real, 'Carg type real');
ok(tapprox(carg($x), atan2($x->im, $x->re)), 'Carg value');

{
# Check cat'ing
$y = $x->re->copy + 1;
my $bigArray = $x->cat($y);
my $sum = $bigArray->sum;
my $cz = czip(8, -2);
my $abs = abs($sum + $cz);
ok(all($abs < .0001), 'check cat for complex') or diag "got:$abs from cat(x=$x y=$y): $bigArray";
}

if (PDL::Core::Dev::got_complex_version('pow', 2)) {
  ok(tapprox($x**2, $x * $x), '** op complex')
    or diag "For ($x), got: ", $x**2, ", expected: ", $x * $x;
  ok(tapprox($x->pow(2), $x * $x), 'complex pow')
    or diag "Got: ", $x->pow(2), ", expected: ", $x * $x;
  ok(tapprox($x->power(2, 0), $x * $x), 'complex power')
    or diag "Got: ", $x->power(2, 0), ", expected: ", $x * $x;
  my $z = pdl(0) + i()*pdl(0);
  $z **= 2;
  ok(tapprox($z, i2C(0)), 'check that 0 +0i exponentiates correctly'); # Wasn't always so.
  my $r = r2C(-10);
  $r **= 2;
  ok(tapprox($r, r2C(100)),
    'check that imaginary part is exactly zero') # Wasn't always so
    or diag "got: ", $r;
}

my $asin_2 = PDL::asin(2)."";
my $nan_re = qr/nan|ind/i;
like $asin_2, $nan_re, 'perl scalar 2 treated as real';
$asin_2 = PDL::asin(2.0)."";
like $asin_2, $nan_re, 'perl scalar 2.0 treated as real';
$asin_2 = PDL::asin(byte 2)."";
like $asin_2, $nan_re, 'real byte treated as real';
$asin_2 = PDL::asin(double 2)."";
like $asin_2, $nan_re, 'real double treated as real';
$asin_2 = PDL::asin(pdl 2)."";
like $asin_2, $nan_re, 'pdl(2) treated as real';
if (PDL::Core::Dev::got_complex_version('asin', 1)) {
  my $c_asin_2 = PDL::asin(cdouble(2))."";
  unlike $c_asin_2, qr/nan/i, 'asin of complex gives complex result';
}

{
# Check stringification of complex ndarray
my $c =  9.1234 + 4.1234*i();
like($c->dummy(2,1).'', qr/9.123.*4.123/, 'stringify native complex');
}

#test overloaded operators
{
    my $less = czip(3, -4);
    my $equal = -1*(-3+4*i);
    my $more = czip(3, 2);
    my $zero_imag = r2C(4);
    eval { my $bool = $less<$more }; ok $@, 'exception on invalid operator';
    eval { my $bool = $less<=$equal }; ok $@, 'exception on invalid operator';
    ok($less==$equal,'equal to');
    ok(!($less!=$equal),'not equal to');
    eval { my $bool = $more>$equal }; ok $@, 'exception on invalid operator';
    eval { my $bool = $more>=$equal }; ok $@, 'exception on invalid operator';
    ok($zero_imag==4,'equal to real');
    ok($zero_imag!=5,'neq real');
}

is pdl(i)->type, 'cdouble', 'pdl(complex ndarray) -> complex-typed ndarray';
is pdl([i])->type, 'cdouble', 'pdl([complex ndarray]) -> complex-typed ndarray';

done_testing;
