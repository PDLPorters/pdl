use strict;
use warnings;
use PDL::LiteF;
use PDL::Core::Dev;
use PDL::Types qw(ppdefs ppdefs_complex ppdefs_all);

use Test::More;
use Test::PDL;

is_deeply [ ppdefs() ], [qw(A B S U L K N P Q F D E)];
is_deeply [ ppdefs_complex() ], [qw(G C H)];
is_deeply [ ppdefs_all() ], [qw(A B S U L K N P Q F D E G C H)];

my $ref = pdl([[-2,1],[-3,1]]);
my $ref2 = squeeze(czip($ref->slice("0,"), $ref->slice("1,")));
my $x = i() -pdl (-2, -3);

is($x->type, 'cdouble', 'type promotion i - ndarray');
is_pdl $x->im, $ref->slice("(1),:"), 'value from i - ndarray';
ok !$x->type->real, 'complex type not real';
ok double->real, 'real type is real';
ok !$x->sumover->type->real, 'sumover type=complex';

for (qw(conj re im)) {
  eval {double(5)->$_};
  is $@, '', "NO error if give real data to $_";
}
for (qw(carg)) {
  eval {double(5)->$_};
  like $@, qr/must be complex/, "error if give real data to $_";
}
eval {czip(cdouble(5),1)};
like $@, qr/must be real/, "error if give complex data to czip";
$x = cdouble(2,3);
$x-=i2C(3);
is type($x), 'cdouble', 'type promotion ndarray - i';
is $x->re->type, 'double', 'real real part';
my $y=cfloat($x);
is type($y), 'cfloat', 'type conversion to cfloat';
is $y->re->type, 'float', 'real real part';
is_pdl $x->im, $ref->slice("(0),(1),*2"), 'value from ndarray - i';
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
is_pdl $x->re, -$ref->slice("0,")->squeeze + 1, 'complex to real dataflow';
my $ai = $x->im;
$x+=i;
my $expected = pdl(-2, -2);
is_pdl $x->im, $expected, 'dataflow after conversion';
$ai++;
is_pdl $x->im, $expected+1, 'dataflow after change ->im';
}

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = $ref2->copy;
my $a=abs($x);
my $p=carg($x)->double; # force to double to avoid glibc bug 18594
is_pdl czip($a*cos($p), $a*sin($p)), $x, 'check re/im and mag/ang equivalence';

# Catan, Csinh, Ccosh, Catanh, Croots

my $cabs = sqrt($x->re**2+$x->im**2);

ok(abs($x)->type->real, 'Cabs type real');
is_pdl abs $x, $cabs, 'Cabs value';
is_pdl abs2($x), $cabs**2, 'Cabs2 value';
ok abs2(cdouble(5))->type->real, 'abs2 always real';
ok(carg($x)->type->real, 'Carg type real');
is_pdl carg($x), atan2($x->im, $x->re), 'Carg value';

is_pdl $x->cat($x->re->copy + 1), pdl('-2+i -3+i; -1 -2'), 'cat for complex';

if (PDL::Core::Dev::got_complex_version('pow', 2)) {
  is_pdl $x**2, $x * $x, '** op complex';
  is_pdl $x->pow(2), $x * $x, 'complex pow';
  is_pdl $x->power(2, 0), $x * $x, 'complex power';
  my $z = pdl(0) + i()*pdl(0);
  $z **= 2;
  is_pdl $z, i2C(0), 'check that 0 +0i exponentiates correctly'; # Wasn't always so.
  my $r = r2C(-10);
  $r **= 2;
  is_pdl $r, r2C(100), 'check imaginary part exactly zero'; # Wasn't always so
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
