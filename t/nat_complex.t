use strict;
use warnings;
use PDL::LiteF;
#use PDL::Complex;
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

is_deeply [ ppdefs() ], [qw(B S U L N Q F D)];
is_deeply [ ppdefs_complex() ], [qw(G C)];
is_deeply [ ppdefs_all() ], [qw(B S U L N Q F D G C)];

my $ref = pdl([[-2,1],[-3,1]]);
my $ref2 = squeeze($ref->slice("0,")+ci()*$ref->slice("1,"));
my $x = ci() -pdl (-2, -3);

is($x->type, 'cdouble', 'type promotion i - ndarray');
ok(tapprox($x->cimag,$ref->slice("1,:")), 'value from i - ndarray');
ok !$x->type->real, 'complex type not real';
ok double->real, 'real type is real';
ok !$x->sumover->type->real, 'sumover type=complex';

$x = cdouble(2,3);
$x-=3*ci();
is type($x), 'cdouble', 'type promotion ndarray - i';
is $x->creal->type, 'double', 'real real part';
my $y=cfloat($x);
is type($y), 'cfloat', 'type conversion to cfloat';
is $y->creal->type, 'float', 'real real part';
ok(tapprox($x->cimag,$ref->slice("0,1")), 'value from ndarray - i') or diag 'got: ', $x->cimag;
is zeroes($_->[0], 2)->r2C->type, $_->[1], "r2C $_->[0] -> $_->[1]"
  for [byte, 'cfloat'], [long, 'cfloat'],
    [float, 'cfloat'], [cfloat, 'cfloat'],
    [double, 'cdouble'], [cdouble, 'cdouble'];
my $got_double = double(-1, 2);
my $got_r2C = $got_double->r2C;
is ''.$got_r2C->creal, ''.$got_double, 'creal(r2C) identical to orig';

my $got = r2C(1);
is $got, 1, 'can give Perl numbers to r2C';
ok !$got->type->real, 'complex type';

for (float, double, cfloat, cdouble) {
  my $got = pdl $_, '[0 BAD]';
  my $bv = $got->badvalue;
  my $obv = $got->orig_badvalue;
  is $got.'', '[0 BAD]', "$_ bad"
    or diag "bv=$bv, obv=$obv: ", explain [$bv, $obv];
  is $got->isbad.'', '[0 1]', "$_ isbad";
  # this captures a failure in IO/Flexraw/t/iotypes.t
  eval { ok $bv == $obv, 'can equality-check badvalue and orig_badvalue' };
  is $@, '' or diag explain [$bv, $obv];
}

# dataflow from complex to real
my $ar = $x->creal;
$ar++;
ok(tapprox($x->creal, -$ref->slice("0,")->squeeze), 'no complex to real dataflow');
$x+=ci;
ok(tapprox($x->cimag, -$ref->slice("1,")*2), 'no dataflow after conversion');

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = $ref2->copy;
my $a=abs($x);
my $p=carg($x)->double; # force to double to avoid glibc bug 18594

$y = $a*cos($p)+ci()*$a*sin($p);
ok(tapprox($x-$y, 0.), 'check re/im and mag/ang equivalence')
  or diag "For ($x), got: ($y) from a=($a) p=($p) cos(p)=(", cos($p), ") sin(p)=(", sin($p), ")";

# to test Cabs, Cabs2, Carg (ref PDL)
# Catan, Csinh, Ccosh, Catanh, Croots

my $cabs = sqrt($x->creal->double**2+$x->cimag->double**2);

ok(ref abs $x eq 'PDL', 'Cabs type');
ok(ref carg ($x) eq 'PDL', 'Carg type');
ok(tapprox(abs $x, $cabs), 'Cabs value') or diag "got: (@{[abs $x]}), expected ($cabs)";

# Check cat'ing of PDL::Complex
$y = $x->creal->copy + 1;
my $bigArray = $x->cat($y);
#ok(abs($bigArray->sum() +  8 - 4*ci()) < .0001, 'check cat for PDL::Complex');

if (PDL::Core::Dev::got_complex_version('pow', 2)) {
  ok(tapprox($x**2, $x * $x), '** op complex')
    or diag "For ($x), got: ", $x**2, ", expected: ", $x * $x;
  ok(tapprox($x->pow(2), $x * $x), 'complex pow')
    or diag "Got: ", $x->pow(2), ", expected: ", $x * $x;
  ok(tapprox($x->power(2, 0), $x * $x), 'complex power')
    or diag "Got: ", $x->power(2, 0), ", expected: ", $x * $x;
  my $z = pdl(0) + ci()*pdl(0);
  $z **= 2;
  ok(tapprox($z, 0 + 0*ci), 'check that 0 +0i exponentiates correctly'); # Wasn't always so.
  my $r = pdl(-10) + ci()*pdl(0);
  $r **= 2;
  ok(tapprox($r, 100 + 0*ci),
    'check that imaginary part is exactly zero') # Wasn't always so
    or diag "got: ", $r;
}

my $asin_2 = PDL::asin(2)."";
like $asin_2, qr/nan/i, 'perl scalar 2 treated as real';
$asin_2 = PDL::asin(2.0)."";
like $asin_2, qr/nan/i, 'perl scalar 2.0 treated as real';
$asin_2 = PDL::asin(byte 2)."";
like $asin_2, qr/nan/i, 'real byte treated as real';
$asin_2 = PDL::asin(double 2)."";
like $asin_2, qr/nan/i, 'real double treated as real';
$asin_2 = PDL::asin(pdl 2)."";
like $asin_2, qr/nan/i, 'pdl(2) treated as real';
if (PDL::Core::Dev::got_complex_version('asin', 1)) {
  my $c_asin_2 = PDL::asin(cdouble(2))."";
  unlike $c_asin_2, qr/nan/i, 'asin of complex gives complex result';
}

TODO: {
   local $TODO = "Known_problems sf.net bug #1176614" if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS} );
   # Check stringification of complex ndarray
   # This is sf.net bug #1176614
   my $c =  9.1234 + 4.1234*ci;
   my $c211 = $c->dummy(2,1);
   my $c211str = "$c211";
   ok($c211str=~/(9.123|4.123)/, 'sf.net bug #1176614');
}

#test overloaded operators
{
    my $less = 3-4*ci;
    my $equal = -1*(-3+4*ci);
    my $more = 3+2*ci;
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

is pdl(ci)->type, 'cdouble', 'pdl(complex ndarray) -> complex-typed ndarray';
is pdl([ci])->type, 'cdouble', 'pdl([complex ndarray]) -> complex-typed ndarray';

done_testing;
