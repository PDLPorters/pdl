use PDL::LiteF;
#use PDL::Complex;
use PDL::Config;

BEGIN {
   use Test::More tests => 16;
}

sub tapprox {
        my($x,$y) = @_;
        my $c = abs($x-$y);
        my $d = max($c);
        $d < 0.0001;
}

$ref = pdl([[-2,1],[-3,1]]);
$ref2 = squeeze($ref->slice("0,")+ci()*$ref->slice("1,"));
$x = ci() -pdl (-2, -3);

ok($x->type eq 'cdouble', 'type promotion i - piddle');
ok(tapprox($x->cimag,$ref->slice("1,:")), 'value from i - piddle');

$x = cdouble (2,3);
$x-=3*ci();
ok(type ($x) eq 'cdouble', 'type promption piddle - i');
$y=cfloat($x);
ok(type ($y) eq 'cfloat', 'type conversion to cfloat');
ok(tapprox($x->cimag,$ref->slice("0,1")), 'value from piddle - i');

# dataflow from complex to real
$ar = $x->creal;
$ar++;
ok(tapprox($x->creal, -$ref->slice("0,")->squeeze), 'no complex to real dataflow');
$x+=ci;
ok(tapprox($x->cimag, -$ref->slice("1,")*2), 'no dataflow after conversion');

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = $ref2->copy;
my $a=abs($x);
my $p=carg($x);

my $y = $a*cos($p)+ci()*$a*sin($p);
ok(tapprox($x-$y, 0.), 'check re/im and mag/ang equivalence');

# to test Cabs, Cabs2, Carg (ref PDL)
# Catan, Csinh, Ccosh, Catanh, Croots

$cabs = sqrt($x->creal**2+$x->cimag**2);

ok(ref abs $x eq 'PDL', 'Cabs type');
ok(ref carg ($x) eq 'PDL', 'Carg type');
ok(tapprox($cabs, abs $x), 'Cabs value');
#ok(tapprox($cabs**2, Cabs2 $x), 'Cabs2 value');

# Check cat'ing of PDL::Complex
$y = $x->creal->copy + 1;
my $bigArray = $x->cat($y);
#ok(abs($bigArray->sum() +  8 - 4*ci()) < .0001, 'check cat for PDL::Complex');

my $z = pdl(0) + ci()*pdl(0);
$z **= 2;

ok($z->creal->at(0) == 0 && $z->cimag->at(0) == 0, 'check that 0 +0i exponentiates correctly'); # Wasn't always so.


my $zz = $z ** 0;

ok($zz->creal->at(0) == 1 && $zz->cimag->at(0) == 0, 'check that 0+0i ** 0 is 1+0i');

$z **= $z;

ok($z->creal->at(0) == 1 && $z->cimag->at(0) == 0, 'check that 0+0i ** 0+0i is 1+0i');

my $r = pdl(-10) + ci()*pdl(0);
$r **= 2;

ok($r->creal->at(0) < 100.000000001 && $r->creal->at(0) > 99.999999999 && $r->cimag->at(0) == 0,
  'check that imaginary part is exactly zero'); # Wasn't always so

TODO: {
   local $TODO = "Known_problems sf.net bug #1176614" if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS} );


   # Check stringification of complex piddle
   # This is sf.net bug #1176614
   my $c =  9.1234 + 4.1234*ci;
   my $c211 = $c->dummy(2,1);
   my $c211str = "$c211";
   ok($c211str=~/(9.123|4.123)/, 'sf.net bug #1176614');
}
