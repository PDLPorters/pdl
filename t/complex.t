use PDL::LiteF;
use PDL::Complex;
use PDL::Config;

BEGIN {
   use Test::More tests => 17;
}

sub tapprox {
        my($x,$y) = @_;
        my $c = abs($x-$y);
        my $d = max($c);
        $d < 0.0001;
}

$ref = pdl([[-2,1],[-3,1]]);
$x = i - pdl(2,3);

ok(ref $x eq PDL::Complex, 'type promotion i - piddle');
ok(tapprox($x->real,$ref), 'value from i - piddle');

$x = pdl(2,3) - i;
ok(ref $x eq PDL::Complex, 'type promption piddle - i');
ok(tapprox($x->real,-$ref), 'value from piddle - i');

# dataflow from complex to real
$ar = $x->real;
$ar++;
ok(tapprox($x->real, -$ref+1), 'complex to real dataflow');

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = cplx($ref);
my $y = $x->Cr2p()->Cp2r();
ok(tapprox($x-$y, 0), 'check re/im and mag/ang equivalence');

# to test Cabs, Cabs2, Carg (ref PDL)
# Catan, Csinh, Ccosh, Catanh, Croots

$cabs = sqrt($x->re**2+$x->im**2);

ok(ref Cabs $x eq 'PDL', 'Cabs type');
ok(ref Cabs2 $x eq 'PDL', 'Cabs2 type');
ok(ref Carg $x eq 'PDL', 'Carg type');
ok(tapprox($cabs, Cabs $x), 'Cabs value');
ok(tapprox($cabs**2, Cabs2 $x), 'Cabs2 value');

# Check cat'ing of PDL::Complex
$y = $x->copy + 1;
my $bigArray = $x->cat($y);
ok(abs($bigArray->sum() +  8 - 4*i) < .0001, 'check cat for PDL::Complex');

my $z = pdl(0) + i*pdl(0);
$z **= 2;

ok($z->at(0) == 0 && $z->at(1) == 0, 'check that 0 +0i exponentiates correctly'); # Wasn't always so.

my $zz = $z ** 0;

ok($zz->at(0) == 1 && $zz->at(1) == 0, 'check that 0+0i ** 0 is 1+0i');

$z **= $z;

ok($z->at(0) == 1 && $z->at(1) == 0, 'check that 0+0i ** 0+0i is 1+0i');

my $r = pdl(-10) + i*pdl(0);
$r **= 2;

ok($r->at(0) < 100.000000001 && $r->at(0) > 99.999999999 && $r->at(1) == 0,
  'check that imaginary part is exactly zero'); # Wasn't always so

TODO: {
   local $TODO = "Known_problems sf.net bug #1176614" if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS} );


   # Check stringification of complex piddle
   # This is sf.net bug #1176614
   my $c =  9.1234 + 4.1234*i;
   my $c211 = $c->dummy(2,1);
   my $c211str = "$c211";
   ok($c211str=~/(9.123|4.123)/, 'sf.net bug #1176614');
}
