use PDL::LiteF;
use PDL::Complex;
use PDL::Config;

BEGIN {
   use Test::More tests => 13;
}

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

$ref = pdl([[-2,1],[-3,1]]);
$a = i - pdl(2,3);
ok(ref $a eq PDL::Complex, 'type promotion i - piddle');
ok(tapprox($a->real,$ref), 'value from i - piddile');

$a = pdl(2,3) - i;
ok(ref $a eq PDL::Complex, 'type promption piddle - i');
ok(tapprox($a->real,-$ref), 'value from piddle - i');

# dataflow from complex to real
$ar = $a->real;
$ar++;
ok(tapprox($a->real, -$ref+1), 'complex to real dataflow');

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$a = cplx($ref);
my $b = $a->Cr2p()->Cp2r();
ok(tapprox($a-$b, 0), 'check re/im and mag/ang equivalence');

# to test Cabs, Cabs2, Carg (ref PDL)
# Catan, Csinh, Ccosh, Catanh, Croots

$cabs = sqrt($a->re**2+$a->im**2);

ok(ref Cabs $a eq 'PDL', 'Cabs type');
ok(ref Cabs2 $a eq 'PDL', 'Cabs2 type');
ok(ref Carg $a eq 'PDL', 'Carg type');
ok(tapprox($cabs, Cabs $a), 'Cabs value');
ok(tapprox($cabs**2, Cabs2 $a), 'Cabs2 value');

# Check cat'ing of PDL::Complex
$b = $a->copy + 1;
my $bigArray = $a->cat($b);
ok(abs($bigArray->sum() +  8 - 4*i) < .0001, 'check cat for PDL::Complex');

TODO: {
   local $TODO = "Known_problems sf.net bug #1176614" if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS} );


   # Check stringification of complex piddle
   # This is sf.net bug #1176614
   my $c =  9.1234 + 4.1234*i;
   my $c211 = $c->dummy(2,1);
   my $c211str = "$c211";
   ok($c211str=~/(9.123|4.123)/, 'sf.net bug #1176614');
}
