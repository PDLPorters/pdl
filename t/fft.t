
use strict;

use PDL;
use PDL::Image2D;
use PDL::FFT;

use Test;
BEGIN { plan tests => 17; }

sub tapprox {
        my($a,$b) = @_;
        my ($c) = abs($a-$b);
        my $d = max($c);
        $d < 0.01;
}

my ( $a, $b, $c, $i, $k, $kk );

foreach my $type(double,float){
  $a = pdl($type,1,-1,1,-1);
  $b = zeroes($type,$a->dims);
  fft($a,$b);
  ok(all($a==pdl($type,0,0,4,0))); #1,3
  ifft($a,$b);
  ok(all($a==pdl($type,1,-1,1,-1))); #2,4
}

$k = ones(5,5);
$a = rfits("m51.fits");

$b = $a->copy;
$c = $b->zeroes;
fft($b,$c);
ifft($b,$c);
ok (tapprox($c,0)); #5

#print "\n",$c->info("Type: %T Dim: %-15D State: %S"),"\n";
#print "Max: ",$c->max,"\n";
#print "Min: ",$c->min,"\n";
   
ok (tapprox($a,$b)); #6

$b = $a->copy;
$c = $b->zeroes; fftnd($b,$c); ifftnd($b,$c);
ok ( tapprox($c,0) ); #7
ok ( tapprox($a,$b) );#8

$b = $a->slice("1:35,1:69");
$c = $b->copy; fftnd($b,$c); ifftnd($b,$c);
ok ( tapprox($c,$b) );#9
ok ( tapprox($a->slice("1:35,1:69"),$b) );#10

# Now compare fft convolutions with direct method

$b = conv2d($a,$k);
$kk = kernctr($a,$k);
fftconvolve( $i=$a->copy, $kk );

ok ( tapprox($kk,0) );#11
ok ( tapprox($i,$b) );#12

$k = pdl[
 [ 0.51385498,  0.17572021,  0.30862427],
 [ 0.53451538,  0.94760132,  0.17172241],
 [ 0.70220947,  0.22640991,  0.49475098],
 [ 0.12469482, 0.083892822,  0.38961792],
 [ 0.27722168,  0.36804199,  0.98342896],
 [ 0.53536987,  0.76565552,  0.64645386],
 [ 0.76712036,   0.7802124,  0.82293701]
];
$b = conv2d($a,$k);

$kk = kernctr($a,$k);
fftconvolve( $i=$a->copy, $kk );

ok ( tapprox($kk,0) );#13
ok ( tapprox($i,$b) );#14

$b = $a->copy;

# Test real ffts
realfft($b);
realifft($b);
ok( tapprox($a,$b) );#15

# Test that errors are properly caught
eval {fft(sequence(10))};
ok( $@, qr/Did you forget/, 'fft offers helpful message when only one argument is supplied'); #16
$@ = '';


eval {ifft(sequence(10))};
ok( $@, qr/Did you forget/, 'ifft offers helpful message when only one argument is supplied'); #17
$@ = '';

# End
