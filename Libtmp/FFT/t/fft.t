use strict;
use warnings;
use PDL;
use PDL::Image2D;
use PDL::FFT;
use Test::More;
use Test::Exception;

sub tapprox {
        my($pa,$pb) = @_;
	all approx $pa, $pb, 0.01;
}

my ( $pa, $pb, $pc, $pi, $pk, $kk );

foreach my $type(double,float,cdouble,cfloat){
  $pa = pdl($type,1,-1,1,-1);
  $pb = zeroes($type,$pa->dims);
  fft($pa,$pb);
  ok(all($pa==pdl($type,0,0,4,0)), "fft for type $type");
  ifft($pa,$pb);
  ok(all($pa==pdl($type,1,-1,1,-1)), "ifft for type $type");
}

$pk = ones(5,5);
$pa = rfits("../../m51.fits");

$pb = $pa->copy;
$pc = $pb->zeroes;
my $pd=czip($pb, $pc);
fft($pb,$pc);
ifft($pb,$pc);
fft($pd);
ifft($pd);
ok (tapprox($pc,0), "fft zeroes");
ok (tapprox(im($pd),0), "fft zeroes using complex ndarrays");

#print "\n",$pc->info("Type: %T Dim: %-15D State: %S"),"\n";
#print "Max: ",$pc->max,"\n";
#print "Min: ",$pc->min,"\n";
   
ok (tapprox($pa,$pb), "m51 image recovered");

$pb = $pa->copy;
$pc = $pb->zeroes; fftnd($pb,$pc); ifftnd($pb,$pc);
ok ( tapprox($pc,0), "fftnd zeroes");
ok ( tapprox($pa,$pb), "fftnd real image");

$pb = $pa->slice("1:35,1:69");
$pc = $pb->copy; fftnd($pb,$pc); ifftnd($pb,$pc);
ok ( tapprox($pc,$pb), "fftnd real and imaginary");
ok ( tapprox($pa->slice("1:35,1:69"),$pb), "fftnd original restored");

# Now compare fft convolutions with direct method

$pb = conv2d($pa,$pk);
$kk = kernctr($pa,$pk);
fftconvolve( $pi=$pa->copy, $kk );

ok ( tapprox($kk,0), "kernctr");
ok ( tapprox($pi,$pb), "fftconvolve" );

$pk = pdl[
 [ 0.51385498,  0.17572021,  0.30862427],
 [ 0.53451538,  0.94760132,  0.17172241],
 [ 0.70220947,  0.22640991,  0.49475098],
 [ 0.12469482, 0.083892822,  0.38961792],
 [ 0.27722168,  0.36804199,  0.98342896],
 [ 0.53536987,  0.76565552,  0.64645386],
 [ 0.76712036,   0.7802124,  0.82293701]
];
$pb = conv2d($pa,$pk);

$kk = kernctr($pa,$pk);
fftconvolve( $pi=$pa->copy, $kk );

ok ( tapprox($kk,0), "kernctr weird kernel");
ok ( tapprox($pi,$pb), "fftconvolve weird kernel");

$pb = $pa->copy;

# Test real ffts
realfft($pb);
realifft($pb);
ok( tapprox($pa,$pb), "realfft");

# Test that errors are properly caught
throws_ok {fft(sequence(10))}
qr/Did you forget/, 'fft offers helpful message when only one argument is supplied'; #16

throws_ok {ifft(sequence(10))}
qr/Did you forget/, 'ifft offers helpful message when only one argument is supplied'; #17

done_testing;
