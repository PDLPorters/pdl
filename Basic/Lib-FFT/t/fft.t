use strict;
use warnings;
use PDL;
use PDL::FFT;
use Test::More;
use Test::Exception;

sub tapprox {
        my($pa,$pb) = @_;
	all approx $pa, $pb, 0.01;
}

foreach my $type(double,float,cdouble,cfloat){
  my $pa = pdl($type,1,-1,1,-1);
  my $pb = zeroes($type,$pa->dims);
  fft($pa,$pb);
  ok(all($pa==pdl($type,0,0,4,0)), "fft for type $type");
  ifft($pa,$pb);
  ok(all($pa==pdl($type,1,-1,1,-1)), "ifft for type $type");
}

my $pa = rfits("../../Basic/m51.fits");

{
my $pb = $pa->copy;
my $pc = $pb->zeroes;
my $pd=czip($pb, $pc);
fft($pb,$pc);
ifft($pb,$pc);
fft($pd);
ifft($pd);
ok (tapprox($pc,0), "fft zeroes");
ok (tapprox($pd->im,0), "fft zeroes using complex ndarrays");
ok (tapprox($pa,$pb), "original image recovered");
}

{
my $pb = $pa->copy;
my $pc = $pb->zeroes;
my $pd=czip($pb, $pc);
fftnd($pb,$pc); ifftnd($pb,$pc);
ok ( tapprox($pc,0), "fftnd zeroes");
ok ( tapprox($pa,$pb), "fftnd real image");
fftnd($pd); ifftnd($pd);
ok ( tapprox($pd,$pb), "fftnd native complex image with imag zeroes");
}

{
my $pb = $pa->slice("1:35,1:69");
my $pc = $pb->copy; fftnd($pb,$pc); ifftnd($pb,$pc);
ok ( tapprox($pc,$pb), "fftnd real and imaginary");
ok ( tapprox($pa->slice("1:35,1:69"),$pb), "fftnd original restored");
}

{
my $pb = $pa->copy;
# Test real ffts
realfft($pb);
realifft($pb);
ok( tapprox($pa,$pb), "realfft");
}

# Test that errors are properly caught
throws_ok {fft(sequence(10))}
qr/Did you forget/, 'fft offers helpful message when only one argument is supplied'; #16

throws_ok {ifft(sequence(10))}
qr/Did you forget/, 'ifft offers helpful message when only one argument is supplied'; #17

done_testing;
