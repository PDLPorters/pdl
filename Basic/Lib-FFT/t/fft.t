use strict;
use warnings;
use PDL;
use PDL::FFT;
use Test::More;
use Test::Exception;
use Test::PDL -atol => 0.01, -require_equal_types => 0;

foreach my $type(double,float,cdouble,cfloat){
  my $pa = pdl($type,1,-1,1,-1);
  my $pb = zeroes($type,$pa->dims);
  fft($pa,$pb);
  ok(all($pa==pdl($type,0,0,4,0)), "fft for type $type");
  ifft($pa,$pb);
  ok(all($pa==pdl($type,1,-1,1,-1)), "ifft for type $type");
}

my $pa = rfits("../../Basic/lib/PDL/Demos/m51.fits");

{
my $pb = $pa->copy;
my $pc = $pb->zeroes;
my $pd=czip($pb, $pc);
fft($pb,$pc);
ifft($pb,$pc);
fft($pd);
ifft($pd);
is_pdl $pc, $pb->zeroes, "fft zeroes";
is_pdl $pd->im, $pb->zeroes, "fft zeroes using complex ndarrays";
is_pdl $pa, $pb, "original image recovered";
}

{
my $pb = $pa->copy;
my $pc = $pb->zeroes;
my $pd=czip($pb, $pc);
fftnd($pb,$pc); ifftnd($pb,$pc);
is_pdl $pc, $pb->zeroes, "fftnd zeroes";
is_pdl $pa, $pb, "fftnd real image";
fftnd($pd); ifftnd($pd);
is_pdl $pd, $pb, "fftnd native complex image with imag zeroes";
}

{
my $pb = $pa->slice("1:35,1:69");
my $pc = $pb->copy; fftnd($pb,$pc); ifftnd($pb,$pc);
is_pdl $pc, $pb, "fftnd real and imaginary";
is_pdl $pa->slice("1:35,1:69"), $pb, "fftnd original restored";
}

{
my $pb = $pa->copy;
# Test real ffts
realfft($pb);
realifft($pb);
is_pdl $pa, $pb, "realfft";
}

# Test that errors are properly caught
throws_ok {fft(sequence(10))}
qr/Did you forget/, 'fft offers helpful message when only one argument is supplied'; #16

throws_ok {ifft(sequence(10))}
qr/Did you forget/, 'ifft offers helpful message when only one argument is supplied'; #17

done_testing;
