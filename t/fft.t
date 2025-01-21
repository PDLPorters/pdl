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
  is_pdl $pa, pdl($type,0,0,4,0), "fft for type $type";
  ifft($pa,$pb);
  is_pdl $pa, pdl($type,1,-1,1,-1), "ifft for type $type";
}

my $pa = rfits("lib/PDL/Demos/m51.fits");

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
    my $x     = xvals( 10, 10 ) + yvals( 10, 10 ) * 10;
    my $index = cat( 3 + xvals( 5, 5 ) * 0.25, 7 + yvals( 5, 5 ) * 0.25 )
      ->reorder( 2, 0, 1 );
    is_pdl $x->long->interpND($index, {method=>'f'}), long('36 36 34 34 35; 51 51 49 49 50; 52 51 49 49 51; 33 33 31 31 32; 26 26 24 24 25');
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
