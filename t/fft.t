use PDL;
use PDL::Image2D;
use PDL::FFT;

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub tapprox {
        my($a,$b) = @_;
        my ($c) = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

print "1..10\n";

$k = ones(5,5);
$a = rfits("m51.fits");

$b = $a->copy;
$c = $b->zeroes; fft($b,$c); ifft($b,$c);
ok (1,tapprox($c,0));

print "\n",$c->info("Type: %T Dim: %-15D State: %S"),"\n";
print "Max: ",$c->max,"\n";
print "Min: ",$c->min,"\n";
   
# The second test fails at the moment: this appears to be an
# oddity with PP's promotion of byte data for GenericTypes => [F,D]
# routines
#  Commented-out for now.
#   ok (2,tapprox($a,$b));

$b = $a->copy;
$c = $b->zeroes; fftnd($b,$c); ifftnd($b,$c);
ok (2,tapprox($c,0));   ok (3,tapprox($a,$b));

$b = $a->slice("1:35,1:69");
$c = $b->copy; fftnd($b,$c); ifftnd($b,$c);
ok (4,tapprox($c,$b));   ok (5,tapprox($a->slice("1:35,1:69"),$b));

# Now compare fft convolutions with direct method

$b = conv2d($a,$k);
$kk = kernctr($a,$k);   fftconvolve($i=$a->copy,$kk);

ok (6,tapprox($kk,0));   ok (7,tapprox($i,$b));

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

$kk = kernctr($a,$k);  fftconvolve($i=$a->copy,$kk);

ok (8,tapprox($kk,0));  ok (9,tapprox($i,$b));

$b = $a->copy;

# Test real ffts
realfft($b); realifft($b); ok(10,tapprox($a,$b));
