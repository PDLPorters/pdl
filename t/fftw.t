use PDL;
use PDL::FFT;
use Test;

BEGIN {
        eval "use PDL::FFTW;";
        $loaded = ($@ ? 0 : 1);
	if ($loaded) {
	  plan tests => 9;
        } else {
	  plan tests => 1;
	  print "ok 1 # Skipped: PDL::FFTW not available\n";
	  exit;
        }
}

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
#print STDERR "diff = [$d]\n";
#        $d < 0.0001;
	$d < 1.1e-4;
}


$datatype = eval('$PDL::FFTW::COMPILED_TYPE') if($loaded); # get the type (doubld or float) that PDL::FFTW was linked/compiled with.
							 # use eval to avoid warning message when PDL::GSL::FFTW not loaded

$n = 30;
$m = 40;

$ir = zeroes($n,$m)->$datatype();
$ii = zeroes($n,$m)->$datatype();
$ir = random $ir;
$ii = random $ii;

$i = cat $ir,$ii;
$i = $i->mv(2,0);
$fi = ifftw $i;

$fir = $ir->copy;
$fii = $ii->copy;
fftnd $fir,$fii;
$ffi = cat $fir,$fii;
$ffi = $ffi->mv(2,0);

$t = ($ffi-$fi)*($ffi-$fi);

# print diff fftnd and ifftw: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );


$orig = fftw $fi;
$orig /= $n*$m;

$t = ($orig-$i)*($orig-$i);
# print "diff ifftw fftw and orig: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );

# Inplace FFT
$i2 = $i->copy;

infftw($i2);

$t = ($i2-$ffi)*($i2-$ffi);
# print "diff fftnd and infftw: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );

$i2 = nfftw $i2;
$i2 /= $n*$m;

$t = ($i-$i2)*($i-$i2);
# print "diff infftw nfftw and orig: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );


$ir = zeroes($n,$m)->$datatype();
$ii = zeroes($n,$m)->$datatype();
$ir = random $ir;

$fir = $ir->copy;
$fii = $ii->copy;
ifftnd $fir,$fii;
$ffi = cat $fir,$fii;
$ffi = $ffi->mv(2,0);
$ffi *= $n*$m;
$sffi = $ffi->mslice(X,[0,$n/2],X);

$fi = rfftw $ir;

$t = ($sffi-$fi)*($sffi-$fi);
# print "diff rfftw and infft: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );

$orig = irfftw $fi;
$orig /= $n*$m;

$t = ($orig-$ir)*($orig-$ir);
# print "diff ifftw fftw and orig: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );


$rin = zeroes(2*(int($n/2)+1),$m)->$datatype();
$tmp = $rin->mslice([0,$n-1],X);
$tmp .= $ir;
$srin = $rin->copy;

$rin = nrfftw $rin;

$t = ($sffi-$rin)*($sffi-$rin);
# print "diff nrfftw and infft: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );

$rin = inrfftw $rin;
$rin /= $n*$m;

$rin = $rin->mslice([0,$n-1],X);
$srin = $srin->mslice([0,$n-1],X);

$t = ($srin-$rin)*($srin-$rin);
# print "diff inrfftw nrfftw and orig: ",sqrt($t->sum),"\n";
ok(tapprox(sqrt($t->sum),pdl(0))  );

# do the inplace routines work with slices?
my $a = ones(2,4)->$datatype();
my $fa = fftw $a;
nfftw $a->slice('');
ok(tapprox($fa,$a)  );
print "$a\n$fa\n";
