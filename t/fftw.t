use strict;

use PDL;
use PDL::FFT;
use Test::More;

eval "use PDL::FFTW;";
my $loaded = ($@ ? 0 : 1);

# this value is add-hoc and adjusted each time we find a problem with a
# given OS or library version of FFTW...
#
# We use the PDL::Core::approx() routine for comparing piddles
#
use constant ABSDIFF => 1.2e-4;

if ($loaded) {
   plan tests => 9;
} else {
   plan skip_all => "PDL::FFTW not available";
}

# get the type (double or float) used by the FFTW library and module
# The eval() is to avoid warning messages from Perl
#
my $datatype = eval('$PDL::FFTW::COMPILED_TYPE');

my $n = 30;
my $m = 40;

my ( $ir, $ii, $i, $fi, $fir, $fii, $ffi );
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

my ( $t, $orig, $i2, $sffi );
$t = ($ffi-$fi)*($ffi-$fi);

# print diff fftnd and ifftw: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "fftnd() and ifftw()");

$orig = fftw $fi;
$orig /= $n*$m;

$t = ($orig-$i)*($orig-$i);
# print "diff ifftw fftw and orig: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "ifftw() fftw() and original");

# Inplace FFT
$i2 = $i->copy;

infftw($i2);

$t = ($i2-$ffi)*($i2-$ffi);
# print "diff fftnd and infftw: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "fftnd and infftw");

$i2 = nfftw $i2;
$i2 /= $n*$m;

$t = ($i-$i2)*($i-$i2);
# print "diff infftw nfftw and orig: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "infftw nfftw and original");

$ir = zeroes($n,$m)->$datatype();
$ii = zeroes($n,$m)->$datatype();
$ir = random $ir;

$fir = $ir->copy;
$fii = $ii->copy;
ifftnd $fir,$fii;
$ffi = cat $fir,$fii;
$ffi = $ffi->mv(2,0);
$ffi *= $n*$m;
$sffi = $ffi->mslice('X',[0,$n/2],'X');

$fi = rfftw $ir;

$t = ($sffi-$fi)*($sffi-$fi);
# print "diff rfftw and infft: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "rfftw() and infft()");

$orig = irfftw $fi;
$orig /= $n*$m;

$t = ($orig-$ir)*($orig-$ir);
# print "diff ifftw fftw and orig: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "ifftw() fftw() and original");

my ( $rin, $srin, $tmp );
$rin = zeroes(2*(int($n/2)+1),$m)->$datatype();
$tmp = $rin->mslice([0,$n-1],'X');
$tmp .= $ir;
$srin = $rin->copy;

$rin = nrfftw $rin;

$t = ($sffi-$rin)*($sffi-$rin);
# print "diff nrfftw and infft: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "nrfftw() and infft()");

$rin = inrfftw $rin;
$rin /= $n*$m;

$rin = $rin->mslice([0,$n-1],'X');
$srin = $srin->mslice([0,$n-1],'X');

$t = ($srin-$rin)*($srin-$rin);
# print "diff inrfftw nrfftw and orig: ",sqrt($t->sum),"\n";
ok(approx(sqrt($t->sum),pdl(0),ABSDIFF),
   "inrfftw() nrfftw() and original");

# do the inplace routines work with slices?
my $a = ones(2,4)->$datatype();
my $fa = fftw $a;
nfftw $a->slice('');
ok(approx($fa,$a,ABSDIFF)->all,
   "inplace routine (nfftw) works with slices");
#print "$a\n$fa\n";
