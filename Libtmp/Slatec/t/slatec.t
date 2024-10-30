use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use Test::PDL;
use PDL::Slatec;
use PDL::MatrixOps qw(identity);

kill 'INT', $$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $mat = pdl [1,0.1],[0.1,2];

my ($eigvals,$eigvecs) = eigsys($mat);

## print STDERR $eigvecs,$eigvals,"\n";

is_pdl $eigvals,float(0.9901,2.009), {atol=>1e-3};
is_pdl $eigvecs,float([0.995,-0.0985],[0.0985,0.995]), {atol=>1e-3};

$mat = pdl [2,3],[4,5];

my $inv = matinv($mat);

my $uni=scalar $mat x $inv;
is_pdl $uni,identity(2);

eval {matinv(identity(2)->dummy(-1,2))};
is $@, '', 'matinv can broadcast';

my $det = $mat->det;
my $deti = $inv->det;

is_pdl $det, pdl(-2);
is_pdl $deti, pdl(-0.5);

# Now do the polynomial fitting tests

# Set up tests x, y and weight
my $y = pdl (1,4,9,16,25,36,49,64.35,32);
my $x = pdl ( 1,2,3,4,5,6,7,8,9);
my $w = pdl ( 1,1,1,1,1,1,1,0.5,0.3);

# input parameters
my $eps = pdl(0);
my $maxdeg = 5;

# Test with a bad value
$y->inplace->setbadat(3);
my ($ndeg, $r, $ierr, $a1) = polyfit($x, $y, $w, $maxdeg, $eps);

ok(($ierr == 1));

# Test with all bad values
$y = zeroes(9)->setbadif(1);
($ndeg, $r, $ierr, $a1) = polyfit($x, $y, $w, $maxdeg, $eps);
ok(($ierr == 2));

# Now test broadcasting over a 2 by N matrix
# Set up tests x, y and weight
$y = pdl ([1,4,9,16,25,36,49,64.35,32],
          [1,4,9,16,25,36,49,64.35,32],);
$x = pdl ([1,2,3,4,5,6,7,8,9],
          [1,2,3,4,5,6,7,8,9],);
$w = pdl ([1,1,1,1,1,1,1,0.5,0.3],
          [1,1,1,1,1,1,1,0.5,0.3],);
$y->inplace->setbadat(3,0);
$y->inplace->setbadat(4,1);
$eps = pdl(0,0);

($ndeg, $r, $ierr, $a1) = polyfit($x, $y, $w, $maxdeg, $eps);

## print STDERR "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";
## print STDERR "poly = $r\n";

ok((sum($ierr == 1) == 2));

# Set up tests x, y and weight
$x = pdl ( 1,2,3,4,5,6,7,8,9);
$y = pdl (1,4,9,16,25,36,49,64.35,32);
$w = pdl ( 1,1,1,1,1,1,1,0.5,0.3);
$maxdeg = 7;
$eps = pdl(0);

# Do the fit
($ndeg, $r, $ierr, $a1) = polyfit($x, $y, $w, $maxdeg, $eps);

ok(($ierr == 1));

# Test POLYCOEF                                                               
my $c = pdl(4);           # Expand about x = 4;

my $tc = polycoef($ndeg, $c, $a1);

my @tc = $tc->list;
my @r  = $r->list;
my $i = 0;

foreach my $xpos ($x->list) {
  my $ypos = 0;
  my $n = 0;
  foreach my $bit ($tc->list) {
    $ypos += $bit * ($xpos- (($c->list)[0]))**$n;
    $n++;
  }
  ## print STDERR "$xpos, $ypos, $r[$i]\n";

  # Compare with answers from polyfit
  ok(sprintf("%5.2f", $ypos) == sprintf("%5.2f", $r[$i]));
  $i++;                                                                       

}

# Try polyvalue with a single x pos
my $xx = pdl(4);
my $nder = 3;

my ($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a1);

ok(int($yp->at(0)) == 8);

# Test polyvalue
$nder = 3;
$xx    = pdl(12,4,6.25,1.5); # Ask for multiple positions at once

($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a1);

# Simple test of expected value                                               
ok(int($yfit->at(1)) == 15);            

# test the PCHIP stuff

## Test: chim/chic
$x = float( 3 .. 10 );
my $f = $x*$x*$x + 425.42352;
my $answer = 3*$x*$x;

my ( $d, $err ) = chim( float($x), float($f) );
is_pdl $err, longlong 0;

# don't check the first and last elements, as expect the
# error to be largest there
# value of 5% comes from tests on linux and solaris machines
is_pdl +(map $_->slice('1:-2'), $d, $answer), {rtol=>0.05};

# compare the results of chic
my $d2 = $f->zeroes;
chic( pdl([0, 0]), pdl([0, 0]), 1, $x, $f, $d2, my $err2=null );
is_pdl $err2, longlong 0;
is_pdl $d2, $d->double, {atol=>2e-2};

## Test: chsp
chsp( pdl([0, 0]), pdl([0, 0]), $x, $f, my $d3=null, my $err3=null );
is_pdl $err3, longlong 0;
is_pdl $d3, $d->double, {atol=>2};

## Test: chfd/chfe
#
my $xe = float( pdl( 4 .. 8 ) + 0.5 );
my ( $fe, $de );
( $fe, $de, $err ) = chfd( $x, $f, $d, 1, $xe );

ok(($err->getndims==0) & ($err->sum == 0));

$answer = $xe*$xe*$xe + 425.42352;
is_pdl $fe, $answer, {rtol=>1e-5};

$answer = 3.0*$xe*$xe;
is_pdl $de, $answer, {rtol=>2e-2};

( $fe, $err ) = chfe( $x, $f, $d, 1, $xe );
is_pdl $fe, $xe*$xe*$xe + 425.42352, {rtol=>1e-3};
is_pdl $err, longlong 0;

# Test: chcm
#
$x   = float( 1, 2, 3, 5, 6, 7 );
$f   = float( 1, 2, 3, 4, 3, 4 );
my $ans = longlong(  1, 1, 1, -1, 1, 2 );

( $d, $err ) = chim($x, $f);
is_pdl $err, longlong 2;

( my $ismon, $err ) = chcm($x, $f, $d, 1);
is_pdl $err, longlong 0;
is_pdl $ismon,$ans;

## Test: chia
#
$x = double( sequence(11) - 0.3 );
$f = $x * $x;
( $d, $err ) = chim($x, $f);

$ans = pdl( 9.0**3, (8.0**3-1.0**3) ) / 3.0;
( my $int, $err ) = chia($x, $f, $d, my $skip=zeroes(2), pdl(0.0,1.0), pdl(9.0,8.0));
is_pdl $err, longlong 0,0;
is_pdl $int, $ans, {atol=>4e-2};

my $hi = pdl( $x->at(9), $x->at(7) );
my $lo = pdl( $x->at(0), $x->at(1) );
$ans = ($hi**3 - $lo**3) / 3;
( $int, $err ) = chid( $x, $f, $d, $skip=zeroes(2), pdl(0,1), pdl(9,7) );
is_pdl $err, longlong 0,0;
is_pdl $int, $ans, {atol=>6e-2};

## Test: chbs - note, only tests that it runs successfully
my $nknots = 0;
my $t = zeroes( float, 2*$x->dim(0)+4 );
my $bcoef  = zeroes( float, 2*$x->dim(0) );
my $ndim = PDL->null;
my $kord = PDL->null;
$err = PDL->null;
chbs( $x, $f, $d, 0, $nknots, $t, $bcoef, $ndim, $kord, $err );
is_pdl $err, longlong 0;

## Test: bvalu - note, only tests that it runs successfully
my $x_slice = $x->slice('0:-2'); # because calling with last value is out of range
my ($val) = bvalu($t, $bcoef, 0, $x_slice);

my $A = identity(4) + ones(4, 4);
$A->slice('2,0') .= 0; # break symmetry to see if need transpose
my $B = sequence(2, 4);
gefa(my $lu=$A->copy, my $ipiv=null, my $info=null);
gesl($lu, $ipiv, $x=$B->transpose->copy, 1); # 1 = do transpose because Fortran
$x = $x->inplace->transpose;
my $got = $A x $x;
is_pdl $got, $B;

{
my $pa = pdl(float,1,-1,1,-1); # even number
my ($az, $x, $y) = PDL::Slatec::fft($pa);
is_pdl $az, float 0;
is_pdl $x, float "[0 1 0 0]";
is_pdl $y, float "[0 0 0 0]";
is_pdl PDL::Slatec::rfft($az, $x, $y), $pa;
$pa = pdl(float,1,-1,1,-1,1); # odd number
($az, $x, $y) = PDL::Slatec::fft($pa);
is_pdl $az, float 0.2;
is_pdl $x, float "[0.4 0.4 0 0 0]";
is_pdl $y, float "[-0.2906170 -1.231073 0 0 0]";
is_pdl PDL::Slatec::rfft($az, $x, $y), $pa;
}

done_testing;
