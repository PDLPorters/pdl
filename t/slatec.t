# -*-perl-*-
use PDL::LiteF;
BEGIN {
	eval " use PDL::Slatec; ";
	$loaded = ($@ ? 0 : 1);
}

# Test counters [should 'use Test' but this was not a standard
# part of perl until 5.005]
my $ntests = 40;
$ntests -= 3 unless ($PDL::Config{WITH_BADVAL}); # two fewer tests if no bad val support

my $n = 0;

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	$n++; # increment test counter
	my $result = shift ;
	print "not " unless $result ;
	print "ok $n\n" ;
}

sub tapprox {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
#	print "APR: $a,$b,$c,$d;\n";
	$d < 0.001;
}

print "1..$ntests\n";
unless ($loaded) {
	#print STDERR "PDL::Slatec not installed. All tests are skipped.\n";
	for (1..$ntests) {
                print "ok $_ # Skipped: PDL::Slatec not available.\n";
	}
	exit;
}

my $mat = pdl [1,0.1],[0.1,2];

($eigvals,$eigvecs) = eigsys($mat);

print $eigvecs,$eigvals,"\n";

ok(tapprox($eigvals,pdl(0.9901,2.009)));
ok(!tapprox($eigvals,pdl(0.99,2.5)));

ok(tapprox($eigvecs,pdl([0.995,-0.0985],[0.0985,0.995])));

$mat = pdl [2,3],[4,5];

$inv = matinv($mat);

inner($mat->dummy(2),$inv->xchg(0,1)->dummy(1),($uni=null));

print $mat;
print $inv;

print $uni;

ok(tapprox($uni,pdl[1,0],[0,1]));

$det = $mat->det;
$det->dump;
$deti = $inv->det;
$deti->dump;

ok(tapprox($det,-2));
ok(tapprox($deti,-0.5));

# Now do the polynomial fitting tests


if ($PDL::Config{WITH_BADVAL}) {

  # Set up tests x, y and weight
  my $y = pdl (1,4,9,16,25,36,49,64.35,32);
  my $x = pdl ( 1,2,3,4,5,6,7,8,9);
  my $w = pdl ( 1,1,1,1,1,1,1,0.5,0.3);

  # input parameters
  my $eps = pdl(0);
  my $maxdeg = 5;

  # Test with a bad value
  $y->inplace->setbadat(3);
  ($ndeg, $r, $ierr, $a) = polyfit($x, $y, $w, $maxdeg, $eps);

  print "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";
  print "poly = $r\n";

  ok(($ierr == 1));

  # Test with all bad values
  $y = zeroes(9)->setbadif(1);
  ($ndeg, $r, $ierr, $a) = polyfit($x, $y, $w, $maxdeg, $eps);

  print "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";
  print "poly = $r\n";

  ok(($ierr == 2));

  # Now test threading over a 2 by N matrix
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

  ($ndeg, $r, $ierr, $a) = polyfit($x, $y, $w, $maxdeg, $eps);

  print "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";
  print "poly = $r\n";

  ok((sum($ierr == 1) == 2));

}

# Set up tests x, y and weight
$y = pdl (1,4,9,16,25,36,49,64.35,32);
$x = pdl ( 1,2,3,4,5,6,7,8,9);
$w = pdl ( 1,1,1,1,1,1,1,0.5,0.3);
$maxdeg = 7;
$eps = pdl(0);

# Do the fit
my ($ndeg, $r, $ierr, $a) = polyfit($x, $y, $w, $maxdeg, $eps);

print "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";
print "poly = $r\n";

ok(($ierr == 1));


# Test POLYCOEF                                                               

my $c = pdl(4);           # Expand about x = 4;

my $tc = polycoef($ndeg, $c, $a);

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
  print "$xpos, $ypos, $r[$i]\n";

  # Compare with answers from polyfit
  ok(sprintf("%5.2f", $ypos) == sprintf("%5.2f", $r[$i]));
  $i++;                                                                       

}

# Try polyvalue with a single x pos
my $xx = pdl([4]);
my $nder = 3;

my ($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a);

print "At $xx, $yfit and $yp\n";
ok(int($yp->at(0)) == 8);

# Test polyvalue
$nder = 3;
$xx    = pdl(12,4,6.25,1.5); # Ask for multiple positions at once

($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a);

print "At $xx is $yfit and $yp\n";

# Simple test of expected value                                               
ok(int($yfit->at(1)) == 15);            

# test the PCHIP stuff

## Test: chim/chic
#
$x = float( 3 .. 10 );
my $f = $x*$x*$x + 425.42352;
my $answer = 3.0*$x*$x; 

my ( $d, $err ) = chim( float($x), float($f) );

ok($err->getndims==0 & $err->sum == 0);

# don't check the first and last elements, as expect the
# error to be largest there
# value of 5% comes from tests on linux and solaris machines
ok(all( slice( abs(($d - $answer)/$answer), '1:-2' ) < 0.05 ) );

# compare the results of chic
my $wk = $f->zeroes( 2 * $f->nelem );
my $d2 = $f->zeroes;
chic( pdl([0, 0]), pdl([0, 0]), 1, $x, $f, $d2, $wk, my $err2=null );
ok($err2->getndims==0 & $err2->sum == 0);
ok(all( abs($d2 - $d) < 0.02 ) );

## Test: chsp
#
chsp( pdl([0, 0]), pdl([0, 0]), $x, $f, my $d3=null, $wk, my $err3=null );
ok($err3->getndims==0 & $err3->sum == 0);
ok(all( abs($d3 - $d) < 2 ) );

## Test: chfd/chfe
#
my $xe = float( pdl( 4 .. 8 ) + 0.5 );
my ( $fe, $de );
( $fe, $de, $err ) = chfd( $x, $f, $d, 1, $xe );

ok($err->getndims==0 & $err->sum == 0);

$answer = $xe*$xe*$xe + 425.42352;
ok(all( abs(($fe - $answer)/$answer) < 1.0e-5 ) );

$answer = 3.0*$xe*$xe;
ok(all( abs(($de - $answer)/$answer) < 0.02 ) );

( $fe, $err ) = chfe( $x, $f, $d, 1, $xe );

ok($err->getndims==0 & $err->sum == 0);

$answer = $xe*$xe*$xe + 425.42352;
ok(all( abs(($fe - $answer)/$answer) < 1.0e-5 ) );

# Test: chcm
#
$x   = float( 1, 2, 3, 5, 6, 7 );
$f   = float( 1, 2, 3, 4, 3, 4 );
$ans = long(  1, 1, 1, -1, 1, 2 );

( $d, $err ) = chim($x, $f);
ok($err->getndims==0 & $err->sum == 2); # 2 switches in monotonicity

my $ismon;
( $ismon, $err ) = chcm($x, $f, $d, 1);

ok($err->getndims==0 & $err->sum == 0);
ok($ismon->get_datatype == 3);
ok(tapprox($ismon,$ans));

## Test: chia
#
$x = float( sequence(11) - 0.3 );
$f = $x * $x;
( $d, $err ) = chim($x, $f);

$ans = pdl( 9.0**3, (8.0**3-1.0**3) ) / 3.0;
( $int, $err ) = chia($x, $f, $d, 1, pdl(0.0,1.0), pdl(9.0,8.0));
ok(all($err == 0));
ok(all( abs($int-$ans) < 0.04 ) );

$hi = pdl( $x->at(9), $x->at(7) );
$lo = pdl( $x->at(0), $x->at(1) );
$ans = ($hi**3 - $lo**3) / 3;
( $int, $err ) = chid( $x, $f, $d, 1, pdl(0,1), pdl(9,7) );
ok(all($err == 0));
ok(all( abs($int-$ans) < 0.06 ) );

=pod ignore as have commented out chbs interface

## Test: chbs - note, only tests that it runs successfully
#
my $nknots = 0;
my $t = zeroes( float, 2*$x->nelem+4 );
my $bcoef  = zeroes( float, 2*$x->nelem );
my $ndim = PDL->null;
my $kord = PDL->null;
$err = PDL->null;
chbs( $x, $f, $d, 0, $nknots, $t, $bcoef, $ndim, $kord, $err );
ok(all($err == 0));

=cut

