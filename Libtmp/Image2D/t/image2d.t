use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL;
use PDL::Image2D;
use PDL::FFT;
use Test::PDL;

# compare fft convolutions with direct method
{
my $pa = rfits("../../Basic/m51.fits");
{
my $pk = ones(5,5);
my $pb = conv2d($pa,$pk);
my $kk = kernctr($pa,$pk);
fftconvolve( my $pi=$pa->copy, $kk );
is_pdl $kk, $pa->zeroes->double, "kernctr";
is_pdl $pi, $pb, "fftconvolve";
}
{
my $pk = pdl[
 [ 0.51385498,  0.17572021,  0.30862427],
 [ 0.53451538,  0.94760132,  0.17172241],
 [ 0.70220947,  0.22640991,  0.49475098],
 [ 0.12469482, 0.083892822,  0.38961792],
 [ 0.27722168,  0.36804199,  0.98342896],
 [ 0.53536987,  0.76565552,  0.64645386],
 [ 0.76712036,   0.7802124,  0.82293701]
];
my $pb = conv2d($pa,$pk);
my $kk = kernctr($pa,$pk);
fftconvolve( my $pi=$pa->copy, $kk );
is_pdl $kk, $pa->zeroes->double, "kernctr weird kernel";
is_pdl $pi, $pb, "fftconvolve weird kernel";
}
}

my $mask = pdl(
  [[0,0,0,0,0],[0,0,1,1,0],[0,0,0,0,0],[0,0,1,1,0],[0,0,0,0,0]],
  [[0,0,0,0,0],[0,1,0,1,0],[0,0,1,1,0],[0,0,0,0,0],[0,0,0,0,0]],
);
my $crops = pdl(indx,
  [2,3,1,3],
  [1,3,1,2],
);
is_pdl crop($mask->slice(',,(1)')), $crops->slice(',(1)'), 'mask non-broadcast';
is_pdl crop($mask), $crops, 'mask broadcast';

my $ans = pdl(
 [ 3,  7, 11, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  5, 13, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);
my $x = xvals zeroes 10,3;
$x->setbadat(2,1);
is_pdl conv2d($x, pdl([1,2],[2,1])), $ans, "conv2d()";

$x = pdl '0 0 0 0 0; 0 1 1 1 0; 0 1 BAD 1 0; 0 1 1 1 0; 0 0 0 0 0';
$ans = pdl '0 0 0 0 0; 0 0 2 0 0; 0 1 5 2 0; 0 0 4 0 0; 0 0 0 0 0';
is_pdl med2d($x, sequence(3,3)), $ans, "med2d()";

{
my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);
is_pdl conv2d(xvals(10,3), pdl([1,2],[2,1])), $ans, "conv2d xvals";
}

{
my $pb = sequence(3,3);
is_pdl conv2d(pdl('0 0 0; 0 1 0; 0 0 0'),$pb), $pb, "conv2d trivial kernel";
}

{
my $pa = ones(3,3);
my $pb = sequence(3,3);
is_pdl conv2d($pb,$pa,{Boundary => 'Reflect'}), pdl('12 18 24; 30 36 42; 48 54 60'), "conv2d reflect";
is_pdl conv2d($pb,$pa,{Boundary => 'Replicate'}), pdl('12 18 24; 30 36 42; 48 54 60'), "conv2d replicate";
is_pdl conv2d($pb,$pa,{Boundary => 'Truncate'}), pdl('8 15 12; 21 36 27; 20 33 24'), "conv2d truncate";
}

{
  my $ones = ones(3,3);
  my $realmask=$ones/9;
  my $realseq=sequence(3,3);
  my $imagmask=$realmask*i();
  my $imagseq=$realseq*i();
  my $imag_exp = $ones*4*i();
  is_pdl $realseq->conv2d($realmask), $ones*4, "Real matrix real mask";
  is_pdl $imagseq->conv2d($realmask), $imag_exp, "Imag matrix real mask";
  is_pdl $realseq->conv2d($imagmask), $imag_exp, "Real matrix imag mask";
  is_pdl $imagseq->conv2d($imagmask), cdouble($ones*-4), "Imag matrix imag mask";
}

{
# max2d_ind
my $pa = 100 / (1.0 + rvals(5,5));
$pa = $pa * ( $pa < 90 );
my @ans = $pa->max2d_ind();
is_deeply \@ans, [50,1,2], "max2d_ind" or diag explain \@ans;
}

{
# centroid2d
my $pa = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
$pa = $pa * ( $pa >= 9 );
my @ans = $pa->centroid2d( 10, 10, 20 );
is_pdl $ans[0], pdl(8.432946), "centroid2d (0)"; # numbers calculated by an independent program
is_pdl $ans[1], pdl(11.756724), "centroid2d (1)";
}

{
# med2d
my $pa = zeroes(5,5);
my $t = $pa->slice("1:3,1:3");
$t .= ones(3,3);
my $pb = sequence(3,3);
my $ans = pdl ( [0,0,0,0,0],[0,0,1,0,0],[0,1,4,2,0],[0,0,4,0,0],[0,0,0,0,0]);
is_pdl med2d($pa,$pb), $ans, "med2d";

{
# med2df
my $pa = sequence(10,10);
is_pdl med2df($pa,3,3,{Boundary=>'Truncate'})->slice("1:-2,1:-2"), $pa->slice("1:-2,1:-2"), "med2df";
}
}

{
# patch2d
my $pa = ones(5,5);
my $mask = zeroes(5,5);
$mask->set(2,2,1);
is_pdl patch2d($pa,$mask), $pa, "patch2d 1-element no-op";

# note:
#   with no bad values, any bad pixel which has no good neighbours
#   has its value copied
#
$mask->slice('1:3,1:3') .= 1;
my $pans = $pa->copy;
note $pa, $mask, patch2d($pa,$mask);
is_pdl patch2d($pa,$mask), $pans, "patch2d 2d slice no-op";

$pa = ones(5,5);
# patchbad2d: bad data
$pa->slice('1:3,1:3') .= $pa->badvalue;
$pa->badflag(1);        # should sort out propagation of badflag

my $ans = ones(5,5);
$ans->set(2,2,$ans->badvalue);
$ans->badflag(1);

is_pdl patchbad2d($pa), $ans, "patchbad2d";

# patchbad2d: good data
$pa = sequence(5,5);
is_pdl patchbad2d($pa), $pa, "patchbad2d good data";

# max2d_ind
$pa = 100 / (1.0 + rvals(5,5));
$pa = $pa->setbadif( $pa > 90 );
my @ans = $pa->max2d_ind();
is_deeply \@ans, [50,1,2], "max2d_ind bad data" or diag explain \@ans;

# centroid2d
$pa = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
$pa = $pa->setbadif( $pa < 9 );
@ans = $pa->centroid2d( 10, 10, 20 );
is_pdl $ans[0], pdl(8.432946), "centroid2d bad data (0)";  # numbers should be same as when set < 9 to 0
is_pdl $ans[1], pdl(11.756724), "centroid2d bad data (1)";
}

{
# box2d bug test
my $one = random(10,10);
my $box = cat $one,$one,$one;
my $bav = $one->box2d(3,3,0);
my $boxav = $box->box2d(3,3,0);
# all 2D averages should be the same
is_pdl $bav->sum->slice('*3'),$boxav->clump(2)->sumover, "box2d";
}

{
my $pa = pdl([0,1,1,0,1],[1,0,1,0,0],[0,0,0,1,0],[1,0,0,0,0],[0,1,0,1,1]);
is(cc8compt($pa)->max, 4, 'cc8compt');
is(cc4compt($pa)->max, 7, 'cc4compt');
dies_ok { ccNcompt($pa,5); } "ccNcompt(5) fails";
lives_ok { ccNcompt($pa,8) } "ccNcompt(8) succeeds";
}

{
my $im = (xvals(25,25)+yvals(25,25)) % 2;
my $seg_b = cc4compt(byte $im);
ok($seg_b->type >= long, "cc4compt type>=long");
is_pdl $seg_b, cc4compt($im)->long, "cc4compt results don't wrap for byte type";
}

{
# pnpoly
my $px = pdl(0,3,1);
my $py = pdl(0,1,4);
my $im2 = (my $im = zeroes(5,5))->copy;
my $im_mask = pnpoly($im->xvals,$im->yvals,$px,$py);
my $inpixels = indx q[ 1 1 ; 1 2 ; 1 3 ; 2 1 ; 2 2 ];
is_pdl whichND($im_mask)->qsortvec, $inpixels, "pnpoly old style, correct pixels inside";

# Make sure the PDL pnpoly and the PP pnpoly give the same result
my $ps = $px->cat($py)->transpose;
is_pdl $im_mask, $im->pnpoly($ps)->longlong, "pnpoly old style vs new style";

# Trivial test to make sure the polyfills using the pnpoly algorithm are working
$im .= 0;
polyfillv($im2,$ps,{'Method'=>'pnpoly'}) .= 22;
is_pdl polyfill($im,$ps,22,{'Method'=>'pnpoly'}), $im2, "polyfill using pnpoly algorithm";

# Trivial test to make sure the polyfills are working
$im .= 0;
$im2 .= 0;
polyfillv($im2,$ps) .= 25;
polyfill($im,$ps,25);
is_pdl $im, $im2, "polyfill using default algorithm";
}

#######################
#warp2d and friends
{#this just runs the example in the documentation
  my $x = pdl( 0,   0, 100, 100 );
  my $y = pdl( 0, 100, 100,   0 );
  # get warped to these positions
  my $u = pdl( 10, 10, 90, 90 );
  my $v = pdl( 10, 90, 90, 10 );
  # shift of origin + scale x/y axis only
  my $fit = byte( [ [1,1], [0,0] ], [ [1,0], [1,0] ] );
  my ( $px, $py ) = fitwarp2d( $x, $y, $u, $v, 2, { FIT => $fit } );
  is_pdl $px,pdl([-12.5,1.25],[0,0]),'px fitwarp2d linear restricted';
  is_pdl $py,pdl([-12.5,0],[1.25,0]),'py fitwarp2d linear restricted';
  # Compared to allowing all 4 terms
  ( $px, $py ) = fitwarp2d( $x, $y, $u, $v, 2 );
  is_pdl $px, pdl([-12.5,1.25],[0,0]),'px fitwarp2d linear unrestricted';
  is_pdl $py, pdl([-12.5,0],[1.25,0]),'py fitwarp2d linear unrestricted';
  # A higher-degree polynomial should not affect the answer much, but
  # will require more control points
  $x = $x->glue(0,pdl(50,12.5, 37.5, 12.5, 37.5));
  $y = $y->glue(0,pdl(50,12.5, 37.5, 37.5, 12.5));
  $u = $u->glue(0,pdl(73,20,40,20,40));
  $v = $v->glue(0,pdl(29,20,40,40,20));
  my ( $px3, $py3 ) = fitwarp2d( $x, $y, $u, $v, 3 );
  my ($x3,$y3) = applywarp2d($px3,$py3,$u,$v);
  is_pdl $x3,$x, {atol=>1e-4, test_name=>'px fitwarp2d quadratic unrestricted'};
  is_pdl $y3,$y, {atol=>1e-4, test_name=>'py fitwarp2d quadratic unrestricted'};

  #define a simple grid image
  my $img = (xvals(50,50) % 8 == 5 ) * (yvals(50,50) % 9 == 6); #stretch the y control points out a bit, and offset them too.
  ($u,$v) = whichND($img)->double->using(0,1); # get the control points
  my $shift = $img->range([1,1],[$img->dims],'p'); # shift 1 horiz and vert
  #get the control points of the shifted image
  ($x,$y) = whichND($shift)->mv(0,-1)->double->dog;
  # fits of order 1,2,3, with/without restriction to shift-and-scale-only
  foreach my $deg (2,3,4) {
    my $fit = zeroes(byte,$deg,$deg,2);
    $fit->slice(':,(0),(0)').=1;
    $fit->slice('(0),:,(1)').=1;
    foreach my $unrestrict ('un', '') {
      my ($pxn,$pyn) = fitwarp2d($x,$y,$u,$v,$deg,$unrestrict?{}:{FIT=>$fit});
      my $out = warp2d($shift,$pxn,$pyn);
      is_pdl $out,$img, {atol=>1e-3, test_name=>"warp2d ${unrestrict}restricted deg $deg values"};
      is_pdl $out->rint, $img, "warp2d ${unrestrict}restricted deg $deg rint exact";
    }
  }
}

done_testing;
