use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL;
use PDL::Image2D;

my $mask = pdl(
  [[0,0,0,0,0],[0,0,1,1,0],[0,0,0,0,0],[0,0,1,1,0],[0,0,0,0,0]],
  [[0,0,0,0,0],[0,1,0,1,0],[0,0,1,1,0],[0,0,0,0,0],[0,0,0,0,0]],
);
my $crops = pdl(indx,
  [2,3,1,3],
  [1,3,1,2],
);
my $got = crop($mask->slice(':,:,(1)'));
ok all($got == $crops->slice(':,(1)')), 'mask non-broadcast' or diag "got=$got";
$got = crop($mask);
ok all($got == $crops), 'mask broadcast' or diag "got=$got";

my $ans = pdl(
 [ 3,  7, 11, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  5, 13, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

my $x = xvals zeroes 10,3;
$x->setbadat(2,1);

my $y = pdl [1,2],[2,1];
my $c = conv2d($x, $y);

is( int(at(sum($c-$ans))), 0, "conv2d()" );

$x = zeroes(5,5);
$x->badflag(1);
my $t = $x->slice("1:3,1:3");
$t .= ones(3,3);
$x->setbadat(2,2);

$y = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,2,0,0],[0,1,5,2,0],[0,0,4,0,0],[0,0,0,0,0]);
my $m = med2d($x,$y);
my $m_sub = $m-$ans;
my $m_sum = sum($m_sub);
my $m_at = at($m_sum);
my $m_int = int($m_at);
is( $m_int, 0, "med2d()" ) or diag "x: $x\nm: $m\nans: $ans\nm_sub: $m_sub\nm_sum: $m_sum\nm_at: $m_at\nm_int: $m_int";

{
# Right answer

my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

# conv2d
my $pa = xvals zeroes 10,3;
my $pb = pdl [1,2],[2,1];
my $pc = conv2d ($pa, $pb);
ok all( approx( $pc, $ans)), "conv2d xvals";  # 1
}

{
# conv2d
my $pa = zeroes(3,3);
$pa->set(1,1,1);
my $pb = sequence(3,3);
ok all( approx( conv2d($pa,$pb), $pb )), "conv2d trivial kernel";    # 2
}

{
# conv2d: boundary => reflect
my $pa = ones(3,3);
my $pb = sequence(3,3);
{
my $ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
ok all( approx( conv2d($pb,$pa,{Boundary => 'Reflect'}), $ans )), "conv2d reflect";  #3
}

{
# conv2d: boundary => replicate
my $ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
ok all( approx( conv2d($pb,$pa,{Boundary => 'Replicate'}), $ans )), "conv2d replicate" ; #4
}

{
# conv2d: boundary => truncate
my $ans = pdl ([8,15,12],[21,36,27],[20,33,24]);
ok all( approx( conv2d($pb,$pa,{Boundary => 'Truncate'}), $ans )), "conv2d truncate"; #5
}
}

{
  my $realmask=ones(3,3)/9;
  my $realseq=sequence(3,3);
  my $imagmask=$realmask*i();
  my $imagseq=$realseq*i();
  my $real_exp = <<EOF;
\n[
 [4 4 4]
 [4 4 4]
 [4 4 4]
]
EOF
  my $neg_exp = <<EOF;
\n[
 [-4 -4 -4]
 [-4 -4 -4]
 [-4 -4 -4]
]
EOF
  my $imag_exp = <<EOF;
\n[
 [4i 4i 4i]
 [4i 4i 4i]
 [4i 4i 4i]
]
EOF
  is $realseq->conv2d($realmask).'', $real_exp, "Real matrix real mask";
  is $imagseq->conv2d($realmask).'', $imag_exp, "Imag matrix real mask";
  is $realseq->conv2d($imagmask).'', $imag_exp, "Real matrix imag mask";
  is $imagseq->conv2d($imagmask).'', $neg_exp, "Imag matrix imag mask";
}

{
# max2d_ind
my $pa = 100 / (1.0 + rvals(5,5));
$pa = $pa * ( $pa < 90 );
my @ans = $pa->max2d_ind();
note "max2d_ind: " . join( ',', @ans ) . "\n";
ok( ($ans[0] == 50) & ($ans[1] == 1) & ($ans[2] == 2), "max2d_ind" );
}

{
# centroid2d
my $pa = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
$pa = $pa * ( $pa >= 9 );
my @ans = $pa->centroid2d( 10, 10, 20 );
ok all( approx( $ans[0], 8.432946)), "centroid2d (0)";  # numbers calculated by an independent program
ok all( approx( $ans[1], 11.756724)), "centroid2d (1)";
}

{
# med2d
my $pa = zeroes(5,5);
my $t = $pa->slice("1:3,1:3");
$t .= ones(3,3);
my $pb = sequence(3,3);
my $ans = pdl ( [0,0,0,0,0],[0,0,1,0,0],[0,1,4,2,0],[0,0,4,0,0],[0,0,0,0,0]);
ok all( approx( med2d($pa,$pb), $ans )), "med2d";

{
# med2df
my $pa = sequence(10,10);
ok all( approx( med2df($pa,3,3,{Boundary=>'Truncate'})->slice("1:-2,1:-2"), $pa->slice("1:-2,1:-2"))), "med2df";
}
}

{
# patch2d
my $pa = ones(5,5);
my $mask = zeroes(5,5);
$mask->set(2,2,1);
ok all( approx( patch2d($pa,$mask), $pa )), "patch2d 1-element no-op";  # 6

# note:
#   with no bad values, any bad pixel which has no good neighbours
#   has its value copied
#
my $m = $mask->slice('1:3,1:3');
$m .= 1;
my $pans = $pa->copy;
note $pa, $mask, patch2d($pa,$mask);
ok all( approx( patch2d($pa,$mask), $pans)), "patch2d 2d slice no-op";  # 7

$pa = ones(5,5);
# patchbad2d: bad data
$m = $pa->slice('1:3,1:3');
$m .= $pa->badvalue;
$pa->badflag(1);        # should sort out propagation of badflag

my $ans = ones(5,5);
$ans->set(2,2,$ans->badvalue);
$ans->badflag(1);

#note $pa, patchbad2d($pa);
ok all( approx( patchbad2d($pa), $ans )), "patchbad2d";  # 8

# patchbad2d: good data
$pa = sequence(5,5);
#note $pa, patchbad2d($pa);
ok all( approx( patchbad2d($pa), $pa )), "patchbad2d good data";  # 9

# max2d_ind
$pa = 100 / (1.0 + rvals(5,5));
$pa = $pa->setbadif( $pa > 90 );
my @ans = $pa->max2d_ind();
note "max2d_ind: " . join( ',', @ans );
ok( ($ans[0] == 50) & ($ans[1] == 1) & ($ans[2] == 2), "max2d_ind bad data");;


# centroid2d
# centroid2d
$pa = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
$pa = $pa->setbadif( $pa < 9 );
@ans = $pa->centroid2d( 10, 10, 20 );
ok all( approx( $ans[0], 8.432946)), "centroid2d bad data (0)";  # numbers should be same as when set < 9 to 0
ok all( approx( $ans[1], 11.756724)), "centroid2d bad data (1)";

}

{
# box2d bug test
my $one = random(10,10);
my $box = cat $one,$one,$one;

my $bav = $one->box2d(3,3,0);
my $boxav = $box->box2d(3,3,0);

# all 2D averages should be the same
ok all( approx($bav->sum,$boxav->clump(2)->sumover)), "box2d";
}

{
# cc8compt & cc4compt
{
my $pa = pdl([0,1,1,0,1],[1,0,1,0,0],[0,0,0,1,0],[1,0,0,0,0],[0,1,0,1,1]);
is(cc8compt($pa)->max, 4, 'cc8compt');
is(cc4compt($pa)->max, 7, 'cc4compt');
dies_ok { ccNcompt($pa,5); } "ccNcompt(5) fails";
lives_ok { ccNcompt($pa,8) } "ccNcompt(8) succeeds";
}

{
my $im = (xvals(25,25)+yvals(25,25));
my $seg_b = cc4compt(byte $im%2);
ok($seg_b->type >= long, "cc4compt type>=long");
ok(cc4compt($im%2)->max == $seg_b->max, "cc4compt results don't wrap for byte type");
}

}

{
# pnpoly
my $px = pdl(0,3,1);
my $py = pdl(0,1,4);
my $im = zeros(5,5);
my $im2 = zeroes(5,5);
my $x = $im->xvals;
my $y = $im->yvals;
my $ps = $px->cat($py)->transpose;
my $im_mask = pnpoly($x,$y,$px,$py);
ok(sum($im_mask) == 5, "pnpoly old style, 5 pixels inside");
my $inpixels = pdl q[ 1 1 ; 1 2 ; 1 3 ; 2 1 ; 2 2 ];
ok(sum($inpixels - qsortvec(scalar whichND($im_mask))) == 0, "pnpoly old style, correct pixels inside");

# Make sure the PDL pnpoly and the PP pnpoly give the same result
ok(all($im_mask == $im->pnpoly($ps)), "pnpoly old style vs new style");

# Trivial test to make sure the polyfills using the pnpoly algorithm are working
$im .= 0;
polyfillv($im2,$ps,{'Method'=>'pnpoly'}) .= 22;
ok(all(polyfill($im,$ps,22,{'Method'=>'pnpoly'}) == $im2), "polyfill using pnpoly algorithm");


# Trivial test to make sure the polyfills are working
$im .= 0;
$im2 .= 0;
polyfillv($im2,$ps) .= 25;
polyfill($im,$ps,25);
ok(all($im == $im2), "polyfill using default algorithm");
}

#######################
#warp2d and friends
{#this just runs the example in the documentation

  my $x = pdl( 0,   0, 100, 100 );
  my $y = pdl( 0, 100, 100,   0 );
  # get warped to these positions
  my $u = pdl( 10, 10, 90, 90 );
  my $v = pdl( 10, 90, 90, 10 );
  #
  # shift of origin + scale x/y axis only
  my $fit = byte( [ [1,1], [0,0] ], [ [1,0], [1,0] ] );
  my ( $px, $py ) = fitwarp2d( $x, $y, $u, $v, 2, { FIT => $fit } );
  ok(all(approx($px,pdl([-12.5,1.25],[0,0]),1e-13)),'px fitwarp2d linear restricted');
  ok(all(approx($py,pdl([-12.5,0],[1.25,0]))),'py fitwarp2d linear restricted');
  # Compared to allowing all 4 terms
  ( $px, $py ) = fitwarp2d( $x, $y, $u, $v, 2 );
  ok(all(approx($px, pdl([-12.5,1.25],[0,0]))),'px fitwarp2d linear unrestricted');
  ok(all(approx($py, pdl([-12.5,0],[1.25,0]))),'py fitwarp2d linear unrestricted');
  # A higher-degree polynomial should not affect the answer much, but
  # will require more control points
  $x = $x->glue(0,pdl(50,12.5, 37.5, 12.5, 37.5));
  $y = $y->glue(0,pdl(50,12.5, 37.5, 37.5, 12.5));
  $u = $u->glue(0,pdl(73,20,40,20,40));
  $v = $v->glue(0,pdl(29,20,40,40,20));
  my ( $px3, $py3 ) = fitwarp2d( $x, $y, $u, $v, 3 );
  my ($x3,$y3) = applywarp2d($px3,$py3,$u,$v);
  ok(all(approx($x3,$x,1e-4)),'px fitwarp2d quadratic unrestricted');
  ok(all(approx($y3,$y,1e-4)),'py fitwarp2d quadratic unrestricted');

#define a simple grid image
my $img = (xvals(50,50) % 8 == 5 ) * (yvals(50,50) % 9 == 6); #stretch the y control points out a bit, and offset them too.
#get the control points
  ($u,$v) = whichND($img)->mv(0,-1)->double->dog;
#and shift it by 1 in horizontal and vertical directions
my $shift = $img->range([1,1],[$img->dims],'p');
#get the control points of the shifted image
  ($x,$y) = whichND($shift)->mv(0,-1)->double->dog;

  use PDL::NiceSlice;
  # we try 1st-, 2nd-, and 3rd-order fits, with and without restrictions to shift-and-scale-only
  foreach my $deg(2,3,4){
      my $fit = zeroes(byte,$deg,$deg,2);
      $fit(:,(0),(0)).=1;
      $fit((0),:,(1)).=1;
      foreach my $restrict_fit(1,0){
	  my ($pxn,$pyn) = fitwarp2d($x,$y,$u,$v,$deg,$restrict_fit?{FIT=>$fit}:{});
	  my $out = warp2d($shift,$pxn,$pyn);
	  ok(all(approx($out,$img,1e-3)),'warp2d ' . ($restrict_fit?'':'un') . "restricted deg $deg values approx") or diag "got=$out\nexpected=$img";
	  ok(all($out->rint==$img),'warp2d ' . ($restrict_fit?'':'un') . "restricted deg $deg rint exact");
      }
  }

}

done_testing;
