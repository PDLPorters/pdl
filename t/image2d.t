# -*-perl-*-
#

use Test::More tests => 29;
use Test::Exception;

use PDL;
use PDL::Image2D;

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

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

SKIP: {
    skip "PDL::Bad support not available.", 5 unless $PDL::Bad::Status;

    my $pa = ones(5,5);
    # patchbad2d: bad data
    my $m = $pa->slice('1:3,1:3');
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
my $ps = $px->cat($py)->xchg(0,1);
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
